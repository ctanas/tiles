;;; warcel.el --- Browse WARC web archives  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Claudiu

;; Author: Claudiu <claudiu@tanaselia.ro>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: hypermedia, files
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Warcel lists the records of a WARC (Web ARChive) file and shows the
;; archived pages either as Org documents or rendered by EWW.
;;
;; Scripts, styles, images, video, audio, frames and form controls are
;; removed before a page is shown, so viewing never runs code or
;; fetches anything from the network.  Links to pages that are in the
;; archive open the archived copy; other links are handled according
;; to `warcel-follow-live-links'.
;;
;; Everything is done by Emacs itself: plain and gzip-compressed WARC
;; files are read with the built-in zlib support and HTML is parsed
;; with the built-in libxml2 support.
;;
;; Usage:
;;
;;   M-x warcel-open RET archive.warc.gz RET
;;
;; An archive that holds a single page opens on that page; others open
;; on the list of their records.  Visiting a .warc file does the same.
;; In a page, ^ lists the records of its archive.
;;
;; In Dired, RET opens archives with `warcel-open' too.
;;
;; In the record list:
;;
;;   RET  show the record (as Org or EWW, see `warcel-default-view')
;;   o    show the record as an Org document
;;   e    show the record with EWW
;;   v    show the raw record (WARC and HTTP headers, then the payload)
;;   s    save the record's payload to a file
;;   a    show all record types, or only `warcel-record-types'
;;   /    only list records whose URI or content type match a regexp
;;
;; To save a web page in a new archive:
;;
;;   M-x warcel-capture RET https://example.com/ RET
;;
;; The URL in the clipboard, if any, is offered as the default.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'browse-url)
(require 'dom)
(require 'eww)
(require 'shr)
(require 'tabulated-list)
(require 'url-parse)
(require 'url-expand)
(require 'url-util)

(declare-function dired-find-file "dired" ())
(declare-function dired-get-file-for-visit "dired" ())
(declare-function dired-get-filename "dired" (&optional localp no-error-if-not-filep))
(declare-function org-link-make-string "ol" (link &optional description))
(declare-function org-open-at-point "org" (&optional arg))
(declare-function org-table-align "org-table" ())
(declare-function org-table-map-tables "org-table" (f &optional quietly))

(defconst warcel-version "0.1.0"
  "Version of warcel.")

(defvar desktop-save-buffer)
(defvar dired-mode-map)
(defvar jka-compr-really-do-compress)
(defvar org-startup-folded)
(defvar org-startup-with-inline-images)
(defvar org-startup-with-latex-preview)

;;;; Customization

(defgroup warcel nil
  "Browse WARC web archives."
  :group 'hypermedia
  :prefix "warcel-")

(defcustom warcel-default-view 'org
  "How \\<warcel-mode-map>\\[warcel-open-record] shows an archived HTML page."
  :type '(choice (const :tag "Org document" org)
                 (const :tag "EWW" eww)))

(defcustom warcel-readable t
  "Non-nil means the Org view shows only the title and main content of pages.
The menus, headers, footers, sidebars, sharing buttons, lists of
related pages and comments around an article are then left out.
\\<warcel-view-mode-map>\\[warcel-toggle-readable] switches a page between that and the whole page."
  :type 'boolean)

(defcustom warcel-record-types '("response" "resource" "revisit" "conversion")
  "WARC record types listed by default.
\\<warcel-mode-map>\\[warcel-toggle-all-records] toggles between these and all records."
  :type '(repeat string))

(defcustom warcel-follow-live-links 'ask
  "What to do when following a link to a page that is not in the archive.
When `ask', ask before opening the live page with `browse-url'.
When t, open it without asking.  When nil, only report it."
  :type '(choice (const :tag "Ask first" ask)
                 (const :tag "Open with `browse-url'" t)
                 (const :tag "Do not open" nil)))

(defcustom warcel-capture-directory nil
  "Directory where \\[warcel-capture] saves new archives.
When nil, save them in the `default-directory' of the current buffer."
  :type '(choice (const :tag "Current directory" nil)
                 directory))

(defcustom warcel-capture-compress t
  "Non-nil means \\[warcel-capture] saves new archives compressed.
They are then .warc.gz files, with each record compressed on its own
as other tools do.  Otherwise they are .warc files."
  :type 'boolean)

(defcustom warcel-capture-timeout 30
  "Seconds to wait for a server that stopped sending data."
  :type 'natnum)

(defcustom warcel-capture-user-agent
  (format "Mozilla/5.0 (compatible; warcel/%s; GNU Emacs %s)"
          warcel-version emacs-version)
  "User-Agent header sent when capturing pages."
  :type 'string)

;;;; Reading bytes

(defconst warcel--chunk-size (* 4 1024 1024)
  "Number of bytes read from a WARC file at a time.")

(defconst warcel--head-size 65536
  "Number of bytes examined to parse the head of a record.")

(defconst warcel--gzip-peek-size 16384
  "Number of compressed bytes inflated to read the head of a gzip member.")

(cl-defstruct (warcel--reader (:constructor warcel--reader-create)
                              (:copier nil))
  "Random access to bytes of a file, or of a whole buffer.
BUFFER holds the bytes from offset BEG up to offset END."
  file size buffer (beg 0) (end 0))

(defvar warcel--scratch nil
  "Unibyte buffer used to inflate the heads of gzip members.")

(defvar warcel--progress nil
  "Progress reporter of the WARC file being read.")

(defun warcel--unibyte-buffer (name)
  "Return a new unibyte buffer named after NAME."
  (let ((buffer (generate-new-buffer name t)))
    (with-current-buffer buffer
      (set-buffer-multibyte nil)
      (setq buffer-undo-list t))
    buffer))

(defun warcel--file-reader (file)
  "Return a reader over the bytes of FILE."
  (warcel--reader-create :file file
                         :size (file-attribute-size (file-attributes file))
                         :buffer (warcel--unibyte-buffer " *warcel-reader*")))

(defun warcel--buffer-reader (buffer)
  "Return a reader over the contents of the unibyte BUFFER."
  (let ((size (buffer-size buffer)))
    (warcel--reader-create :buffer buffer :size size :end size)))

(defun warcel--reader-at (reader offset length)
  "Make READER hold the LENGTH bytes starting at OFFSET.
Fewer bytes are held near the end of the data.  Return the position
of OFFSET in the reader's buffer."
  (let ((want (min (warcel--reader-size reader) (+ offset length))))
    (unless (and (<= (warcel--reader-beg reader) offset)
                 (<= want (warcel--reader-end reader)))
      (let ((end (min (warcel--reader-size reader)
                      (+ offset (max length warcel--chunk-size)))))
        (with-current-buffer (warcel--reader-buffer reader)
          (erase-buffer)
          (insert-file-contents-literally (warcel--reader-file reader)
                                          nil offset end))
        (setf (warcel--reader-beg reader) offset
              (warcel--reader-end reader) end)))
    (- offset (warcel--reader-beg reader) -1)))

(defun warcel--reader-u32 (reader offset)
  "Return the little-endian 32-bit number at OFFSET in READER, or nil."
  (when (and (>= offset 0) (<= (+ offset 4) (warcel--reader-size reader)))
    (let ((pos (warcel--reader-at reader offset 4)))
      (with-current-buffer (warcel--reader-buffer reader)
        (logior (char-after pos)
                (ash (char-after (+ pos 1)) 8)
                (ash (char-after (+ pos 2)) 16)
                (ash (char-after (+ pos 3)) 24))))))

(defun warcel--progress (offset)
  "Report that reading the WARC file reached OFFSET."
  (when warcel--progress
    (progress-reporter-update warcel--progress offset)))

;;;; Gzip members

;; A compressed WARC file is a series of gzip members, usually one per
;; record.  `zlib-decompress-region' stops at the end of the first
;; member without telling where it ended, so members are found by
;; looking for gzip headers that are followed by valid gzip data.

(defun warcel--gzip-candidate (reader from)
  "Return the offset of the first plausible gzip header at or after FROM.
READER holds the compressed data."
  (let ((size (warcel--reader-size reader))
        (magic (unibyte-string #x1f #x8b #x08))
        (case-fold-search nil)
        found)
    (while (and (not found) (<= (+ from 10) size))
      (let* ((pos (warcel--reader-at reader from 10))
             (base (- from pos))
             (end (warcel--reader-end reader)))
        (with-current-buffer (warcel--reader-buffer reader)
          (goto-char pos)
          (while (and (not found) (search-forward magic nil t))
            (let ((start (match-beginning 0)))
              ;; The header is 10 bytes long and its flag byte has
              ;; three reserved bits that must be zero.
              (when (and (<= (+ base start 10) end)
                         (zerop (logand (char-after (+ start 3)) #xe0)))
                (setq found (+ base start))))))
        (unless found
          (setq from (if (>= end size) size (max (1+ from) (- end 9)))))))
    found))

(defun warcel--warc-start (data)
  "Return where the WARC record at the start of DATA begins, or nil.
Newlines before the record are skipped."
  (let ((case-fold-search nil))
    (and (string-match "\\`[\r\n]*WARC/" data)
         (- (match-end 0) 5))))

(defun warcel--gunzip-peek (reader offset)
  "Inflate the start of the gzip member at OFFSET in READER.
Return (DATA . STATUS).  STATUS is t if the whole member was
inflated, 0 if the member goes on after DATA, and nil if OFFSET
does not start a gzip member."
  (let* ((pos (warcel--reader-at reader offset warcel--gzip-peek-size))
         (source (warcel--reader-buffer reader))
         (end (min (+ pos warcel--gzip-peek-size)
                   (with-current-buffer source (point-max))))
         (full (= (- end pos) warcel--gzip-peek-size)))
    (unless (buffer-live-p warcel--scratch)
      (setq warcel--scratch (warcel--unibyte-buffer " *warcel-scratch*")))
    (with-current-buffer warcel--scratch
      (erase-buffer)
      (insert-buffer-substring source pos end)
      (let ((status (condition-case nil
                        (zlib-decompress-region (point-min) (point-max) t)
                      (error nil)))
            (data (buffer-string)))
        (cons data
              (cond
               ((string-empty-p data) nil)
               ((eq status t) t)
               ;; Invalid data makes inflating stop before the end of
               ;; the input; only the end of the file can cut short a
               ;; member that is not followed by more data.
               ((and (eql status 0) (or full (warcel--warc-start data))) 0)))))))

(defun warcel--inflate-member (file beg end buffer)
  "Append the inflated gzip member of FILE between BEG and END to BUFFER.
Return non-nil if the member was inflated completely."
  (with-current-buffer buffer
    (goto-char (point-max))
    (let ((start (point)))
      (insert-file-contents-literally file nil beg end)
      (condition-case nil
          (eq t (zlib-decompress-region start (point-max) t))
        (error (delete-region start (point-max))
               nil)))))

(defun warcel--gzip-member-end (reader offset expected)
  "Find where the gzip member starting at OFFSET in READER ends.
EXPECTED is the inflated size of the member if it holds just the
record at its start, or nil.  Return a list (END NEXT ALIGNED DATA).
END is the offset where the member ends and NEXT the inflated start
of the member there, nil at the end of the file.  ALIGNED is non-nil
if the inflated size of the member is EXPECTED.  DATA is nil or a
buffer holding the inflated member, for the caller to kill."
  (let ((size (warcel--reader-size reader))
        (expected (and expected (logand expected #xffffffff)))
        (from (+ offset 20))
        result)
    (while (not result)
      (let ((candidate (warcel--gzip-candidate reader from)))
        (if (not candidate)
            (setq result (list size nil
                               (and expected
                                    (eql expected
                                         (warcel--reader-u32 reader (- size 4))))
                               nil))
          (let ((peek (warcel--gunzip-peek reader candidate))
                ;; A gzip member ends with the size of its inflated data.
                (isize (warcel--reader-u32 reader (- candidate 4))))
            (cond
             ((not (cdr peek)))
             ((and expected (eql isize expected))
              (setq result (list candidate (car peek) t nil)))
             (t
              ;; Compressed files stored in the archive look like
              ;; members too: only inflating up to the candidate tells
              ;; whether the member really ends there.
              (let ((data (warcel--unibyte-buffer " *warcel-member*")))
                (if (and (warcel--inflate-member (warcel--reader-file reader)
                                                 offset candidate data)
                         (eql isize (logand (buffer-size data) #xffffffff)))
                    (setq result (list candidate (car peek) nil data))
                  (kill-buffer data)))))
            (setq from (1+ candidate))))))
    result))

(defun warcel--append-member (buffer file beg end data)
  "Append the inflated gzip member of FILE between BEG and END to BUFFER.
DATA is nil or a buffer already holding the inflated member; it is killed."
  (if (not data)
      (warcel--inflate-member file beg end buffer)
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert-buffer-substring data))
    (kill-buffer data)))

;;;; Records

(cl-defstruct (warcel-record (:constructor warcel-record--create)
                             (:copier nil))
  "A record of a WARC file.
MEMBER is the (BEG . END) byte range of the gzip member holding the
record, a list of such ranges when the record spans several members,
or nil for an uncompressed file.  OFFSET is where the record
starts in the member's inflated data, or in the file.  HEADER-LEN is
the size of the WARC head and LENGTH the size of the block after it.
For HTTP messages, HTTP-STATUS, HTTP-HEADERS and HTTP-HEADER-LEN
describe the message head at the start of the block."
  index member offset header-len length headers
  http-status http-headers http-header-len)

(cl-defstruct (warcel-archive (:constructor warcel-archive--create)
                              (:copier nil))
  "The records of a WARC file and indexes over them."
  file records by-uri by-id by-digest)

(defun warcel--field (fields name)
  "Return the value of the header field NAME in the alist FIELDS."
  (cdr (assoc-string name fields t)))

(defun warcel--parse-fields (lines)
  "Parse the header field LINES into an alist of (NAME . VALUE)."
  (let (fields)
    (dolist (line lines)
      (cond
       ((and fields (string-match-p "\\`[ \t]" line))
        (setcdr (car fields) (concat (cdar fields) " " (string-trim line))))
       ((string-match "\\`\\([^:]+\\):[ \t]*\\(.*?\\)[ \t\r]*\\'" line)
        (push (cons (match-string 1 line)
                    (decode-coding-string (match-string 2 line) 'utf-8))
              fields))))
    (nreverse fields)))

(defun warcel--http-block-p (headers)
  "Return non-nil if a record with WARC HEADERS holds an HTTP response."
  (and (member (downcase (or (warcel--field headers "WARC-Type") ""))
               '("response" "revisit"))
       (string-prefix-p "application/http"
                        (downcase (or (warcel--field headers "Content-Type")
                                      "")))))

(defun warcel--parse-head (pos limit)
  "Parse the head of the WARC record at POS, looking no further than LIMIT.
Return a plist with :headers, :header-len and :length, plus
:http-status, :http-headers and :http-header-len for HTTP responses.
Return nil if there is no complete record head before LIMIT."
  (goto-char pos)
  (let ((case-fold-search nil))
    (when (and (looking-at "WARC/[0-9.]+\r?\n")
               (re-search-forward "\n\r?\n" limit t))
      (let* ((fields-beg (min (save-excursion (goto-char pos)
                                              (line-beginning-position 2))
                              (match-beginning 0)))
             (headers (warcel--parse-fields
                       (split-string (buffer-substring-no-properties
                                      fields-beg (match-beginning 0))
                                     "\r?\n" t)))
             (block (point))
             (length (max 0 (string-to-number
                             (or (warcel--field headers "Content-Length") "0"))))
             (head (list :headers headers :header-len (- block pos)
                         :length length)))
        (when (and (warcel--http-block-p headers)
                   (looking-at "HTTP/[0-9.]+[ \t]+\\([0-9][0-9][0-9]\\)"))
          (let ((status (string-to-number (match-string 1)))
                (status-end (line-beginning-position 2)))
            (setq head (plist-put head :http-status status))
            (when (re-search-forward "\n\r?\n" (min limit (+ block length)) t)
              (setq head (plist-put head :http-header-len (- (point) block)))
              (setq head (plist-put
                          head :http-headers
                          (warcel--parse-fields
                           (split-string (buffer-substring-no-properties
                                          (min status-end (match-beginning 0))
                                          (match-beginning 0))
                                         "\r?\n" t)))))))
        head))))

(defun warcel--make-record (head member offset)
  "Return a record described by HEAD, starting at OFFSET of MEMBER."
  (warcel-record--create :member member
                         :offset offset
                         :header-len (plist-get head :header-len)
                         :length (plist-get head :length)
                         :headers (plist-get head :headers)
                         :http-status (plist-get head :http-status)
                         :http-headers (plist-get head :http-headers)
                         :http-header-len (plist-get head :http-header-len)))

(defun warcel--read-head (reader offset)
  "Parse the head of the record at OFFSET in READER, or return nil."
  (let ((size (warcel--reader-size reader))
        (length warcel--head-size)
        head)
    (while (and (not head) length)
      (let ((pos (warcel--reader-at reader offset length)))
        (with-current-buffer (warcel--reader-buffer reader)
          (setq head (warcel--parse-head pos (point-max)))))
      (unless head
        ;; Unusually large head: look further before giving up.
        (setq length (and (< (+ offset length) size)
                          (< length (* 16 warcel--head-size))
                          (* 4 length)))))
    head))

(defun warcel--skip-newlines (reader offset)
  "Return the offset of the first byte at or after OFFSET that is not a newline.
READER holds the data."
  (let ((size (warcel--reader-size reader))
        done)
    (while (and (not done) (< offset size))
      (let ((pos (warcel--reader-at reader offset 1)))
        (with-current-buffer (warcel--reader-buffer reader)
          (goto-char pos)
          (skip-chars-forward "\r\n")
          (setq offset (+ offset (- (point) pos))
                done (< (point) (point-max))))))
    offset))

(defun warcel--resync (reader from)
  "Return the offset of the first record head at or after FROM in READER."
  (let ((size (warcel--reader-size reader))
        (case-fold-search nil)
        found)
    (while (and (not found) from (< from size))
      (let* ((pos (warcel--reader-at reader from warcel--head-size))
             (candidate (with-current-buffer (warcel--reader-buffer reader)
                          (goto-char pos)
                          (and (re-search-forward "^WARC/1\\.[0-9]+\r?\n" nil t)
                               (+ (- from pos) (match-beginning 0))))))
        (cond
         ((not candidate)
          (let ((end (warcel--reader-end reader)))
            (setq from (and (< end size) (max (1+ from) (- end 12))))))
         ((warcel--read-head reader candidate)
          (setq found candidate))
         (t (setq from (1+ candidate))))))
    found))

(defun warcel--scan-plain (reader member emit)
  "Call EMIT with each record of the uncompressed WARC data in READER.
MEMBER is the gzip member the data was inflated from, or nil."
  (let ((size (warcel--reader-size reader))
        (offset 0)
        last)
    (while (< (setq offset (warcel--skip-newlines reader offset)) size)
      (let ((head (warcel--read-head reader offset)))
        (unless head
          ;; The previous record may be shorter than it claims, or be
          ;; followed by garbage: go on with the next record head.
          (let* ((block (and last (+ (warcel-record-offset last)
                                     (warcel-record-header-len last))))
                 (next (warcel--resync reader (or block offset))))
            (unless next
              (error "No WARC record at offset %d" offset))
            (when (and block (< next offset))
              (setf (warcel-record-length last) (- next block)))
            (setq offset next
                  head (warcel--read-head reader next))))
        (setq last (warcel--make-record head member offset))
        (funcall emit last)
        (setq offset (+ offset (plist-get head :header-len)
                        (plist-get head :length)))
        (unless member
          (warcel--progress offset))))))

(defun warcel--complete-end (reader offset)
  "Return the offset after the complete records from OFFSET in READER."
  (let ((size (warcel--reader-size reader))
        done)
    (while (not done)
      (let* ((start (warcel--skip-newlines reader offset))
             (head (and (< start size) (warcel--read-head reader start)))
             (end (and head (+ start (plist-get head :header-len)
                               (plist-get head :length)))))
        (if (and end (<= end size))
            (setq offset end)
          (setq done t))))
    offset))

(defun warcel--scan-gzip-segment (reader offset end next data emit)
  "Call EMIT with each record of the gzip members from OFFSET in READER.
The first member ends at END, where NEXT is the inflated start of the
following member.  DATA is nil or a buffer holding the inflated first
member.  Members are taken until their records are complete.  Return
\(END . NEXT) for the end of the last member taken."
  (let ((file (warcel--reader-file reader))
        (size (warcel--reader-size reader))
        (buffer (warcel--unibyte-buffer " *warcel-segment*"))
        (members (list (cons offset end)))
        (complete 0))
    (unwind-protect
        (progn
          (warcel--append-member buffer file offset end data)
          (while (and (< end size)
                      (let ((inflated (warcel--buffer-reader buffer)))
                        (setq complete (warcel--complete-end inflated complete))
                        (< (warcel--skip-newlines inflated complete)
                           (warcel--reader-size inflated))))
            (pcase-let ((`(,member-end ,member-next ,_ ,member-data)
                         (warcel--gzip-member-end reader end nil)))
              (warcel--append-member buffer file end member-end member-data)
              (push (cons end member-end) members)
              (setq end member-end
                    next member-next)))
          (setq members (nreverse members))
          (warcel--scan-plain (warcel--buffer-reader buffer)
                              (if (cdr members) members (car members))
                              emit))
      (kill-buffer buffer))
    (cons end next)))

(defun warcel--scan-gzip (reader emit)
  "Call EMIT with each record of the gzip-compressed WARC data in READER."
  (let ((size (warcel--reader-size reader))
        (offset 0)
        (peek (car (warcel--gunzip-peek reader 0))))
    (while (< offset size)
      (let* ((start (warcel--warc-start peek))
             (head (and start
                        (with-temp-buffer
                          (set-buffer-multibyte nil)
                          (insert peek)
                          (warcel--parse-head (1+ start) (point-max))))))
        (pcase-let ((`(,end ,next ,aligned ,data)
                     (warcel--gzip-member-end
                      reader offset
                      (and head (+ start (plist-get head :header-len)
                                   (plist-get head :length) 4)))))
          (if (and head aligned)
              (funcall emit (warcel--make-record head (cons offset end) start))
            (let ((segment (warcel--scan-gzip-segment reader offset end next
                                                      data emit)))
              (setq end (car segment)
                    next (cdr segment))))
          (warcel--progress end)
          (setq offset end
                peek next))))))

(defun warcel--file-format (reader)
  "Return the format of the file read by READER: gzip, zstd, warc or nil."
  (let* ((pos (warcel--reader-at reader 0 8))
         (magic (with-current-buffer (warcel--reader-buffer reader)
                  (buffer-substring-no-properties pos (point-max)))))
    (cond
     ((string-prefix-p (unibyte-string #x1f #x8b) magic) 'gzip)
     ((string-prefix-p (unibyte-string #x28 #xb5 #x2f #xfd) magic) 'zstd)
     ((string-match-p "\\`[\r\n]*WARC/" magic) 'warc))))

(defun warcel-read-archive (file)
  "Read the records of the WARC FILE and return a `warcel-archive'."
  (unless (file-readable-p file)
    (user-error "Cannot read %s" file))
  (let* ((file (expand-file-name file))
         (reader (warcel--file-reader file))
         (records nil)
         (count 0)
         (emit (lambda (record)
                 (setf (warcel-record-index record) (cl-incf count))
                 (push record records)))
         (warcel--progress (make-progress-reporter
                            (format "Reading %s..." (file-name-nondirectory file))
                            0 (max 1 (warcel--reader-size reader))))
         problem)
    (unwind-protect
        (condition-case err
            (pcase (warcel--file-format reader)
              ('gzip (warcel--scan-gzip reader emit))
              ('warc (warcel--scan-plain reader nil emit))
              ('zstd (user-error "Zstandard-compressed WARC files are not supported"))
              (_ (user-error "Not a WARC file: %s" file)))
          (error
           ;; Keep what was read from a truncated or damaged file.
           (if records
               (setq problem (error-message-string err))
             (signal (car err) (cdr err)))))
      (kill-buffer (warcel--reader-buffer reader)))
    (progress-reporter-done warcel--progress)
    (when problem
      (message "Stopped reading %s after %d records: %s"
               (file-name-nondirectory file) count problem))
    (warcel--make-archive file (vconcat (nreverse records)))))

;;;; Archive indexes

(defun warcel-record-header (record name)
  "Return the value of the WARC header field NAME of RECORD."
  (warcel--field (warcel-record-headers record) name))

(defun warcel-record-type (record)
  "Return the WARC type of RECORD, such as \"response\"."
  (downcase (or (warcel-record-header record "WARC-Type") "")))

(defun warcel-record-uri (record)
  "Return the target URI of RECORD, or nil."
  (let ((uri (warcel-record-header record "WARC-Target-URI")))
    (and uri (string-trim uri "[< \t]+" "[> \t]+"))))

(defun warcel-record-content-type (record)
  "Return the content type of RECORD's payload, with its parameters."
  (or (if (warcel-record-http-status record)
          (warcel--field (warcel-record-http-headers record) "Content-Type")
        (warcel-record-header record "Content-Type"))
      ""))

(defun warcel-record-mime (record)
  "Return the media type of RECORD's payload, such as \"text/html\"."
  (downcase (string-trim (car (split-string (warcel-record-content-type record)
                                            ";")))))

(defun warcel--payload-size (record)
  "Return the size of RECORD's payload as stored in the archive."
  (max 0 (- (warcel-record-length record)
            (or (warcel-record-http-header-len record) 0))))

(defun warcel--normalize-uri (uri)
  "Return URI in the form used to index and look up records."
  (let ((uri (replace-regexp-in-string
              "#.*\\'" "" (string-trim uri "[< \t]+" "[> \t]+"))))
    (if (string-match "\\`\\([a-zA-Z][a-zA-Z0-9+.-]*\\)://\\([^/?#]*\\)\\(.*\\)\\'"
                      uri)
        (let ((scheme (downcase (match-string 1 uri)))
              (host (downcase (match-string 2 uri)))
              (rest (match-string 3 uri)))
          (setq host (pcase scheme
                       ("http" (string-remove-suffix ":80" host))
                       ("https" (string-remove-suffix ":443" host))
                       (_ host)))
          (concat scheme "://" host (if (string-empty-p rest) "/" rest)))
      uri)))

(defun warcel--uri-variants (uri)
  "Return the forms of URI to look up, most specific first."
  (let* ((uri (warcel--normalize-uri uri))
         (other (cond
                 ((string-match-p "\\`[^:/]+://[^/]*/\\'" uri) nil)
                 ((string-suffix-p "/" uri) (substring uri 0 -1))
                 ((not (string-search "?" uri)) (concat uri "/"))))
         (forms (delq nil (list uri other))))
    (append forms
            (delq nil (mapcar (lambda (form)
                                (cond
                                 ((string-prefix-p "http://" form)
                                  (concat "https://" (substring form 7)))
                                 ((string-prefix-p "https://" form)
                                  (concat "http://" (substring form 8)))))
                              forms)))))

(defun warcel--make-archive (file records)
  "Return an archive of the vector RECORDS read from FILE."
  (let ((by-uri (make-hash-table :test #'equal))
        (by-id (make-hash-table :test #'equal))
        (by-digest (make-hash-table :test #'equal)))
    (seq-doseq (record records)
      (let ((type (warcel-record-type record))
            (id (warcel-record-header record "WARC-Record-ID"))
            (uri (warcel-record-uri record))
            (digest (warcel-record-header record "WARC-Payload-Digest")))
        (when id
          (puthash id record by-id))
        (when (and uri (member type '("response" "resource" "revisit" "conversion")))
          (push record (gethash (warcel--normalize-uri uri) by-uri)))
        (when (and digest (member type '("response" "resource"))
                   (not (gethash digest by-digest)))
          (puthash digest record by-digest))))
    (maphash (lambda (key value) (puthash key (nreverse value) by-uri)) by-uri)
    (warcel-archive--create :file file :records records :by-uri by-uri
                            :by-id by-id :by-digest by-digest)))

(defun warcel--lookup (archive url)
  "Return the record of ARCHIVE that best represents URL, or nil."
  (when-let* ((records (seq-some (lambda (form)
                                   (gethash form (warcel-archive-by-uri archive)))
                                 (warcel--uri-variants url))))
    (or (seq-find (lambda (record)
                    (let ((status (warcel-record-http-status record)))
                      (and status (<= 200 status 299))))
                  records)
        (seq-find (lambda (record)
                    (equal (warcel-record-type record) "resource"))
                  records)
        (car records))))

(defun warcel--revisit-target (archive record)
  "Return the record of ARCHIVE holding the payload of the revisit RECORD."
  (or (when-let* ((id (warcel-record-header record "WARC-Refers-To")))
        (gethash id (warcel-archive-by-id archive)))
      (when-let* ((digest (warcel-record-header record "WARC-Payload-Digest")))
        (gethash digest (warcel-archive-by-digest archive)))
      (when-let* ((uri (or (warcel-record-header record "WARC-Refers-To-Target-URI")
                           (warcel-record-uri record))))
        (seq-find (lambda (other)
                    (member (warcel-record-type other) '("response" "resource")))
                  (gethash (warcel--normalize-uri uri)
                           (warcel-archive-by-uri archive))))))

(defun warcel--redirect-location (record)
  "Return the Location of RECORD if it is an HTTP redirect."
  (let ((status (warcel-record-http-status record)))
    (and status (<= 300 status 399)
         (warcel--field (warcel-record-http-headers record) "Location"))))

(defun warcel--resolve (archive record &optional keep-redirects)
  "Return the record of ARCHIVE whose payload should be shown for RECORD.
Revisit records are replaced by the record they refer to, and
redirects by their archived target unless KEEP-REDIRECTS is non-nil."
  (let (seen next)
    (while (and (not (memq record seen))
                (setq next
                      (if (equal (warcel-record-type record) "revisit")
                          (warcel--revisit-target archive record)
                        (when-let* (((not keep-redirects))
                                    (location (warcel--redirect-location record)))
                          (warcel--lookup archive
                                          (warcel--expand-url
                                           location
                                           (warcel-record-uri record)))))))
      (push record seen)
      (setq record next))
    record))

;;;; Payloads

(defvar warcel--member-cache nil
  "The last inflated gzip member, as (KEY . BUFFER).")

(defun warcel--member-data (file member)
  "Return a buffer holding the inflated gzip MEMBER of FILE.
MEMBER is (BEG . END), or a list of them to inflate one after the other."
  (let ((key (list file member
                   (file-attribute-modification-time (file-attributes file))))
        (buffer (cdr warcel--member-cache)))
    (unless (and (buffer-live-p buffer)
                 (equal key (car warcel--member-cache)))
      (if (buffer-live-p buffer)
          (with-current-buffer buffer (erase-buffer))
        (setq buffer (warcel--unibyte-buffer " *warcel-member*")))
      (dolist (range (if (consp (car member)) member (list member)))
        (warcel--inflate-member file (car range) (cdr range) buffer))
      (setq warcel--member-cache (cons key buffer)))
    buffer))

(defun warcel--record-block (archive record)
  "Return the block of RECORD in ARCHIVE as a unibyte string."
  (let* ((file (warcel-archive-file archive))
         (start (+ (warcel-record-offset record)
                   (warcel-record-header-len record)))
         (end (+ start (warcel-record-length record)))
         (member (warcel-record-member record)))
    (if member
        (with-current-buffer (warcel--member-data file member)
          (buffer-substring-no-properties (min (1+ start) (point-max))
                                          (min (1+ end) (point-max))))
      (let ((size (file-attribute-size (file-attributes file))))
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert-file-contents-literally file nil (min start size)
                                          (min end size))
          (buffer-string))))))

(defun warcel--split-http (block)
  "Split the HTTP message BLOCK into a list (START-LINE HEADERS BODY)."
  (if (string-match "\n\r?\n" block)
      (let* ((body (substring block (match-end 0)))
             (lines (split-string (substring block 0 (match-beginning 0))
                                  "\r?\n")))
        (list (car lines) (warcel--parse-fields (cdr lines)) body))
    (list block nil "")))

(defun warcel--dechunk (body)
  "Remove the chunked transfer coding from BODY.
Return BODY unchanged if it is not chunked."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert body)
    (goto-char (point-min))
    (let ((case-fold-search t)
          (ok t)
          done)
      (while (and ok (not done))
        (cond
         ((looking-at "\\([0-9a-f]+\\)[^\r\n]*\r?\n")
          (let ((size (string-to-number (match-string 1) 16)))
            (delete-region (match-beginning 0) (match-end 0))
            (cond
             ((zerop size)
              (delete-region (point) (point-max))
              (setq done t))
             ((> (+ (point) size) (point-max))
              (setq done t))
             (t
              (forward-char size)
              (if (looking-at "\r?\n")
                  (delete-region (match-beginning 0) (match-end 0))
                (setq ok nil))))))
         ((eobp) (setq done t))
         (t (setq ok nil))))
      (if ok (buffer-string) body))))

(defun warcel--inflate (data)
  "Inflate gzip, zlib or raw deflate DATA; return nil if it is none of them."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert data)
    (ignore-errors (zlib-decompress-region (point-min) (point-max) t))
    (when (zerop (buffer-size))
      ;; Some servers send raw deflate data: give it a zlib header and
      ;; accept the output despite the missing checksum.
      (insert (unibyte-string #x78 #x9c) data)
      (unless (ignore-errors (zlib-decompress-region (point-min) (point-max) t))
        (erase-buffer)))
    (and (> (buffer-size) 0) (buffer-string))))

(defun warcel--record-payload (archive record)
  "Return the payload of RECORD in ARCHIVE as a unibyte string.
Transfer and content codings of HTTP responses are removed."
  (let ((block (warcel--record-block archive record)))
    (if (not (warcel-record-http-status record))
        block
      (pcase-let ((`(,_ ,headers ,body) (warcel--split-http block)))
        (let ((transfer (downcase (or (warcel--field headers "Transfer-Encoding") "")))
              (content (downcase (string-trim
                                  (or (warcel--field headers "Content-Encoding") "")))))
          (when (string-search "chunked" transfer)
            (setq body (warcel--dechunk body)))
          (cond
           ((or (string-empty-p body)
                (member content '("" "identity" "none")))
            body)
           ((member content '("gzip" "x-gzip" "deflate"))
            ;; Some crawlers store the payload already decoded.
            (or (warcel--inflate body) body))
           (t (user-error "Unsupported content encoding: %s" content))))))))

(defconst warcel--charset-aliases
  '(("iso-8859-1" . windows-1252) ("latin1" . windows-1252)
    ("us-ascii" . windows-1252) ("ascii" . windows-1252)
    ("utf8" . utf-8) ("x-sjis" . shift_jis) ("shift-jis" . shift_jis)
    ("gb2312" . gbk) ("x-gbk" . gbk) ("ks_c_5601-1987" . euc-kr))
  "Charset names that differ from the Emacs coding system to use.")

(defun warcel--coding-system (charset)
  "Return the coding system for the MIME CHARSET name, or nil."
  (let* ((name (downcase (string-trim charset "[\"' \t]+" "[\"' \t]+")))
         (coding (or (cdr (assoc name warcel--charset-aliases)) (intern name))))
    (and (coding-system-p coding) coding)))

(defun warcel--charset (content-type bytes)
  "Return the coding system of the text BYTES with CONTENT-TYPE."
  (let ((case-fold-search t)
        (head (substring bytes 0 (min 4096 (length bytes)))))
    (or (and (string-match "charset=\\([^;]+\\)" content-type)
             (warcel--coding-system (match-string 1 content-type)))
        (and (string-match "<meta[^>]*charset=[\"']?\\([-_.:a-z0-9]+\\)" head)
             (warcel--coding-system (match-string 1 head)))
        'prefer-utf-8)))

(defun warcel--payload-text (archive record)
  "Return the payload of RECORD in ARCHIVE decoded as text."
  (let ((bytes (warcel--record-payload archive record)))
    (decode-coding-string
     bytes (warcel--charset (warcel-record-content-type record) bytes))))

;;;; HTML

(defconst warcel--dropped-tags
  '(script noscript style template link meta base
    img image picture source track video audio canvas svg math map area
    iframe object embed applet param
    input button select option optgroup textarea datalist output
    progress meter comment)
  "HTML elements removed from pages before they are shown.")

(defconst warcel--kept-attributes
  '(href id name title start value colspan rowspan dir lang)
  "HTML attributes kept on elements of pages that are shown.")

(defun warcel--expand-url (url base)
  "Return URL made absolute with respect to BASE."
  (let ((url (string-trim url)))
    (if (or (not base) (string-empty-p base)
            (string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*:" url))
        url
      (condition-case nil
          (url-expand-file-name url base)
        (error url)))))

(defun warcel--link-url-p (url &optional absolute)
  "Return non-nil if URL may be followed from a page that is shown.
That is an http, https or mailto URL or, unless ABSOLUTE is non-nil,
a relative one.  Pages come from anywhere: other schemes, such as
javascript:, or elisp:, shell: and file+sys: for Org, could run code
or open local files."
  (let ((case-fold-search t)
        (url (string-trim url)))
    (if (string-match-p "\\`[a-z][a-z0-9+.-]*:" url)
        (string-match-p "\\`\\(?:https?\\|mailto\\):" url)
      (not absolute))))

(defun warcel--hidden-p (node)
  "Return non-nil if the HTML element NODE is not meant to be shown."
  (let ((attributes (dom-attributes node))
        (case-fold-search t))
    (or (assq 'hidden attributes)
        (equal (cdr (assq 'aria-hidden attributes)) "true")
        (when-let* ((style (cdr (assq 'style attributes))))
          (string-match-p "display[ \t]*:[ \t]*none\\|visibility[ \t]*:[ \t]*hidden"
                          style)))))

(defun warcel--dom-text (node)
  "Return all the text in the DOM NODE."
  (if (fboundp 'dom-inner-text)
      (dom-inner-text node)
    (with-no-warnings (dom-texts node ""))))

(defun warcel--has-text-p (nodes)
  "Return non-nil if the DOM NODES contain visible text."
  (seq-some (lambda (node)
              (if (stringp node)
                  (string-match-p "[^ \t\n\r]" node)
                (warcel--has-text-p (dom-children node))))
            nodes))

(defun warcel--image-alt (node)
  "Return the first non-empty alternative text of an image in NODE."
  (seq-some (lambda (image)
              (let ((alt (dom-attr image 'alt)))
                (and alt (string-match-p "[^ \t\n\r]" alt) (string-trim alt))))
            (dom-by-tag node 'img)))

(defun warcel--sanitize (node)
  "Return a copy of the DOM NODE fit to be shown, or nil to drop it.
Scripts, styles, media, embedded frames and form controls are removed,
and so are all attributes but `warcel--kept-attributes'."
  (cond
   ((stringp node) node)
   ((or (not (consp node))
        (memq (dom-tag node) warcel--dropped-tags)
        (warcel--hidden-p node))
    nil)
   ((eq (dom-tag node) 'head)
    (when-let* ((title (car (dom-by-tag node 'title))))
      `(head nil (title nil ,(warcel--dom-text title)))))
   ((eq (dom-tag node) 'frame)
    ;; Framesets would be empty without their frames: link to them.
    (when-let* ((src (dom-attr node 'src))
                ((warcel--link-url-p src)))
      `(p nil (a ((href . ,src)) ,(format "Frame: %s" (or (dom-attr node 'name) src))))))
   (t
    (let ((children (delq nil (mapcar #'warcel--sanitize (dom-children node))))
          (attributes (seq-filter
                       (lambda (attribute)
                         (and (memq (car attribute) warcel--kept-attributes)
                              (or (not (eq (car attribute) 'href))
                                  (warcel--link-url-p (cdr attribute)))))
                       (dom-attributes node))))
      ;; Keep links whose only content was an image.
      (when (and (eq (dom-tag node) 'a) (not (warcel--has-text-p children)))
        (when-let* ((alt (warcel--image-alt node)))
          (setq children (list alt))))
      `(,(if (eq (dom-tag node) 'frameset) 'div (dom-tag node))
        ,attributes ,@children)))))

(defun warcel--meta-refresh (dom)
  "Return the URL that a meta refresh element of DOM redirects to."
  (let ((case-fold-search t))
    (seq-some (lambda (meta)
                (when (string-equal-ignore-case (or (dom-attr meta 'http-equiv) "")
                                                "refresh")
                  (let ((content (or (dom-attr meta 'content) "")))
                    (when (string-match "url[ \t]*=[ \t]*[\"']?\\([^\"']+\\)" content)
                      (string-trim (match-string 1 content))))))
              (dom-by-tag dom 'meta))))

(defun warcel--in-tag-p (pos)
  "Return non-nil if POS is between the attributes of an HTML tag."
  (save-excursion
    (goto-char pos)
    (when (and (search-backward "<" nil t) (looking-at "<[a-zA-Z]"))
      (let (quote outside)
        (while (and (< (point) pos) (not outside))
          (let ((char (char-after)))
            (cond
             (quote (when (eq char quote) (setq quote nil)))
             ((memq char '(?\" ?')) (setq quote char))
             ((eq char ?>) (setq outside t))))
          (forward-char 1))
        (not (or quote outside))))))

(defun warcel--mark-hidden (html)
  "Return HTML with a value given to its bare `hidden' attributes.
The HTML parser drops attributes it does not know that have no value."
  (let ((case-fold-search t)
        (regexp "[ \t\n\r]\\(hidden\\)[ \t\n\r/>]"))
    (if (not (string-match-p regexp html))
        html
      (with-temp-buffer
        (insert html)
        (goto-char (point-min))
        (while (re-search-forward regexp nil t)
          (let ((end (match-end 1)))
            (when (warcel--in-tag-p (match-beginning 1))
              (goto-char end)
              (insert "=\"\""))))
        (buffer-string)))))

(defun warcel--parse-html (text)
  "Parse the HTML TEXT into a DOM."
  (with-temp-buffer
    (insert (warcel--mark-hidden text))
    (libxml-parse-html-region (point-min) (point-max))))

;;;; Main content of a page

;; Finding the article of a page, as reading views in browsers do: the
;; element holding most of its paragraphs is the content, widened to
;; the article or main element around it; within it, what is around
;; the text of an article is left out.  This works on the page as
;; parsed, before `warcel--sanitize' removes the classes, ids and roles
;; it relies on.

;; Word boundaries are spelled out: \\b and \\_< depend on the syntax
;; table of the current buffer.
(defconst warcel--unlikely-regexp
  (concat "-ad-\\|\\(?:^\\|[^a-z]\\)ads?\\(?:[^a-z]\\|$\\)\\|advert\\|agegate"
          "\\|banner\\|breadcrumb\\|combx\\|comment\\|community\\|consent\\|cookie"
          "\\|disqus\\|footer\\|gdpr\\|header\\|legends\\|masthead\\|menu"
          "\\|modal\\|\\(?:^\\|[^a-z]\\)nav\\|newsletter\\|outbrain\\|pager"
          "\\|pagination\\|popup\\|promo\\|related\\|remark\\|replies\\|rss\\|share"
          "\\|sharing\\|shoutbox\\|sidebar\\|skip\\|skyscraper\\|social\\|sponsor"
          "\\|subscribe\\|supplemental\\|taboola\\|toolbar\\|widget")
  "Regexp matching the classes and ids of what is around the content of a page.")

(defconst warcel--maybe-regexp
  (concat "article\\|\\(?:^\\|[^a-z]\\)body\\(?:[^a-z]\\|$\\)\\|column\\|content"
          "\\|\\(?:^\\|[^a-z]\\)main\\(?:[^a-z]\\|$\\)\\|story\\|entry"
          "\\|\\(?:^\\|[^a-z]\\)post")
  "Regexp matching classes and ids that may be the content of a page.
An element whose classes or ids match it is kept even when they also
match `warcel--unlikely-regexp'.")

(defconst warcel--boilerplate-roles
  '("navigation" "banner" "contentinfo" "complementary" "search" "menu"
    "menubar" "dialog" "alertdialog" "alert" "tooltip")
  "ARIA roles of what is around the content of a page.")

(defun warcel--long-paragraphs (node)
  "Return how many paragraphs in NODE are long ones."
  (seq-count (lambda (paragraph) (>= (warcel--text-length paragraph) 150))
             (dom-by-tag node 'p)))

(defun warcel--holds-article-p (node &optional paragraphs)
  "Return non-nil if NODE holds the article of the page.
It does when it contains the main element, or an article element with
a few long paragraphs: not one that is a comment or a link to another
page.  With PARAGRAPHS non-nil, it also does when it has a few long
paragraphs anywhere, as when broken markup puts a page in its header."
  (or (dom-search node (lambda (child)
                         (or (eq (dom-tag child) 'main)
                             (equal (dom-attr child 'role) "main")
                             (and (eq (dom-tag child) 'article)
                                  (>= (warcel--long-paragraphs child) 3)))))
      (and paragraphs (>= (warcel--long-paragraphs node) 3))))

(defun warcel--boilerplate-p (node)
  "Return non-nil if the element NODE is around the content of a page.
That is navigation, a header without the title, a footer, a sidebar,
sharing buttons, related pages, comments and the like."
  (let ((tag (dom-tag node))
        (names (downcase (concat (or (dom-attr node 'class) "") " "
                                 (or (dom-attr node 'id) ""))))
        (case-fold-search t))
    ;; Markup says little for sure: what holds the article stays.
    (or (and (or (memq tag '(nav aside footer menu dialog)) ; not form: some
                 (and (eq tag 'header) (not (dom-by-tag node 'h1))) ; pages are one
                 (member (downcase (or (dom-attr node 'role) ""))
                         warcel--boilerplate-roles))
             (not (warcel--holds-article-p node t)))
        (and (not (memq tag '(html body article main a)))
             (string-match-p warcel--unlikely-regexp names)
             (not (string-match-p warcel--maybe-regexp names))
             (not (warcel--holds-article-p node))))))

(defun warcel--text-length (node)
  "Return the length of the text in NODE, without surrounding whitespace."
  (length (string-trim (warcel--dom-text node))))

(defun warcel--link-density (node)
  "Return the share of the text of NODE that is in links, from 0 to 1."
  (let ((length (warcel--text-length node)))
    (if (zerop length)
        0
      (/ (float (apply #'+ (mapcar #'warcel--text-length (dom-by-tag node 'a))))
         length))))

(defun warcel--link-list-p (node)
  "Return non-nil if NODE is a block of little more than links.
Menus and lists of related pages are such blocks.  Tables are not
looked at: tables of data often have a link on each row."
  (and (memq (dom-tag node) '(ul ol div section))
       (> (warcel--link-density node) 0.5)
       (not (warcel--holds-article-p node))))

(defun warcel--readable-clean (node)
  "Return a copy of NODE without what is around the content of a page."
  (cond
   ((stringp node) node)
   ;; What is never shown goes first, so that the text of styles and
   ;; scripts does not count as text of the page.
   ((or (memq (dom-tag node) warcel--dropped-tags) (warcel--hidden-p node)) nil)
   ((or (warcel--boilerplate-p node) (warcel--link-list-p node)) nil)
   (t `(,(dom-tag node) ,(dom-attributes node)
        ,@(delq nil (mapcar #'warcel--readable-clean (dom-children node)))))))

(defconst warcel--block-tags
  '(address article aside blockquote details dialog dd div dl dt fieldset
    figcaption figure footer form h1 h2 h3 h4 h5 h6 header hr li main nav ol
    p pre section table ul)
  "HTML elements that start a block of their own.")

(defun warcel--div-paragraph-p (node)
  "Return non-nil if NODE is a div used as a paragraph: text without blocks."
  (and (eq (dom-tag node) 'div)
       (not (seq-some (lambda (child) (memq (dom-tag child) warcel--block-tags))
                      (dom-non-text-children node)))))

(defun warcel--dom-parents (dom)
  "Return a hash table mapping each element of DOM to its parent."
  (let ((parents (make-hash-table :test #'eq)))
    (cl-labels ((walk (node)
                  (dolist (child (dom-non-text-children node))
                    (puthash child node parents)
                    (walk child))))
      (walk dom))
    parents))

(defun warcel--main-element (body)
  "Return the element of BODY that holds the main content, or nil.
Each paragraph scores for its parent, and half as much for its
grandparent; the best element, its links discounted, wins.  It is
widened to the article or main element around it, if any."
  (let ((parents (warcel--dom-parents body))
        (scores (make-hash-table :test #'eq))
        best best-score)
    (dolist (paragraph (append (dom-by-tag body 'p) (dom-by-tag body 'pre)
                               ;; Pages also write paragraphs as divs.
                               (dom-search body #'warcel--div-paragraph-p)))
      (let* ((text (string-trim (warcel--dom-text paragraph)))
             (score (+ 1 (cl-count ?, text) (min 3 (/ (length text) 100)))))
        (when (>= (length text) 25)
          (when-let* ((parent (gethash paragraph parents)))
            (puthash parent (+ score (gethash parent scores 0)) scores)
            (when-let* ((grandparent (gethash parent parents)))
              (puthash grandparent (+ (/ score 2.0) (gethash grandparent scores 0))
                       scores))))))
    (maphash (lambda (node score)
               (let ((score (* score (- 1 (warcel--link-density node)))))
                 (when (or (not best) (> score best-score))
                   (setq best node best-score score))))
             scores)
    ;; Less than a few paragraphs: there is no article to find.
    (when (and best (>= best-score 5))
      (let ((node best))
        (while (and node
                    (not (memq (dom-tag node) '(article main body html)))
                    (not (equal (dom-attr node 'role) "main")))
          (setq node (gethash node parents)))
        (if (and node (not (memq (dom-tag node) '(body html))))
            node
          (warcel--with-content-siblings best (gethash best parents) scores
                                         (max 10 (* 0.2 best-score))))))))

(defun warcel--with-content-siblings (best parent scores threshold)
  "Return BEST with the siblings that are content too, in their order.
An article is often split into several elements under PARENT: those
scoring at least THRESHOLD in SCORES are content, and so are the
paragraphs, headings, lists and quotes around them with few links."
  (let ((kept (seq-filter
               (lambda (sibling)
                 (and (consp sibling)
                      (or (eq sibling best)
                          (>= (* (gethash sibling scores 0)
                                 (- 1 (warcel--link-density sibling)))
                              threshold)
                          (and (memq (dom-tag sibling)
                                     '(p h1 h2 h3 h4 h5 h6 ul ol dl blockquote pre figure table))
                               (warcel--has-text-p (list sibling))
                               (< (warcel--link-density sibling) 0.25)))))
               (and parent (dom-children parent)))))
    (if (cdr kept) `(div nil ,@kept) best)))

(defun warcel--dom-title-text (node)
  "Return the text of NODE on one line, or nil if it has none."
  (let ((text (string-trim (replace-regexp-in-string
                            "[ \t\n\r]+" " " (warcel--dom-text node)))))
    (and (not (string-empty-p text)) text)))

(defun warcel--readable-dom (dom)
  "Return the title and main content of the page DOM, as (TITLE . DOM).
The DOM returned holds only the main content, without the title when
it opened it.  When no article is found, as on pages behind a paywall,
it is the whole page without what is around content: menus, headers,
footers, sidebars, dialogs.  Return nil if nothing is left."
  ;; What is around the content goes first, so that a long comment
  ;; section, say, can't be taken for the article.
  (when-let* ((body (warcel--readable-clean (or (car (dom-by-tag dom 'body)) dom)))
              ((warcel--has-text-p (list body)))
              (content (or (warcel--main-element body) body)))
    (let* ((heading (car (dom-by-tag content 'h1)))
           (title (or (and heading (warcel--dom-title-text heading))
                      (seq-some (lambda (meta)
                                  (and (equal (dom-attr meta 'property) "og:title")
                                       (dom-attr meta 'content)))
                                (dom-by-tag dom 'meta)))))
      ;; The title is shown as such: not again as the first heading.
      (when (and heading (equal title (warcel--dom-title-text heading)))
        (dom-remove-node content heading))
      (cons title `(html nil (body nil ,content))))))

(defun warcel--html-p (archive record)
  "Return non-nil if the payload of RECORD in ARCHIVE is an HTML page."
  (let ((mime (warcel-record-mime record)))
    (or (member mime '("text/html" "application/xhtml+xml"))
        (warcel--redirect-location record)
        (and (member mime '("" "application/octet-stream"))
             (let ((case-fold-search t)
                   (payload (warcel--record-payload archive record)))
               (string-match-p "\\`\\(?:\357\273\277\\)?[ \t\r\n]*<\\(?:!doctype html\\|html\\|head\\|body\\)"
                               (substring payload 0 (min 512 (length payload)))))))))

(defun warcel--page (archive record &optional readable)
  "Return the HTML page stored in RECORD of ARCHIVE, ready to be shown.
The result is a plist with :record, :uri, :date, :title, :base and
:dom, the sanitized DOM of the page.  With READABLE non-nil, the DOM
is only the main content of the page when it has one (see
`warcel--readable-dom'), and :readable is then non-nil."
  (let* ((max-lisp-eval-depth (max max-lisp-eval-depth 4000))
         (payload (warcel--record-payload archive record))
         (dom (if (string-blank-p payload)
                  (list 'html nil (list 'body nil))
                (warcel--parse-html
                 (decode-coding-string
                  payload (warcel--charset (warcel-record-content-type record)
                                           payload)))))
         (uri (or (warcel-record-uri record) ""))
         (base-href (dom-attr (car (dom-by-tag dom 'base)) 'href))
         (base (if base-href (warcel--expand-url base-href uri) uri))
         (title (when-let* ((node (car (dom-by-tag dom 'title))))
                  (string-trim (replace-regexp-in-string
                                "[ \t\n\r]+" " " (warcel--dom-text node)))))
         (redirect (or (warcel--redirect-location record)
                       (warcel--meta-refresh dom)))
         (main (and readable (not redirect) (warcel--readable-dom dom)))
         (title (or (car main) title))
         (clean (or (warcel--sanitize (if main (cdr main) dom)) (list 'html nil))))
    (when redirect
      (let ((target (warcel--expand-url redirect base)))
        (dom-add-child-before (or (car (dom-by-tag clean 'body)) clean)
                              `(p nil "Redirects to " (a ((href . ,target)) ,target)))))
    (list :record record
          :uri uri
          :date (warcel-record-header record "WARC-Date")
          :title (if (or (not title) (string-empty-p title)) uri title)
          :base base
          :readable (and main t)
          :dom clean)))

;;;; HTML to Org

(defconst warcel--zwsp "\u200b"
  "Zero-width space, used where Org markup must be separated from text.")

(defvar warcel--org-base nil
  "URL that relative links of the page being converted are relative to.")

(defvar warcel--org-inline nil
  "Non-nil when converting inside a link, heading or table cell.
Block structure is then flattened into running text.")

(defvar warcel--org-in-block nil
  "Non-nil inside list items, quotes and tables, where headings cannot go.")

(defvar warcel--org-in-quote nil
  "Non-nil inside a quote block.")

(defvar warcel--org-in-link nil
  "Non-nil inside a link.")

(defvar warcel--org-markers nil
  "Emphasis markers of the enclosing emphasized text.")

(defun warcel--org-after-space-p ()
  "Return non-nil if the Org text so far ends with a space or nothing."
  (goto-char (point-max))
  (or (bobp) (memq (char-before) '(?\s ?\t ?\n #x200b))))

(defun warcel--org-text (string)
  "Insert the text of a DOM text node STRING."
  (let ((text (replace-regexp-in-string "[ \t\n\r\f\u00a0]+" " " string)))
    (when (and (string-prefix-p " " text) (warcel--org-after-space-p))
      (setq text (substring text 1)))
    (unless (string-empty-p text)
      (goto-char (point-max))
      (when (or (and (bolp) (string-match-p "\\`[*#|:]" text))
                (and (> (point) (point-min))
                     (get-text-property (1- (point)) 'warcel-closing)
                     (string-match-p "\\`[[:alnum:]]" text)))
        ;; Keep Org from reading the text as markup, or from missing
        ;; the end of the emphasis just before it.
        (insert warcel--zwsp))
      (insert text))))

(defun warcel--org-newlines (n)
  "End the Org text with at least N newlines, unless it is empty."
  (goto-char (point-max))
  (if warcel--org-inline
      (unless (warcel--org-after-space-p)
        (insert " "))
    (let ((end (point)))
      (skip-chars-backward " \t\n")
      (if (bobp)
          (delete-region (point) end)
        (let ((have (cl-count ?\n (buffer-substring-no-properties (point) end))))
          (delete-region (point) end)
          (insert (make-string (max n have) ?\n)))))))

(defun warcel--org-children (node)
  "Insert the Org text of the children of NODE."
  (dolist (child (dom-children node))
    (warcel--org-node child)))

(defun warcel--org-block (node n)
  "Insert NODE as a block separated from its surroundings by N newlines."
  (warcel--org-newlines n)
  (warcel--org-children node)
  (warcel--org-newlines n))

(defun warcel--org-flat (text)
  "Return TEXT on a single line."
  (string-trim (replace-regexp-in-string "[ \t\n]+" " " text)))

(defun warcel--org-render (nodes &optional inline)
  "Return the Org text of the DOM NODES, converted on their own.
If INLINE is non-nil, flatten block structure."
  (let ((warcel--org-in-block t)
        (warcel--org-inline (or inline warcel--org-inline))
        (warcel--org-markers nil))
    (with-temp-buffer
      (dolist (node nodes)
        (warcel--org-node node))
      (warcel--org-tidy)
      (buffer-string))))

(defun warcel--org-tidy ()
  "Remove trailing spaces and extra blank lines from the Org text."
  (goto-char (point-min))
  (while (re-search-forward "[ \t]+$" nil t)
    (replace-match ""))
  (goto-char (point-min))
  (while (re-search-forward "\n\n\n+" nil t)
    (replace-match "\n\n"))
  (goto-char (point-min))
  (skip-chars-forward "\n")
  (delete-region (point-min) (point))
  (goto-char (point-max))
  (skip-chars-backward " \t\n")
  (delete-region (point) (point-max)))

(defun warcel--org-indent (text width)
  "Return TEXT with all lines but the first indented by WIDTH spaces."
  (replace-regexp-in-string "\n\\([^\n]\\)"
                            (concat "\n" (make-string width ?\s) "\\1")
                            text t))

(defun warcel--org-wrap (start marker)
  "Put the emphasis MARKER around the text inserted since START.
Do nothing if the text is empty or spans lines."
  (let ((beg (save-excursion (goto-char start)
                             (skip-chars-forward " \t\n")
                             (point)))
        (end (save-excursion (goto-char (point-max))
                             (skip-chars-backward " \t\n")
                             (point))))
    (when (and (< beg end)
               (not (string-search "\n" (buffer-substring-no-properties beg end))))
      (goto-char end)
      (insert (propertize marker 'warcel-closing t))
      (goto-char beg)
      (unless (or (= beg (point-min))
                  (memq (char-before) '(?\s ?\t ?\n ?- ?\( ?\" ?' ?{ #x200b)))
        (insert warcel--zwsp))
      (insert marker))
    (goto-char (point-max))))

(defun warcel--org-emphasis (node marker)
  "Insert the children of NODE emphasized with MARKER."
  (if (member marker warcel--org-markers)
      (warcel--org-children node)
    (goto-char (point-max))
    (let ((start (copy-marker (point)))
          (warcel--org-markers (cons marker warcel--org-markers)))
      (warcel--org-children node)
      (warcel--org-wrap start marker)
      (set-marker start nil))))

(defun warcel--org-verbatim (node)
  "Insert the children of NODE as verbatim text."
  (goto-char (point-max))
  (let ((start (copy-marker (point))))
    (warcel--org-children node)
    (let* ((text (buffer-substring-no-properties start (point-max)))
           (marker (cond ((string-search "[[" text) nil)
                         ((not (string-search "~" text)) "~")
                         ((not (string-search "=" text)) "="))))
      (when marker
        (warcel--org-wrap start marker)))
    (set-marker start nil)))

(defun warcel--org-link (node)
  "Insert the link NODE."
  (let* ((href (string-trim (or (dom-attr node 'href) "")))
         (url (unless (or warcel--org-in-link
                          (string-empty-p href)
                          (string-prefix-p "#" href))
                (warcel--expand-url href warcel--org-base))))
    ;; Checked again once made absolute: the page's base or redirect
    ;; could give it another scheme, and Org would take a path that is
    ;; still relative for a local file.
    (if (not (and url (warcel--link-url-p url t)))
        (warcel--org-children node)
      (goto-char (point-max))
      (let ((start (copy-marker (point))))
        (let ((warcel--org-in-link t)
              (warcel--org-inline t))
          (warcel--org-children node))
        (let* ((raw (buffer-substring-no-properties start (point-max)))
               (description (warcel--org-flat raw)))
          (delete-region start (point-max))
          (unless (string-empty-p description)
            (when (and (string-match-p "\\`[ \t\n]" raw)
                       (not (warcel--org-after-space-p)))
              (insert " "))
            (insert (org-link-make-string url description))
            (when (string-match-p "[ \t\n]\\'" raw)
              (insert " "))))
        (set-marker start nil)))))

(defun warcel--org-break ()
  "Insert a line break."
  (goto-char (point-max))
  (if warcel--org-inline
      (unless (warcel--org-after-space-p)
        (insert " "))
    (delete-region (progn (skip-chars-backward " \t") (point)) (point-max))
    (insert "\n")))

(defun warcel--org-rule ()
  "Insert a horizontal rule."
  (unless warcel--org-inline
    (warcel--org-newlines 2)
    (insert "-----")
    (warcel--org-newlines 2)))

(defun warcel--org-heading (node level)
  "Insert the heading NODE of LEVEL."
  (cond
   (warcel--org-inline
    (warcel--org-children node))
   (warcel--org-in-block
    (warcel--org-newlines 2)
    (warcel--org-emphasis node "*")
    (warcel--org-newlines 2))
   (t
    (warcel--org-newlines 2)
    (let ((start (copy-marker (point))))
      (let ((warcel--org-inline t))
        (warcel--org-children node))
      (let ((title (warcel--org-flat (buffer-substring start (point-max)))))
        (delete-region start (point-max))
        (unless (string-empty-p title)
          (insert (make-string level ?*) " " title)
          (warcel--org-newlines 2)))
      (set-marker start nil)))))

(defun warcel--dom-raw-text (node)
  "Return the text of NODE with its white space preserved."
  (mapconcat (lambda (child)
               (cond ((stringp child) child)
                     ((eq (dom-tag child) 'br) "\n")
                     (t (warcel--dom-raw-text child))))
             (dom-children node)
             ""))

(defun warcel--org-pre (node)
  "Insert the preformatted NODE as an example block."
  (if warcel--org-inline
      (warcel--org-children node)
    (let ((text (string-trim-right
                 (string-remove-prefix
                  "\n" (string-replace "\r" "" (warcel--dom-raw-text node))))))
      (unless (string-blank-p text)
        (warcel--org-newlines 2)
        (insert "#+begin_example\n"
                (replace-regexp-in-string "^[ \t]*\\(,*\\(?:\\*\\|#\\+\\)\\)"
                                          ",\\1" text nil nil 1)
                "\n#+end_example")
        (warcel--org-newlines 2)))))

(defun warcel--org-quote (node)
  "Insert the block quote NODE."
  (if (or warcel--org-inline warcel--org-in-quote)
      (warcel--org-block node 2)
    (let ((text (let ((warcel--org-in-quote t))
                  (warcel--org-render (dom-children node)))))
      (unless (string-empty-p text)
        (warcel--org-newlines 2)
        (insert "#+begin_quote\n" text "\n#+end_quote")
        (warcel--org-newlines 2)))))

(defun warcel--attribute-number (node attribute)
  "Return the number in ATTRIBUTE of NODE, or nil."
  (let ((value (dom-attr node attribute)))
    (and value
         (string-match "\\`[ \t]*\\(-?[0-9]+\\)" value)
         (string-to-number (match-string 1 value)))))

(defun warcel--org-list (node)
  "Insert the list NODE."
  (if warcel--org-inline
      (warcel--org-children node)
    (let ((ordered (eq (dom-tag node) 'ol))
          (number (or (warcel--attribute-number node 'start) 1))
          (space (if warcel--org-in-block 1 2)))
      (warcel--org-newlines space)
      (dolist (child (dom-children node))
        (let ((text (cond
                     ((stringp child)
                      (and (string-match-p "[^ \t\n\r]" child)
                           (warcel--org-render (list child))))
                     ((eq (dom-tag child) 'li)
                      (when-let* ((value (warcel--attribute-number child 'value)))
                        (setq number value))
                      (warcel--org-render (dom-children child)))
                     (t (warcel--org-render (list child))))))
          (when (and text (not (string-empty-p text)))
            (let ((bullet (if ordered (format "%d. " number) "- ")))
              (warcel--org-newlines 1)
              (insert bullet (warcel--org-indent text (length bullet))))
            (setq number (1+ number)))))
      (warcel--org-newlines space))))

(defun warcel--description-items (node)
  "Return the term and description elements of the list NODE."
  (let (items)
    (dolist (child (dom-children node))
      (when (consp child)
        (if (eq (dom-tag child) 'div)
            (dolist (item (warcel--description-items child))
              (push item items))
          (push child items))))
    (nreverse items)))

(defun warcel--org-description-item (terms text)
  "Insert a description list item for TERMS described by TEXT."
  (let ((terms (seq-remove #'string-empty-p terms)))
    (when (or terms (not (string-empty-p text)))
      (warcel--org-newlines 1)
      (insert "- "
              (if terms (concat (string-join terms ", ") " :: ") "")
              (warcel--org-indent text 2)))))

(defun warcel--org-description-list (node)
  "Insert the description list NODE."
  (if warcel--org-inline
      (warcel--org-children node)
    (let ((space (if warcel--org-in-block 1 2))
          terms)
      (warcel--org-newlines space)
      (dolist (item (warcel--description-items node))
        (pcase (dom-tag item)
          ('dt (push (warcel--org-flat (warcel--org-render (dom-children item) t))
                     terms))
          ('dd (warcel--org-description-item
                (reverse terms) (warcel--org-render (dom-children item)))
               (setq terms nil))))
      (when terms
        (warcel--org-description-item (reverse terms) ""))
      (warcel--org-newlines space))))

(defun warcel--table-row (row header)
  "Return (HEADER-P . CELLS) for the table ROW.
HEADER non-nil means the row is in the table head."
  (let ((cells (seq-filter (lambda (child)
                             (and (consp child) (memq (dom-tag child) '(td th))))
                           (dom-children row))))
    (cons (or header
              (and cells (seq-every-p (lambda (cell) (eq (dom-tag cell) 'th))
                                      cells)))
          cells)))

(defun warcel--table-rows (table)
  "Return the rows of TABLE as a list of (HEADER-P . CELLS)."
  (let (rows)
    (dolist (child (dom-children table))
      (when (consp child)
        (pcase (dom-tag child)
          ((or 'thead 'tbody 'tfoot)
           (dolist (row (dom-children child))
             (when (and (consp row) (eq (dom-tag row) 'tr))
               (push (warcel--table-row row (eq (dom-tag child) 'thead)) rows))))
          ('tr (push (warcel--table-row child nil) rows)))))
    (nreverse rows)))

(defun warcel--layout-table-p (rows)
  "Return non-nil if the table with ROWS is used for layout, not data."
  (or (< (apply #'max 0 (mapcar (lambda (row) (length (cdr row))) rows)) 2)
      (seq-some
       (lambda (row)
         (seq-some
          (lambda (cell)
            (or (dom-search cell (lambda (node)
                                   (memq (dom-tag node)
                                         '(table ul ol dl pre blockquote hr
                                                 h1 h2 h3 h4 h5 h6))))
                (> (length (warcel--dom-text cell)) 300)))
          (cdr row)))
       rows)))

(defun warcel--org-table (node)
  "Insert the table NODE."
  (let ((rows (warcel--table-rows node)))
    (cond
     (warcel--org-inline
      (warcel--org-children node))
     ((warcel--layout-table-p rows)
      ;; Show the cells of layout tables one after the other.
      (warcel--org-newlines 1)
      (if rows
          (dolist (row rows)
            (dolist (cell (cdr row))
              (warcel--org-block cell 1)))
        (warcel--org-children node))
      (warcel--org-newlines 1))
     (t
      (let ((caption (seq-find (lambda (child)
                                 (and (consp child) (eq (dom-tag child) 'caption)))
                               (dom-children node)))
            (in-head t)
            (head-rows 0)
            (content nil)
            lines)
        (dolist (row rows)
          (when (cdr row)
            (let ((cells (mapcar (lambda (cell)
                                   (string-replace
                                    "|" "¦"
                                    (warcel--org-flat
                                     (warcel--org-render (dom-children cell) t))))
                                 (cdr row))))
              (when (and in-head (not (car row)))
                (setq in-head nil)
                (when (> head-rows 0)
                  (push "|-" lines)))
              (when in-head
                (cl-incf head-rows))
              (when (seq-some (lambda (cell) (not (string-empty-p cell))) cells)
                (setq content t))
              (push (concat "| " (string-join cells " | ") " |") lines))))
        (when content
          (when caption
            (warcel--org-block caption 2))
          (warcel--org-newlines 2)
          (insert (string-join (nreverse lines) "\n"))
          (warcel--org-newlines 2)))))))

(defun warcel--org-node (node)
  "Insert the Org text of the DOM NODE."
  (cond
   ((stringp node)
    (warcel--org-text node))
   ((consp node)
    (pcase (dom-tag node)
      ((or 'p 'address 'figure 'figcaption 'details 'summary 'center
           'fieldset 'legend 'caption)
       (warcel--org-block node 2))
      ((or 'div 'section 'article 'main 'header 'footer 'nav 'aside 'form
           'hgroup 'dialog 'body 'html 'li 'dt 'dd 'tr 'td 'th 'thead 'tbody
           'tfoot)
       (warcel--org-block node 1))
      ((or 'h1 'h2 'h3 'h4 'h5 'h6)
       (warcel--org-heading
        node (string-to-number (substring (symbol-name (dom-tag node)) 1))))
      ((or 'b 'strong) (warcel--org-emphasis node "*"))
      ((or 'i 'em 'cite 'dfn 'var) (warcel--org-emphasis node "/"))
      ((or 'u 'ins) (warcel--org-emphasis node "_"))
      ((or 's 'strike 'del) (warcel--org-emphasis node "+"))
      ((or 'code 'kbd 'samp 'tt) (warcel--org-verbatim node))
      ('a (warcel--org-link node))
      ('br (warcel--org-break))
      ('hr (warcel--org-rule))
      ('pre (warcel--org-pre node))
      ('blockquote (warcel--org-quote node))
      ((or 'ul 'ol 'menu 'dir) (warcel--org-list node))
      ('dl (warcel--org-description-list node))
      ('table (warcel--org-table node))
      ('q (warcel--org-text "“")
          (warcel--org-children node)
          (warcel--org-text "”"))
      ((or 'head 'title) nil)
      (_ (warcel--org-children node))))))

(defun warcel--page-org (page)
  "Return the Org document for PAGE, as returned by `warcel--page'."
  (require 'ol)
  (let* ((max-lisp-eval-depth (max max-lisp-eval-depth 4000))
         (dom (plist-get page :dom))
         (text (let ((warcel--org-base (plist-get page :base))
                     (warcel--org-inline nil)
                     (warcel--org-in-block nil)
                     (warcel--org-in-quote nil)
                     (warcel--org-in-link nil)
                     (warcel--org-markers nil))
                 (with-temp-buffer
                   (warcel--org-node (or (car (dom-by-tag dom 'body)) dom))
                   (warcel--org-tidy)
                   (buffer-string))))
         (date (plist-get page :date)))
    (concat ":PROPERTIES:\n"
            ":WARC-Target-URI: " (plist-get page :uri) "\n"
            (if date (concat ":WARC-Date: " date "\n") "")
            ":END:\n"
            "#+title: " (warcel--org-flat (plist-get page :title)) "\n\n"
            text "\n")))

;;;; Showing records

(defvar-local warcel--archive nil
  "The archive shown in this buffer.")

(defvar-local warcel--record nil
  "The record shown in this buffer.")

(defun warcel--current-archive ()
  "Return the archive of the current buffer."
  (or (and (derived-mode-p 'warcel-eww-mode)
           (plist-get eww-data :warcel-archive))
      warcel--archive
      (user-error "No WARC archive here")))

(defun warcel--current-record ()
  "Return the record at point or shown in the current buffer."
  (or (cond
       ((derived-mode-p 'warcel-mode) (tabulated-list-get-id))
       ((derived-mode-p 'warcel-eww-mode) (plist-get eww-data :warcel-record))
       (t warcel--record))
      (user-error "No WARC record here")))

(defun warcel--short-uri (uri)
  "Return URI without its scheme, for use in buffer names."
  (truncate-string-to-width (replace-regexp-in-string "\\`[a-z]+://" "" uri)
                            60 nil nil "…"))

(defun warcel--web-url-p (url &rest _)
  "Return non-nil if URL is an HTTP or HTTPS URL."
  (string-match-p "\\`https?:" url))

(defun warcel--browse-url (url &rest _)
  "Follow URL from a warcel buffer; for `browse-url-handlers'."
  (warcel--follow-url url))

(defun warcel--browse-live (url)
  "Open URL, which is not in the archive, as `warcel-follow-live-links' says."
  (if (if (eq warcel-follow-live-links 'ask)
          (y-or-n-p (format "%s is not in the archive; open it live? " url))
        warcel-follow-live-links)
      (let ((browse-url-handlers (default-value 'browse-url-handlers))
            (browse-url-browser-function
             (default-value 'browse-url-browser-function)))
        (browse-url url))
    (message "Not in the archive: %s" url)))

(defun warcel--follow-url (url)
  "Show the archived copy of URL, or handle it as a live link."
  (let* ((archive (warcel--current-archive))
         (found (warcel--lookup archive url))
         (target (and found (warcel--resolve archive found)))
         (fragment (url-target (url-generic-parse-url url))))
    (cond
     ((not target)
      (warcel--browse-live url))
     ((derived-mode-p 'warcel-eww-mode)
      (if (and fragment (eq target (plist-get eww-data :warcel-record)))
          (warcel--eww-jump fragment)
        (warcel--show-eww archive target fragment)))
     ((eq target warcel--record)
      (message "The link points to this page"))
     (t (warcel--display archive target 'org)))))

(defun warcel-follow-link ()
  "Follow the link at point, preferring pages in the archive."
  (interactive)
  (cond
   ((derived-mode-p 'org-mode) (org-open-at-point))
   ((thing-at-point 'url) (warcel--follow-url (thing-at-point 'url)))
   (t (user-error "No link at point"))))

(defvar-keymap warcel-view-mode-map
  :doc "Keymap of `warcel-view-mode'."
  "q" #'quit-window
  "^" #'warcel-list-records
  "R" #'warcel-toggle-readable
  "RET" #'warcel-follow-link
  "o" #'warcel-show-org
  "e" #'warcel-show-eww
  "v" #'warcel-view-source
  "s" #'warcel-save-payload)

(define-minor-mode warcel-view-mode
  "Minor mode of buffers showing a record of a WARC archive.

\\{warcel-view-mode-map}"
  :lighter " WARC"
  :keymap warcel-view-mode-map)

(defun warcel--setup-view (archive record)
  "Set up the current buffer to show RECORD of ARCHIVE."
  (setq warcel--archive archive
        warcel--record record
        buffer-read-only t
        buffer-undo-list t)
  (setq-local browse-url-handlers
              (cons (cons #'warcel--web-url-p #'warcel--browse-url)
                    browse-url-handlers))
  (warcel-view-mode 1)
  (set-buffer-modified-p nil)
  (goto-char (point-min)))

(defun warcel--record-buffer (record)
  "Return the buffer for showing RECORD."
  (get-buffer-create
   (format "*warcel: %s*" (warcel--short-uri (or (warcel-record-uri record) "")))))

(defvar-local warcel--readable 'default
  "Whether the Org view in this buffer shows only the main content of pages.
`default' means as `warcel-readable' says, until the view is toggled.")
;; Rendering sets `org-mode' again, which must not reset it.
(put 'warcel--readable 'permanent-local t)

(defvar-local warcel--main-content-shown nil
  "Non-nil when the Org view in this buffer shows only the main content.")

(defun warcel--readable-p ()
  "Return non-nil if the Org view in this buffer shows main content only."
  (if (eq warcel--readable 'default) warcel-readable warcel--readable))

(defun warcel-toggle-readable ()
  "Switch the page shown between its main content and the whole page.
The main content is the title and text of the article, without the
menus, headers, footers and sidebars around it (see `warcel-readable')."
  (interactive)
  (unless (and warcel--record (derived-mode-p 'org-mode))
    (user-error "Only pages shown as Org have a main content view"))
  (setq warcel--readable (not (warcel--readable-p)))
  (warcel--render-org (warcel--current-archive) warcel--record)
  (message (cond (warcel--main-content-shown "Showing the main content of the page")
                 (warcel--readable "No main content found: showing the whole page")
                 (t "Showing the whole page"))))

(defun warcel--render-org (archive record)
  "Show the HTML page in RECORD of ARCHIVE as Org in the current buffer."
  (require 'org)
  (require 'org-table)
  (let* ((page (warcel--page archive record (warcel--readable-p)))
         (text (warcel--page-org page))
         (inhibit-read-only t))
    (erase-buffer)
    (insert text)
    (let ((org-startup-folded 'showeverything)
          (org-startup-with-inline-images nil)
          (org-startup-with-latex-preview nil))
      (org-mode))
    (setq warcel--main-content-shown (plist-get page :readable))
    (with-demoted-errors "Aligning tables: %S"
      (org-table-map-tables #'org-table-align t)))
  (visual-line-mode 1)
  (warcel--setup-view archive record))

(defun warcel--show-org (archive record)
  "Show the HTML page in RECORD of ARCHIVE as an Org document."
  (let ((buffer (warcel--record-buffer record)))
    (with-current-buffer buffer
      (warcel--render-org archive record))
    (pop-to-buffer-same-window buffer)))

(defun warcel--text-mode (mime)
  "Return the major mode for showing text of the media type MIME."
  (cond
   ((string-match-p "css" mime) #'css-mode)
   ((string-match-p "json" mime)
    (if (fboundp 'js-json-mode) #'js-json-mode #'js-mode))
   ((string-match-p "javascript\\|ecmascript" mime) #'js-mode)
   ((string-match-p "xml" mime) #'nxml-mode)
   ((string-prefix-p "text/plain" mime) #'text-mode)
   (t #'fundamental-mode)))

(defun warcel--render-text (archive record)
  "Show the text payload of RECORD in ARCHIVE in the current buffer."
  (let ((text (warcel--payload-text archive record))
        (inhibit-read-only t))
    (erase-buffer)
    (insert text)
    (funcall (warcel--text-mode (warcel-record-mime record))))
  (warcel--setup-view archive record))

(defun warcel--show-text (archive record)
  "Show the text payload of RECORD in ARCHIVE."
  (let ((buffer (warcel--record-buffer record)))
    (with-current-buffer buffer
      (warcel--render-text archive record))
    (pop-to-buffer-same-window buffer)))

(defun warcel--text-mime-p (mime)
  "Return non-nil if the media type MIME is text."
  (or (string-prefix-p "text/" mime)
      (string-match-p "json\\|xml\\|javascript\\|ecmascript\\|warc-fields\\|application/http\\|x-www-form-urlencoded"
                      mime)))

(defun warcel--media-mime-p (mime)
  "Return non-nil if the media type MIME is an image, audio or video."
  (string-match-p "\\`\\(?:image\\|audio\\|video\\)/" mime))

(defun warcel--showable-p (archive record)
  "Return non-nil if RECORD of ARCHIVE can be shown as a page or as text."
  (and (member (warcel-record-type record) '("response" "resource" "conversion"))
       (ignore-errors
         (or (warcel--html-p archive record)
             (let ((mime (warcel-record-mime record)))
               (and (warcel--text-mime-p mime)
                    (not (warcel--media-mime-p mime))))))))

(defun warcel--page-kind (record)
  "Return what RECORD is when looking for the page of an archive.
Return `page' for successful HTML pages, `error' for HTML error pages,
`text' for other text and nil for anything else.  Only the content
type is looked at."
  (let ((mime (warcel-record-mime record))
        (status (warcel-record-http-status record)))
    (cond
     ((not (member (warcel-record-type record) '("response" "resource" "conversion")))
      nil)
     ((or (member mime '("text/html" "application/xhtml+xml"))
          (warcel--redirect-location record))
      (if (and status (>= status 400)) 'error 'page))
     ((and (warcel--text-mime-p mime) (not (warcel--media-mime-p mime)))
      'text))))

(defun warcel--single-page (archive)
  "Return the record of the only page in ARCHIVE, or nil.
The pages are the HTML pages that listed records lead to: redirects
and revisits count as the page they lead to, while the images,
styles and scripts that come with a page do not count.  Error pages
count when the archive has no other page, and other text when it has
no page at all.  Return nil if the page found cannot be shown."
  (let ((found (list (cons 'page nil) (cons 'error nil) (cons 'text nil))))
    ;; FOUND maps each kind to nil, a record, or `several'.
    (catch 'several
      (seq-doseq (record (warcel-archive-records archive))
        (when (member (warcel-record-type record) warcel-record-types)
          (let* ((target (warcel--resolve archive record))
                 (slot (assq (warcel--page-kind target) found)))
            (when slot
              (cond
               ((not (cdr slot)) (setcdr slot target))
               ((not (eq (cdr slot) target))
                (setcdr slot 'several)
                (when (eq (car slot) 'page)
                  (throw 'several nil)))))))))
    (let ((page (seq-some (lambda (slot) (cdr slot)) found)))
      (and (warcel-record-p page)
           (warcel--showable-p archive page)
           page))))

(defun warcel--render (archive record view)
  "Show RECORD of ARCHIVE in the current buffer, HTML pages as VIEW.
RECORD must satisfy `warcel--showable-p'."
  (cond
   ((not (warcel--html-p archive record))
    (warcel--render-text archive record))
   ((eq view 'eww)
    (warcel-eww-mode)
    (warcel--eww-render archive record nil))
   (t (warcel--render-org archive record))))

(defun warcel--display (archive record view)
  "Show RECORD of ARCHIVE; HTML pages are shown as VIEW, org or eww."
  (let* ((target (warcel--resolve archive record))
         (type (warcel-record-type target))
         (mime (warcel-record-mime target)))
    (cond
     ((not (member type '("response" "resource" "revisit" "conversion")))
      (warcel-view-source archive target))
     ((equal type "revisit")
      (user-error "The record this revisit refers to is not in the archive"))
     ((warcel--html-p archive target)
      (if (eq view 'eww)
          (warcel--show-eww archive target)
        (warcel--show-org archive target)))
     ((warcel--media-mime-p mime)
      (user-error "Not showing %s content; press `s' to save it" mime))
     ((warcel--text-mime-p mime)
      (warcel--show-text archive target))
     (t
      (user-error "Not a text payload (%s); press `s' to save it"
                  (if (string-empty-p mime) "unknown type" mime))))
    (unless (eq target record)
      (message "Showing %s" (or (warcel-record-uri target) "the original record")))))

(defun warcel-open-record ()
  "Show the record at point, with `warcel-default-view' for HTML pages."
  (interactive)
  (warcel--display (warcel--current-archive) (warcel--current-record)
                   warcel-default-view))

(defun warcel-show-org ()
  "Show the record at point, or of this buffer, as an Org document."
  (interactive)
  (warcel--display (warcel--current-archive) (warcel--current-record) 'org))

(defun warcel-show-eww ()
  "Show the record at point, or of this buffer, with EWW."
  (interactive)
  (warcel--display (warcel--current-archive) (warcel--current-record) 'eww))

(defun warcel-view-source (&optional archive record)
  "Show the headers and payload of RECORD of ARCHIVE as stored.
Interactively, show the record at point or of the current buffer."
  (interactive)
  (let* ((archive (or archive (warcel--current-archive)))
         (record (or record (warcel--current-record)))
         (mime (warcel-record-mime record))
         (buffer (get-buffer-create
                  (format "*warcel source: %s*"
                          (warcel--short-uri
                           (or (warcel-record-uri record)
                               (format "record %d" (warcel-record-index record))))))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (fundamental-mode)
        (dolist (field (warcel-record-headers record))
          (insert (propertize (car field) 'face 'bold) ": " (cdr field) "\n"))
        (insert "\n")
        (if (warcel-record-http-status record)
            (pcase-let ((`(,start ,headers ,_)
                         (warcel--split-http (warcel--record-block archive record))))
              (insert (propertize start 'face 'bold) "\n")
              (dolist (field headers)
                (insert (propertize (car field) 'face 'bold) ": " (cdr field) "\n"))
              (insert "\n")
              (if (and (warcel--text-mime-p mime)
                       (not (string-match-p "\\`\\(?:image\\|audio\\|video\\)/" mime)))
                  (insert (condition-case err
                              (warcel--payload-text archive record)
                            (user-error (format "[%s]" (error-message-string err)))))
                (insert (format "[%s of %s data]"
                                (file-size-human-readable (warcel--payload-size record))
                                (if (string-empty-p mime) "unknown" mime)))))
          (if (or (warcel--text-mime-p mime) (string-empty-p mime))
              (insert (decode-coding-string (warcel--record-block archive record)
                                            (warcel--charset
                                             (warcel-record-content-type record) "")))
            (insert (format "[%s of %s data]"
                            (file-size-human-readable (warcel-record-length record))
                            mime)))))
      (warcel--setup-view archive record))
    (pop-to-buffer-same-window buffer)))

(defun warcel--payload-file-name (record)
  "Return a file name for saving the payload of RECORD."
  (let* ((uri (or (warcel-record-uri record) ""))
         (path (car (url-path-and-query (url-generic-parse-url uri))))
         (name (and path (file-name-nondirectory (url-unhex-string path)))))
    (if (or (not name) (string-empty-p name))
        (if (equal (warcel-record-mime record) "text/html") "index.html" "payload")
      (string-replace "/" "_" name))))

(defun warcel-save-payload (archive record file)
  "Save the payload of RECORD in ARCHIVE to FILE.
Interactively, save the record at point or of the current buffer."
  (interactive
   (let ((archive (warcel--current-archive))
         (record (warcel--current-record)))
     (list archive record
           (read-file-name "Save payload to: " nil nil nil
                           (warcel--payload-file-name record)))))
  (setq file (expand-file-name file))
  (when (file-directory-p file)
    (setq file (expand-file-name (warcel--payload-file-name record) file)))
  (when (and (file-exists-p file)
             (not (y-or-n-p (format "Overwrite %s? " file))))
    (user-error "Payload not saved"))
  (let ((payload (warcel--record-payload archive
                                         (warcel--resolve archive record t))))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert payload)
      (let ((coding-system-for-write 'no-conversion))
        (write-region nil nil file)))
    (message "Saved %s (%s)" file (file-size-human-readable (length payload)))))

;;;; EWW

(defvar-keymap warcel-eww-mode-map
  :doc "Keymap of `warcel-eww-mode'."
  "<remap> <eww-follow-link>" #'warcel-eww-follow-link
  "<remap> <eww-reload>" #'warcel-eww-reload
  "<remap> <eww-view-source>" #'warcel-view-source
  "^" #'warcel-list-records
  "o" #'warcel-show-org)

(define-derived-mode warcel-eww-mode eww-mode "warcel"
  "Major mode for archived pages rendered by EWW.
Links to pages in the archive show the archived page; reloading
shows the page from the archive again.

\\{warcel-eww-mode-map}"
  :interactive nil
  (setq-local desktop-save-buffer nil))

(defun warcel--eww-render (archive record fragment)
  "Render the HTML page in RECORD of ARCHIVE in the current buffer.
Move to the element named FRAGMENT, if any."
  (let ((page (warcel--page archive record)))
    (setq eww-data (list :title (plist-get page :title)
                         :url (plist-get page :uri)
                         ;; `eww-readable' parses this again.
                         :source (with-temp-buffer
                                   (dom-print (plist-get page :dom))
                                   (buffer-string))
                         :warcel-archive archive
                         :warcel-record record))
    (let ((shr-inhibit-images t)
          (shr-target-id fragment))
      (if (fboundp 'eww-display-document)
          (eww-display-document
           (list 'base (list (cons 'href (plist-get page :base)))
                 (plist-get page :dom)))
        (let ((inhibit-read-only t)
              (shr-base (shr-parse-base (plist-get page :base))))
          (erase-buffer)
          (shr-insert-document (plist-get page :dom))
          (goto-char (point-min)))))
    (eww-update-header-line-format)))

(defun warcel--show-eww (archive record &optional fragment)
  "Show the HTML page in RECORD of ARCHIVE with EWW.
Move to the element named FRAGMENT, if any.  Pages shown from an EWW
buffer replace its page, so that they are in its history."
  (let ((buffer (if (derived-mode-p 'warcel-eww-mode)
                    (current-buffer)
                  (get-buffer-create "*warcel-eww*"))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'warcel-eww-mode)
        (warcel-eww-mode))
      (when (plist-get eww-data :url)
        (eww-save-history))
      (when (fboundp 'eww--before-browse)
        (eww--before-browse))
      (setq eww-history-position 0)
      (warcel--eww-render archive record fragment))
    (pop-to-buffer-same-window buffer)))

(defun warcel--eww-jump (target)
  "Move to the element named TARGET on the current EWW page."
  (let ((match (save-excursion
                 (goto-char (point-min))
                 (text-property-search-forward 'shr-target-id target #'member))))
    (if match
        (goto-char (prop-match-beginning match))
      (message "No element named %s on this page" target))))

(defun warcel-eww-follow-link (&optional external mouse-event)
  "Follow the link at point, or at MOUSE-EVENT, preferring the archive.
With a \\[universal-argument] prefix EXTERNAL, open the live page with
an external browser."
  (interactive (list current-prefix-arg last-nonmenu-event) warcel-eww-mode)
  (when (mouse-event-p mouse-event)
    (mouse-set-point mouse-event))
  (let ((url (get-text-property (point) 'shr-url)))
    (cond
     ((not url) (message "No link under point"))
     (external (eww-browse-with-external-browser url))
     (t (warcel--follow-url url)))))

(defun warcel-eww-reload ()
  "Show the current page from the archive again."
  (interactive nil warcel-eww-mode)
  (let ((point (point)))
    (warcel--eww-render (plist-get eww-data :warcel-archive)
                        (plist-get eww-data :warcel-record)
                        nil)
    (goto-char (min point (point-max)))))

;;;; Record list

(defvar-local warcel--show-all nil
  "Non-nil means list records of all types.")

(defvar-local warcel--filter nil
  "Regexp that listed URIs or content types must match, or nil.")

(defvar-keymap warcel-mode-map
  :doc "Keymap of `warcel-mode'."
  :parent tabulated-list-mode-map
  "RET" #'warcel-open-record
  "o" #'warcel-show-org
  "e" #'warcel-show-eww
  "v" #'warcel-view-source
  "s" #'warcel-save-payload
  "a" #'warcel-toggle-all-records
  "/" #'warcel-filter)

(defun warcel--sort-by-index (a b)
  "Return non-nil if the record of entry A comes before that of entry B."
  (< (warcel-record-index (car a)) (warcel-record-index (car b))))

(defun warcel--sort-by-size (a b)
  "Return non-nil if the payload of entry A is smaller than that of entry B."
  (< (warcel--payload-size (car a)) (warcel--payload-size (car b))))

;;;###autoload
(define-derived-mode warcel-mode tabulated-list-mode "WARC"
  "Major mode for listing the records of a WARC file.

\\{warcel-mode-map}"
  (setq tabulated-list-format
        [("#" 5 warcel--sort-by-index :right-align t)
         ("Type" 8 t)
         ("Status" 6 t :right-align t)
         ("Content type" 24 t)
         ("Size" 7 warcel--sort-by-size :right-align t)
         ("Date" 19 t)
         ("URI" 40 t)])
  (setq tabulated-list-padding 1)
  (setq-local revert-buffer-function #'warcel--revert)
  (tabulated-list-init-header)
  (when buffer-file-name
    (warcel--take-over-visited-buffer)))

(defun warcel--forget-visited-file ()
  "Empty the current buffer and stop it from visiting its WARC file.
The buffer then shows what was read from the file, which must never
be saved over it.  Return the file name."
  (let ((file buffer-file-name)
        (inhibit-read-only t))
    (erase-buffer)
    (set-buffer-multibyte t)
    (set-buffer-modified-p nil)
    (setq buffer-file-name nil
          buffer-file-truename nil
          buffer-undo-list t
          default-directory (file-name-directory file))
    file))

(defun warcel--take-over-visited-buffer ()
  "Replace the contents of the WARC file visited here with its records."
  (let* ((archive (warcel-read-archive buffer-file-name))
         (file (warcel--forget-visited-file)))
    (setq-local list-buffers-directory file)
    (setq warcel--archive archive)
    (warcel--refresh)))

(defun warcel--list-archive (archive)
  "List the records of ARCHIVE in the current buffer."
  (warcel-mode)
  (setq warcel--archive archive)
  (warcel--refresh))

;;;###autoload
(defun warcel-visit ()
  "Show the WARC file visited in the current buffer.
If the archive holds a single page, show it as `warcel-default-view'
says; otherwise list the records of the archive.  This function is
meant for `auto-mode-alist'.  New and empty files are left alone."
  (if (or (not (file-exists-p buffer-file-name))
          (zerop (file-attribute-size (file-attributes buffer-file-name))))
      (fundamental-mode)
    (warcel--visit-archive)))

(defun warcel--visit-archive ()
  "Show the WARC file visited in the current buffer, as `warcel-visit' does."
  (let* ((archive (warcel-read-archive buffer-file-name))
         (page (warcel--single-page archive))
         (file (warcel--forget-visited-file)))
    (if (not page)
        (warcel--list-archive archive)
      (condition-case err
          (warcel--render archive page warcel-default-view)
        (error
         (message "Cannot show %s: %s" (warcel-record-uri page)
                  (error-message-string err))
         (warcel--list-archive archive))))
    ;; The buffer shows archived web content: local variables in it
    ;; must not take effect.
    (setq-local local-enable-local-variables nil)
    (setq-local list-buffers-directory file)))

(defun warcel--status-face (status)
  "Return the face for the HTTP STATUS."
  (cond ((>= status 400) 'error)
        ((>= status 300) 'shadow)))

(defun warcel--listed-p (record)
  "Return non-nil if RECORD belongs in the list."
  (and (or warcel--show-all
           (member (warcel-record-type record) warcel-record-types))
       (or (not warcel--filter)
           (let ((case-fold-search t))
             (or (string-match-p warcel--filter (or (warcel-record-uri record) ""))
                 (string-match-p warcel--filter (warcel-record-mime record)))))))

(defun warcel--entry (record)
  "Return the list entry of RECORD."
  (let ((status (warcel-record-http-status record))
        (date (or (warcel-record-header record "WARC-Date") "")))
    (list record
          (vector (number-to-string (warcel-record-index record))
                  (warcel-record-type record)
                  (if status
                      (propertize (number-to-string status)
                                  'face (warcel--status-face status))
                    "")
                  (warcel-record-mime record)
                  (file-size-human-readable (warcel--payload-size record))
                  (if (>= (length date) 19)
                      (concat (substring date 0 10) " " (substring date 11 19))
                    date)
                  (or (warcel-record-uri record)
                      (warcel-record-header record "WARC-Filename")
                      "")))))

(defun warcel--refresh ()
  "List the records of the archive of the current buffer."
  (let ((records (warcel-archive-records warcel--archive))
        entries)
    (seq-doseq (record records)
      (when (warcel--listed-p record)
        (push (warcel--entry record) entries)))
    (setq tabulated-list-entries (nreverse entries))
    (setq mode-line-process
          (concat (format " %d/%d" (length tabulated-list-entries) (length records))
                  (and warcel--filter (format " /%s/" warcel--filter))))
    (tabulated-list-print t)
    (when (and (null tabulated-list-entries) (> (length records) 0))
      (message (substitute-command-keys
                "\\<warcel-mode-map>No records to list; \\[warcel-toggle-all-records] lists all records")))))

(defun warcel--revert (&rest _)
  "Read the archive of the current buffer again and list its records."
  (setq warcel--archive (warcel-read-archive (warcel-archive-file warcel--archive)))
  (warcel--refresh))

(defun warcel-toggle-all-records ()
  "Toggle between listing all records and those in `warcel-record-types'."
  (interactive nil warcel-mode)
  (setq warcel--show-all (not warcel--show-all))
  (warcel--refresh))

(defun warcel-filter (regexp)
  "Only list records whose URI or content type matches REGEXP.
An empty REGEXP lists all records again."
  (interactive (list (read-regexp "List records matching (empty for all)"))
               warcel-mode)
  (setq warcel--filter (and regexp (not (string-empty-p regexp)) regexp))
  (warcel--refresh))

(defun warcel--listing-buffer (file)
  "Return the buffer that lists the records of FILE, or nil."
  (seq-find (lambda (buffer)
              (with-current-buffer buffer
                (and (derived-mode-p 'warcel-mode)
                     warcel--archive
                     (equal (warcel-archive-file warcel--archive) file))))
            (buffer-list)))

(defun warcel--list-buffer (archive)
  "Return the buffer that lists the records of ARCHIVE, creating it if needed."
  (let ((file (warcel-archive-file archive)))
    (or (warcel--listing-buffer file)
        (let ((buffer (generate-new-buffer
                       (format "*warcel %s*" (file-name-nondirectory file)))))
          (with-current-buffer buffer
            (warcel--list-archive archive)
            (setq default-directory (file-name-directory file)))
          buffer))))

(defun warcel--goto-uri (uri)
  "Move to the last listed page of URI, or to the first record."
  (let ((target (warcel--normalize-uri uri))
        found)
    (goto-char (point-max))
    (while (and (not found) (zerop (forward-line -1)))
      (let ((record (tabulated-list-get-id)))
        (setq found (and record
                         (member (warcel-record-type record)
                                 '("response" "resource" "conversion"))
                         (equal (warcel--normalize-uri (or (warcel-record-uri record) ""))
                                target)))))
    (unless found
      (goto-char (point-min)))))

(defun warcel--goto-record (record)
  "Move to the listed RECORD, or to the last listed page of its URI."
  (goto-char (point-min))
  (while (and (not (eobp)) (not (eq (tabulated-list-get-id) record)))
    (forward-line 1))
  (when (eobp)
    (warcel--goto-uri (or (warcel-record-uri record) ""))))

(defun warcel-list-records ()
  "List the records of the archive shown in the current buffer."
  (interactive)
  (let ((archive (warcel--current-archive))
        (record (if (derived-mode-p 'warcel-eww-mode)
                    (plist-get eww-data :warcel-record)
                  warcel--record)))
    (pop-to-buffer-same-window (warcel--list-buffer archive))
    (when record
      (warcel--goto-record record))))

;;;###autoload
(defun warcel-open (file &optional list)
  "Show the WARC FILE.
If the archive holds a single page, show it as `warcel-default-view'
says.  Otherwise, or if LIST is non-nil, list the records of the
archive.  Interactively, LIST is the prefix argument."
  (interactive
   (list (read-file-name "WARC file: " nil nil t
                         (and (derived-mode-p 'dired-mode)
                              (dired-get-filename 'no-dir t)))
         current-prefix-arg))
  (let* ((file (expand-file-name file))
         (listing (warcel--listing-buffer file))
         (archive (if listing
                      (buffer-local-value 'warcel--archive listing)
                    (warcel-read-archive file)))
         (page (and (not list) (warcel--single-page archive))))
    (if (not page)
        (pop-to-buffer-same-window (warcel--list-buffer archive))
      (warcel--display archive page warcel-default-view)
      (message (substitute-command-keys
                (if warcel--main-content-shown
                    "\\<warcel-view-mode-map>Main content of the page: \\[warcel-toggle-readable] shows the whole page, \\[warcel-list-records] lists the records"
                  "\\<warcel-view-mode-map>Single page archive: \\[warcel-list-records] lists its records"))))))

;;;; Compressing

;; Emacs can inflate gzip data but has no way to deflate it, so this is
;; a small DEFLATE encoder (RFC 1951).  Repeated strings are found with
;; hash chains, and each block of the result is written with fixed or
;; dynamic Huffman codes, or stored, whichever is smallest.

(defconst warcel--length-bases
  [3 4 5 6 7 8 9 10 11 13 15 17 19 23 27 31 35 43 51 59 67 83 99 115 131
   163 195 227 258]
  "Smallest match length of each length code, from code 257.")

(defconst warcel--length-extra
  [0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0]
  "Number of extra bits of each length code, from code 257.")

(defconst warcel--distance-bases
  [1 2 3 4 5 7 9 13 17 25 33 49 65 97 129 193 257 385 513 769 1025 1537
   2049 3073 4097 6145 8193 12289 16385 24577]
  "Smallest distance of each distance code.")

(defconst warcel--distance-extra
  [0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13]
  "Number of extra bits of each distance code.")

(defconst warcel--code-length-order
  [16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15]
  "Order in which the lengths of the code length code are written.")

(defconst warcel--lz-chain 32
  "Number of earlier occurrences examined to find a match.")

(defconst warcel--lz-nice 128
  "Length of a match good enough to stop looking for a longer one.")

(defconst warcel--lz-insert 32
  "Length of the longest match whose positions are all remembered.")

(defconst warcel--block-symbols 16384
  "Number of symbols in a compressed block.")

(defvar warcel--length-codes nil
  "Vector of the length code (from 0 for 257) of each match length.")

(defvar warcel--distance-codes nil
  "Vector of the distance code of each distance.")

(defvar warcel--fixed-codes nil
  "Fixed Huffman codes, as (LENGTHS CODES DISTANCE-LENGTHS DISTANCE-CODES).")

(defvar warcel--crc-table nil
  "Table used to compute CRC-32 checksums.")

(defvar warcel--bits 0
  "Bits not yet written, the first ones lowest.")

(defvar warcel--bit-count 0
  "Number of bits in `warcel--bits'.")

(defun warcel--reverse-bits (code length)
  "Return the LENGTH lowest bits of CODE in reverse order."
  (let ((result 0))
    (dotimes (_ length)
      (setq result (logior (ash result 1) (logand code 1))
            code (ash code -1)))
    result))

(defun warcel--huffman-codes (lengths)
  "Return the canonical Huffman codes for the code LENGTHS.
The codes are bit-reversed, as they are written lowest bit first."
  (let* ((max (seq-max lengths))
         (counts (make-vector (1+ max) 0))
         (next (make-vector (1+ max) 0))
         (codes (make-vector (length lengths) 0))
         (code 0))
    (seq-doseq (length lengths)
      (when (> length 0)
        (cl-incf (aref counts length))))
    (cl-loop for bits from 1 to max
             do (setq code (ash (+ code (aref counts (1- bits))) 1))
             (aset next bits code))
    (dotimes (symbol (length lengths))
      (let ((length (aref lengths symbol)))
        (when (> length 0)
          (aset codes symbol (warcel--reverse-bits (aref next length) length))
          (cl-incf (aref next length)))))
    codes))

(defun warcel--tree-depths (freqs symbols)
  "Return the depths of SYMBOLS in a Huffman tree for the frequencies FREQS."
  (let ((depths (make-vector (length freqs) 0))
        ;; Each node is (WEIGHT . SYMBOLS-BELOW), lightest first.
        (queue (sort (mapcar (lambda (symbol)
                               (list (aref freqs symbol) symbol))
                             symbols)
                     #'car-less-than-car)))
    (while (cdr queue)
      (let* ((a (pop queue))
             (b (pop queue))
             (node (cons (+ (car a) (car b)) (append (cdr a) (cdr b)))))
        (dolist (symbol (cdr node))
          (cl-incf (aref depths symbol)))
        (if (or (not queue) (< (car node) (caar queue)))
            (push node queue)
          (let ((tail queue))
            (while (and (cdr tail) (<= (caadr tail) (car node)))
              (setq tail (cdr tail)))
            (setcdr tail (cons node (cdr tail)))))))
    depths))

(defun warcel--huffman-lengths (freqs limit)
  "Return Huffman code lengths of at most LIMIT bits for the frequencies FREQS.
At least two symbols get a code, so that the code is complete."
  (let ((freqs (copy-sequence freqs))
        (symbols nil)
        lengths)
    (dotimes (symbol (length freqs))
      (when (> (aref freqs symbol) 0)
        (push symbol symbols)))
    (let ((symbol 0))
      (while (< (length symbols) 2)
        (unless (memq symbol symbols)
          (aset freqs symbol 1)
          (push symbol symbols))
        (setq symbol (1+ symbol))))
    (while (> (seq-max (setq lengths (warcel--tree-depths freqs symbols))) limit)
      ;; Flatten the tree by making rare symbols less rare.
      (dolist (symbol symbols)
        (aset freqs symbol (max 1 (ash (aref freqs symbol) -1)))))
    lengths))

(defun warcel--deflate-tables ()
  "Make the tables used to compress data, unless they exist."
  (unless warcel--crc-table
    (let ((length-codes (make-vector 259 0))
          (distance-codes (make-vector 32769 0))
          (fixed (make-vector 288 8))
          (crc-table (make-vector 256 0)))
      (dotimes (code 29)
        (let ((base (aref warcel--length-bases code)))
          (dotimes (i (ash 1 (aref warcel--length-extra code)))
            (when (<= (+ base i) 258)
              (aset length-codes (+ base i) code)))))
      ;; 258 has a code of its own, though it is also 227 plus 31.
      (aset length-codes 258 28)
      (dotimes (code 30)
        (let ((base (aref warcel--distance-bases code)))
          (dotimes (i (ash 1 (aref warcel--distance-extra code)))
            (aset distance-codes (+ base i) code))))
      (cl-loop for symbol from 144 to 255 do (aset fixed symbol 9))
      (cl-loop for symbol from 256 to 279 do (aset fixed symbol 7))
      (dotimes (n 256)
        (let ((c n))
          (dotimes (_ 8)
            (setq c (if (= 1 (logand c 1))
                        (logxor #xedb88320 (ash c -1))
                      (ash c -1))))
          (aset crc-table n c)))
      (setq warcel--length-codes length-codes
            warcel--distance-codes distance-codes
            warcel--fixed-codes (let ((distances (make-vector 30 5)))
                                  (list fixed (warcel--huffman-codes fixed)
                                        distances (warcel--huffman-codes distances)))
            warcel--crc-table crc-table))))

(defun warcel--crc32 (data &optional crc)
  "Return the CRC-32 of the unibyte string DATA.
CRC is the CRC-32 of data that came before DATA, if any."
  (let ((table warcel--crc-table)
        (crc (logxor (or crc 0) #xffffffff)))
    (dotimes (i (length data))
      (setq crc (logxor (aref table (logand (logxor crc (aref data i)) 255))
                        (ash crc -8))))
    (logxor crc #xffffffff)))

(defsubst warcel--lz-hash (data i)
  "Return the hash of the three bytes at I in DATA."
  (logand (logxor (ash (aref data i) 10) (ash (aref data (1+ i)) 5)
                  (aref data (+ i 2)))
          32767))

(defun warcel--lz77 (data)
  "Return DATA, a unibyte string, as LZ77 symbols.
The result is (SYMBOLS . COUNT), where the first COUNT elements of
the vector SYMBOLS are bytes for literals, or LENGTH << 16 | DISTANCE
for matches."
  (let* ((size (length data))
         (head (make-vector 32768 -1))
         (prev (make-vector (min 32768 (max size 1)) -1))
         (symbols (make-vector (max size 1) 0))
         (count 0)
         (i 0))
    (while (< i size)
      (let ((best 0)
            (distance 0))
        (when (< (+ i 2) size)
          (let* ((hash (warcel--lz-hash data i))
                 (candidate (aref head hash))
                 (oldest (- i 32768))
                 (longest (min 258 (- size i)))
                 (chain warcel--lz-chain))
            (while (and (> candidate oldest) (>= candidate 0) (> chain 0))
              ;; Check the byte that would make the match longer first.
              (when (= (aref data (+ candidate best)) (aref data (+ i best)))
                (let ((length 0))
                  (while (and (< length longest)
                              (= (aref data (+ candidate length))
                                 (aref data (+ i length))))
                    (setq length (1+ length)))
                  (when (> length best)
                    (setq best length
                          distance (- i candidate)))))
              (if (>= best (min longest warcel--lz-nice))
                  (setq chain 0)
                (let ((next (aref prev (logand candidate 32767))))
                  (setq candidate (if (< next candidate) next -1)
                        chain (1- chain)))))
            (aset prev (logand i 32767) (aref head hash))
            (aset head hash i)))
        ;; Distant short matches take more room than their bytes.
        (when (and (= best 3) (> distance 4096))
          (setq best 0))
        (if (< best 3)
            (progn
              (aset symbols count (aref data i))
              (setq i (1+ i)))
          (aset symbols count (logior (ash best 16) distance))
          (when (<= best warcel--lz-insert)
            (let ((j (1+ i))
                  (end (min (+ i best) (- size 2))))
              (while (< j end)
                (let ((hash (warcel--lz-hash data j)))
                  (aset prev (logand j 32767) (aref head hash))
                  (aset head hash j))
                (setq j (1+ j)))))
          (setq i (+ i best))))
      (setq count (1+ count)))
    (cons symbols count)))

(defsubst warcel--put-bits (value count)
  "Write the COUNT lowest bits of VALUE at point, lowest first."
  (setq warcel--bits (logior warcel--bits (ash value warcel--bit-count))
        warcel--bit-count (+ warcel--bit-count count))
  (while (>= warcel--bit-count 8)
    (insert (logand warcel--bits 255))
    (setq warcel--bits (ash warcel--bits -8)
          warcel--bit-count (- warcel--bit-count 8))))

(defun warcel--align-bits ()
  "Write the bits not yet written, padded to a whole byte."
  (when (> warcel--bit-count 0)
    (insert (logand warcel--bits 255)))
  (setq warcel--bits 0
        warcel--bit-count 0))

(defun warcel--write-stored (data start end final)
  "Write the bytes of DATA from START to END as stored blocks.
FINAL non-nil means the last of these blocks ends the data."
  (while (progn
           (let* ((stop (min end (+ start 65535)))
                  (size (- stop start)))
             (warcel--put-bits (if (and final (= stop end)) 1 0) 1)
             (warcel--put-bits 0 2)
             (warcel--align-bits)
             (insert (logand size 255) (ash size -8)
                     (logand (lognot size) 255) (logand (ash (lognot size) -8) 255))
             (insert (substring data start stop))
             (setq start stop))
           (< start end))))

(defun warcel--length-runs (lengths)
  "Return the code lengths LENGTHS as code length code symbols.
Each element is (SYMBOL . EXTRA), EXTRA being the value of the extra
bits of SYMBOL."
  (let ((i 0)
        (size (length lengths))
        runs)
    (while (< i size)
      (let* ((length (aref lengths i))
             (run 1))
        (while (and (< (+ i run) size) (= (aref lengths (+ i run)) length))
          (setq run (1+ run)))
        (setq i (+ i run))
        (if (= length 0)
            (while (> run 0)
              (cond
               ((>= run 11) (let ((n (min run 138)))
                              (push (cons 18 (- n 11)) runs)
                              (setq run (- run n))))
               ((>= run 3) (push (cons 17 (- run 3)) runs)
                (setq run 0))
               (t (push (cons 0 0) runs)
                  (setq run (1- run)))))
          (push (cons length 0) runs)
          (setq run (1- run))
          (while (> run 0)
            (if (>= run 3)
                (let ((n (min run 6)))
                  (push (cons 16 (- n 3)) runs)
                  (setq run (- run n)))
              (push (cons length 0) runs)
              (setq run (1- run)))))))
    (nreverse runs)))

(defun warcel--write-huffman (data symbols from to start end final)
  "Write SYMBOLS FROM to TO, standing for DATA from START to END.
Write them as a block with fixed or dynamic Huffman codes, or as
stored blocks, whichever is smallest.  FINAL non-nil means the block
ends the data."
  (let ((freqs (make-vector 286 0))
        (distance-freqs (make-vector 30 0))
        (extra 0))
    (cl-loop for k from from below to
             for symbol = (aref symbols k)
             do (if (< symbol 256)
                    (cl-incf (aref freqs symbol))
                  (let ((length-code (aref warcel--length-codes (ash symbol -16)))
                        (distance-code (aref warcel--distance-codes
                                             (logand symbol #xffff))))
                    (cl-incf (aref freqs (+ 257 length-code)))
                    (cl-incf (aref distance-freqs distance-code))
                    (setq extra (+ extra (aref warcel--length-extra length-code)
                                   (aref warcel--distance-extra distance-code))))))
    (aset freqs 256 1)
    (let* ((lengths (warcel--huffman-lengths freqs 15))
           (distance-lengths (warcel--huffman-lengths distance-freqs 15))
           (lit-count (max 257 (1+ (cl-position-if #'cl-plusp lengths :from-end t))))
           (distance-count (1+ (cl-position-if #'cl-plusp distance-lengths :from-end t)))
           (runs (warcel--length-runs
                  (vconcat (seq-take lengths lit-count)
                           (seq-take distance-lengths distance-count))))
           (run-freqs (make-vector 19 0))
           (run-lengths (progn
                          (dolist (run runs)
                            (cl-incf (aref run-freqs (car run))))
                          (warcel--huffman-lengths run-freqs 7)))
           (run-count (max 4 (1+ (cl-position-if
                                  (lambda (symbol) (> (aref run-lengths symbol) 0))
                                  warcel--code-length-order :from-end t))))
           (fixed (nth 0 warcel--fixed-codes))
           (cost (lambda (lengths distance-lengths)
                   (+ extra
                      (cl-loop for f across freqs for l across lengths sum (* f l))
                      (cl-loop for f across distance-freqs for l across distance-lengths
                               sum (* f l)))))
           (dynamic-cost (+ 17 (* 3 run-count)
                            (cl-loop for (symbol . _) in runs
                                     sum (+ (aref run-lengths symbol)
                                            (pcase symbol (16 2) (17 3) (18 7) (_ 0))))
                            (funcall cost lengths distance-lengths)))
           (fixed-cost (+ 3 (funcall cost fixed (nth 2 warcel--fixed-codes))))
           (stored-cost (+ (* 8 (- end start))
                           (* 42 (max 1 (ceiling (- end start) 65535.0))))))
      (cond
       ((and (<= stored-cost fixed-cost) (<= stored-cost dynamic-cost))
        (warcel--write-stored data start end final))
       ((<= fixed-cost dynamic-cost)
        (warcel--put-bits (if final 1 0) 1)
        (warcel--put-bits 1 2)
        (warcel--write-symbols symbols from to fixed (nth 1 warcel--fixed-codes)
                               (nth 2 warcel--fixed-codes) (nth 3 warcel--fixed-codes)))
       (t
        (let ((run-codes (warcel--huffman-codes run-lengths)))
          (warcel--put-bits (if final 1 0) 1)
          (warcel--put-bits 2 2)
          (warcel--put-bits (- lit-count 257) 5)
          (warcel--put-bits (1- distance-count) 5)
          (warcel--put-bits (- run-count 4) 4)
          (dotimes (i run-count)
            (warcel--put-bits (aref run-lengths (aref warcel--code-length-order i)) 3))
          (dolist (run runs)
            (let ((symbol (car run)))
              (warcel--put-bits (aref run-codes symbol) (aref run-lengths symbol))
              (pcase symbol
                (16 (warcel--put-bits (cdr run) 2))
                (17 (warcel--put-bits (cdr run) 3))
                (18 (warcel--put-bits (cdr run) 7)))))
          (warcel--write-symbols symbols from to
                                 lengths (warcel--huffman-codes lengths)
                                 distance-lengths
                                 (warcel--huffman-codes distance-lengths))))))))

(defun warcel--write-symbols (symbols from to lengths codes distance-lengths
                                      distance-codes)
  "Write SYMBOLS FROM to TO and an end of block with the given codes.
LENGTHS and CODES are the literal and length codes, DISTANCE-LENGTHS
and DISTANCE-CODES the distance codes."
  (let ((length-codes warcel--length-codes)
        (distance-table warcel--distance-codes))
    (cl-loop for k from from below to
             for symbol = (aref symbols k)
             do (if (< symbol 256)
                    (warcel--put-bits (aref codes symbol) (aref lengths symbol))
                  (let* ((length (ash symbol -16))
                         (distance (logand symbol #xffff))
                         (code (aref length-codes length))
                         (distance-code (aref distance-table distance)))
                    (warcel--put-bits (aref codes (+ 257 code)) (aref lengths (+ 257 code)))
                    (warcel--put-bits (- length (aref warcel--length-bases code))
                                      (aref warcel--length-extra code))
                    (warcel--put-bits (aref distance-codes distance-code)
                                      (aref distance-lengths distance-code))
                    (warcel--put-bits (- distance (aref warcel--distance-bases distance-code))
                                      (aref warcel--distance-extra distance-code)))))
    (warcel--put-bits (aref codes 256) (aref lengths 256))))

(defun warcel--deflate (parts)
  "Insert at point the DEFLATE data of PARTS.
PARTS is a list of (DATA . COMPRESS), where DATA is a unibyte string
and COMPRESS is nil for data that would not get smaller, which is
stored as it is."
  (warcel--deflate-tables)
  (let ((warcel--bits 0)
        (warcel--bit-count 0)
        (blocks nil))
    ;; Plan the blocks first, to know which one is the last.
    (dolist (part parts)
      (let* ((data (car part))
             (size (length data)))
        (cond
         ((zerop size))
         ((not (cdr part))
          (push (list 'stored data 0 size) blocks))
         (t
          (pcase-let ((`(,symbols . ,count) (warcel--lz77 data))
                      (from 0)
                      (start 0))
            (while (< from count)
              (let ((to (min count (+ from warcel--block-symbols)))
                    (end start))
                (cl-loop for k from from below to
                         for symbol = (aref symbols k)
                         do (setq end (+ end (if (< symbol 256) 1 (ash symbol -16)))))
                (push (list 'huffman data start end symbols from to) blocks)
                (setq from to
                      start end))))))))
    (if (not blocks)
        ;; An empty final block with fixed codes: only the end of block.
        (progn
          (warcel--put-bits 1 1)
          (warcel--put-bits 1 2)
          (warcel--put-bits 0 7))
      (let ((last (car blocks)))
        (dolist (block (nreverse blocks))
          (pcase block
            (`(stored ,data ,start ,end)
             (warcel--write-stored data start end (eq block last)))
            (`(huffman ,data ,start ,end ,symbols ,from ,to)
             (warcel--write-huffman data symbols from to start end
                                    (eq block last)))))))
    (warcel--align-bits)))

(defun warcel--u32 (n)
  "Return N as four bytes, lowest first."
  (unibyte-string (logand n 255) (logand (ash n -8) 255)
                  (logand (ash n -16) 255) (logand (ash n -24) 255)))

(defun warcel--gzip (parts)
  "Return PARTS compressed as one gzip member, as a unibyte string.
PARTS is a list of (DATA . COMPRESS), as for `warcel--deflate'."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert (unibyte-string #x1f #x8b 8 0 0 0 0 0 0 255))
    (warcel--deflate parts)
    (let ((crc 0)
          (size 0))
      (dolist (part parts)
        (setq crc (warcel--crc32 (car part) crc)
              size (+ size (length (car part)))))
      (insert (warcel--u32 crc) (warcel--u32 (logand size #xffffffff))))
    (buffer-string)))

;;;; Capturing

(defconst warcel--max-redirects 10
  "Number of redirects followed when capturing a page.")

(defun warcel--base32 (bytes)
  "Return the unibyte string BYTES in unpadded base32 (RFC 4648)."
  (let ((alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567")
        (bits 0)
        (count 0)
        chars)
    (dotimes (i (length bytes))
      (setq bits (logior (ash bits 8) (aref bytes i))
            count (+ count 8))
      (while (>= count 5)
        (setq count (- count 5))
        (push (aref alphabet (logand (ash bits (- count)) 31)) chars))
      (setq bits (logand bits (1- (ash 1 count)))))
    (when (> count 0)
      (push (aref alphabet (logand (ash bits (- 5 count)) 31)) chars))
    (concat (nreverse chars))))

(defun warcel--digest (data)
  "Return the digest of the unibyte string DATA, as written in WARC headers."
  (concat "sha1:" (warcel--base32 (secure-hash 'sha1 data nil nil t))))

(defun warcel--record-id ()
  "Return a new WARC record ID, made of a random UUID."
  (let ((bytes (append (secure-hash 'sha1 (format "%S %S %S" (random) (current-time)
                                                  (emacs-pid))
                                    nil nil t)
                       nil)))
    ;; Mark the UUID as a random one (version 4, RFC 4122 variant).
    (setf (nth 6 bytes) (logior #x40 (logand (nth 6 bytes) #x0f))
          (nth 8 bytes) (logior #x80 (logand (nth 8 bytes) #x3f)))
    (apply #'format
           "<urn:uuid:%02x%02x%02x%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x>"
           (take 16 bytes))))

(defun warcel--capture-url (url)
  "Return URL in the form that is fetched and archived.
A URL without scheme is taken to be https.  The fragment is dropped,
the host name is IDNA-encoded and the rest percent-encoded.  Signal
a `user-error' if URL cannot be captured."
  (let ((url (string-trim url)))
    (when (string-empty-p url)
      (user-error "No URL given"))
    (unless (string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*:" url)
      (setq url (concat "https://" url)))
    (let ((parsed (url-generic-parse-url url)))
      (unless (member (url-type parsed) '("http" "https"))
        (user-error "Only http and https URLs can be captured: %s" url))
      (when (member (url-host parsed) '(nil ""))
        (user-error "No host name in %s" url))
      (setf (url-host parsed) (puny-encode-domain (url-host parsed))
            (url-target parsed) nil)
      (unless (string-prefix-p "/" (url-filename parsed))
        (setf (url-filename parsed) (concat "/" (url-filename parsed))))
      (url-encode-url (url-recreate-url parsed)))))

(defun warcel--clipboard-url ()
  "Return the URL in the clipboard, or at the head of the kill ring.
Return nil if neither holds a URL."
  (seq-some (lambda (text)
              (when (stringp text)
                (let ((text (string-trim (substring-no-properties text)))
                      (case-fold-search t))
                  (and (string-match-p "\\`https?://[^[:space:]]+\\'" text)
                       text))))
            (list (seq-some (lambda (type)
                              (ignore-errors (gui-get-selection 'CLIPBOARD type)))
                            '(UTF8_STRING STRING))
                  (car kill-ring))))

(defun warcel--read-capture-url ()
  "Read the URL of a page to capture, offering the one in the clipboard."
  (let ((default (warcel--clipboard-url)))
    (read-string (format-prompt "Capture URL" default) nil nil default)))

(defun warcel--capture-directory ()
  "Return the directory where new archives are saved."
  (file-name-as-directory
   (expand-file-name (or warcel-capture-directory default-directory))))

(defun warcel--capture-file-name (url)
  "Return the name of a new archive for the page at URL."
  (let* ((host (replace-regexp-in-string
                "[^[:alnum:].-]" "_" (url-host (url-generic-parse-url url))))
         (base (expand-file-name (concat host (format-time-string "-%Y%m%d-%H%M%S"))
                                 (warcel--capture-directory)))
         (extension (if warcel-capture-compress ".warc.gz" ".warc"))
         (file (concat base extension))
         (count 1))
    (while (file-exists-p file)
      (setq file (format "%s-%d%s" base (cl-incf count) extension)))
    file))

(defun warcel--archive-format (file)
  "Return the format of the WARC FILE, `warc' or `gzip', or nil if not WARC."
  (let ((reader (warcel--file-reader file)))
    (unwind-protect
        (pcase (warcel--file-format reader)
          ('warc 'warc)
          ('gzip (and (warcel--warc-start (car (warcel--gunzip-peek reader 0)))
                      'gzip)))
      (kill-buffer (warcel--reader-buffer reader)))))

(defun warcel--check-capture-file (file url)
  "Return the file to save the page at URL in, when FILE was chosen.
Signal a `user-error' if FILE is not a WARC file, and ask before
adding to an existing archive."
  (let ((file (expand-file-name file)))
    (when (file-directory-p file)
      (setq file (expand-file-name
                  (file-name-nondirectory (warcel--capture-file-name url)) file)))
    (when (and (file-exists-p file)
               (> (file-attribute-size (file-attributes file)) 0))
      (unless (warcel--archive-format file)
        (user-error "%s is not a WARC file" (abbreviate-file-name file)))
      (unless (y-or-n-p (format "Add the page to %s? " (abbreviate-file-name file)))
        (user-error "Nothing captured")))
    file))

(defun warcel--http-request (url)
  "Return the HTTP request that fetches URL."
  (let ((parsed (url-generic-parse-url url)))
    (concat "GET " (url-filename parsed) " HTTP/1.1\r\n"
            "Host: " (url-host parsed)
            (if (url-portspec parsed) (format ":%d" (url-port parsed)) "") "\r\n"
            "User-Agent: " warcel-capture-user-agent "\r\n"
            "Accept: text/html,application/xhtml+xml,*/*;q=0.8\r\n"
            ;; Compressed pages are smaller to fetch and to store.
            "Accept-Encoding: gzip\r\n"
            "Connection: close\r\n"
            "\r\n")))

(defun warcel--response-complete-p (buffer)
  "Return non-nil if BUFFER holds a whole HTTP response of known length.
Other responses end when the server closes the connection."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "\r?\n\r?\n" nil t)
        (let ((head (buffer-substring-no-properties (point-min) (point)))
              (body (- (point-max) (point)))
              (case-fold-search t))
          (cond
           ((string-match-p "\\`HTTP/[0-9.]+[ \t]+\\(?:204\\|304\\)" head) t)
           ((string-match-p "^transfer-encoding:.*chunked" head) nil)
           ((string-match "^content-length:[ \t]*\\([0-9]+\\)" head)
            (>= body (string-to-number (match-string 1 head))))))))))

(defun warcel--fetch-start (url callback &optional progress)
  "Start fetching URL in the background and return a function that stops it.
When the whole response has arrived, call CALLBACK with the exchange
with the server: a plist with :url, :date, :ip, :status, :location,
and :request and :response, the bytes sent and received.  If URL
cannot be fetched, call CALLBACK with an error message instead.  With
PROGRESS non-nil, report progress in the echo area."
  (let* ((parsed (url-generic-parse-url url))
         (https (equal (url-type parsed) "https"))
         (host (string-trim (url-host parsed) "\\[" "]"))
         (request (warcel--http-request url))
         (buffer (warcel--unibyte-buffer " *warcel-capture*"))
         (date (current-time))
         (active (float-time))
         process timer ip done)
    (cl-labels
        ((stop ()
           ;; Stop fetching and return the bytes received.
           (setq done t)
           (when timer
             (cancel-timer timer))
           (when process
             (delete-process process))
           (prog1 (with-current-buffer buffer (buffer-string))
             (kill-buffer buffer)))
         (finish (&optional problem)
           ;; Call CALLBACK with the exchange, or with PROBLEM.
           (unless done
             (let ((response (stop)))
               (funcall
                callback
                (cond
                 (problem)
                 ((string-match "\\`HTTP/[0-9.]+[ \t]+\\([0-9][0-9][0-9]\\)" response)
                  (let ((status (string-to-number (match-string 1 response))))
                    (list :url url :date date :ip ip :status status
                          :location (and (<= 300 status 399)
                                         (warcel--field (nth 1 (warcel--split-http response))
                                                        "Location"))
                          :request request :response response)))
                 (t (format "No HTTP response from %s" host)))))))
         (receive (_process string)
           (with-current-buffer buffer
             (goto-char (point-max))
             (insert string))
           (setq active (float-time))
           (when progress
             (message "Fetching %s... %s" url (file-size-human-readable (buffer-size buffer))))
           (when (warcel--response-complete-p buffer)
             (finish)))
         (sentinel (process event)
           (cond
            ((string-prefix-p "open" event)
             (let ((address (process-contact process :remote)))
               (when (vectorp address)
                 (setq ip (format-network-address address t))))
             (process-send-string process request))
            ((string-prefix-p "failed" event)
             (finish (format "Cannot connect to %s: %s" host (string-trim event))))
            ;; Other responses end when the server closes the connection.
            ((not (process-live-p process))
             (finish))))
         (check-activity ()
           (when (> (- (float-time) active) warcel-capture-timeout)
             (finish (format "No data from %s for %d seconds"
                             url warcel-capture-timeout)))))
      (let ((problem
             (if (and https (not (gnutls-available-p)))
                 "This Emacs has no GnuTLS support, so it cannot fetch https URLs"
               (when progress
                 (message "Connecting to %s..." host))
               (condition-case err
                   (progn
                     (setq process (open-network-stream "warcel-capture" buffer host
                                                        (url-port parsed)
                                                        :type (if https 'tls 'plain)
                                                        :coding 'binary
                                                        :nowait t))
                     (set-process-query-on-exit-flag process nil)
                     (set-process-filter process #'receive)
                     (set-process-sentinel process #'sentinel)
                     (setq timer (run-at-time 0.25 0.25 #'check-activity))
                     nil)
                 (error (format "Cannot connect to %s: %s"
                                host (error-message-string err)))))))
        (when problem
          ;; Report it once the caller has the function that stops fetching.
          (run-at-time 0 nil #'finish problem)))
      (lambda ()
        (unless done
          (stop))))))

(defun warcel--fetch-page-start (url callback &optional progress)
  "Start fetching URL in the background, following redirects.
Return a function that stops fetching.  Call CALLBACK with the list of
exchanges with the servers, as described in `warcel--fetch-start', or
with an error message if the page cannot be fetched.  With PROGRESS
non-nil, report progress in the echo area."
  (let (seen exchanges stop)
    (cl-labels
        ((fetch (url)
           (cond
            ((member url seen)
             (funcall callback (format "Redirect loop at %s" url)))
            ((> (length seen) warcel--max-redirects)
             (funcall callback (format "More than %d redirects from %s"
                                       warcel--max-redirects (car (last seen)))))
            (t
             (push url seen)
             (setq stop (warcel--fetch-start url #'fetched progress)))))
         (fetched (result)
           (if (stringp result)
               (funcall callback result)
             (push result exchanges)
             (let* ((location (plist-get result :location))
                    (next (and location
                               (warcel--expand-url location (plist-get result :url)))))
               (cond
                ((not (and next (string-match-p "\\`https?://" next)))
                 (funcall callback (reverse exchanges)))
                ((setq next (ignore-error user-error (warcel--capture-url next)))
                 (fetch next))
                (t
                 (funcall callback (format "Cannot follow the redirect to %s"
                                           location))))))))
      (fetch url)
      (lambda ()
        (funcall stop)))))

(defun warcel--wait (start)
  "Call START with a callback, and return what the callback gets.
START starts a job in the background, which calls the callback with its
result, or with an error message, and returns a function that stops the
job.  Signal a `user-error' with the message if there is one.  The job
is stopped if waiting is interrupted."
  (let* ((result nil)
         (stop (funcall start (lambda (value) (setq result (list value))))))
    (unwind-protect
        (while (not result)
          (accept-process-output nil 0.1))
      (unless result
        (funcall stop)))
    (if (stringp (car result))
        (user-error "%s" (car result))
      (car result))))

(defun warcel--fetch-page (url)
  "Fetch URL, following redirects, and return the list of exchanges.
Signal a `user-error' if the page cannot be fetched."
  (warcel--wait (lambda (callback)
                  (warcel--fetch-page-start url callback t))))

(defun warcel--warc-record (fields block)
  "Return a WARC record with the header FIELDS and BLOCK.
FIELDS is an alist of header field names and values.  BLOCK is a
list of (DATA . COMPRESS), DATA being unibyte strings, and COMPRESS
nil for data that would not get smaller if compressed.  The record is
returned in the same form."
  `((,(encode-coding-string
       (concat "WARC/1.1\r\n"
               (mapconcat (lambda (field)
                            (concat (car field) ": " (cdr field) "\r\n"))
                          fields "")
               (format "Content-Length: %d\r\n\r\n"
                       (apply #'+ (mapcar (lambda (part) (length (car part))) block))))
       'utf-8)
     . t)
    ,@block
    ("\r\n\r\n" . t)))

(defun warcel--compressible-p (headers)
  "Return non-nil if the body of an HTTP message with HEADERS may compress.
Bodies that are compressed already do not."
  (let ((encoding (downcase (string-trim (or (warcel--field headers "Content-Encoding")
                                             ""))))
        (type (downcase (or (warcel--field headers "Content-Type") ""))))
    (and (member encoding '("" "identity"))
         (not (and (warcel--media-mime-p type)
                   (not (string-search "svg" type))))
         (not (string-match-p
               "\\`application/\\(?:zip\\|gzip\\|x-gzip\\|zstd\\|x-xz\\|x-bzip2\\|x-7z\\|pdf\\)"
               type)))))

(defun warcel--http-parts (message)
  "Return the HTTP MESSAGE as a list of (DATA . COMPRESS).
The head is compressed, and the body when `warcel--compressible-p'."
  (if (string-match "\r?\n\r?\n" message)
      (let ((body-start (match-end 0)))
        `((,(substring message 0 body-start) . t)
          (,(substring message body-start)
           . ,(warcel--compressible-p
               (nth 1 (warcel--split-http message))))))
    `((,message . t))))

(defun warcel--warcinfo-record (file)
  "Return the record that describes the new archive FILE."
  (warcel--warc-record
   `(("WARC-Type" . "warcinfo")
     ("WARC-Record-ID" . ,(warcel--record-id))
     ("WARC-Date" . ,(format-time-string "%FT%TZ" nil t))
     ("WARC-Filename" . ,(file-name-nondirectory file))
     ("Content-Type" . "application/warc-fields"))
   `((,(encode-coding-string
        (format "software: warcel/%s (GNU Emacs %s)\r\nformat: WARC File Format 1.1\r\n"
                warcel-version emacs-version)
        'utf-8)
      . t))))

(defun warcel--exchange-records (exchange)
  "Return the response and request records of EXCHANGE."
  (let* ((date (format-time-string "%FT%TZ" (plist-get exchange :date) t))
         (url (plist-get exchange :url))
         (ip (plist-get exchange :ip))
         (request (plist-get exchange :request))
         (response (plist-get exchange :response))
         (response-id (warcel--record-id))
         (payload (pcase-let ((`(,_ ,headers ,body) (warcel--split-http response)))
                    ;; The payload is the body without its transfer coding.
                    (if (string-search "chunked"
                                       (downcase (or (warcel--field headers
                                                                    "Transfer-Encoding")
                                                     "")))
                        (warcel--dechunk body)
                      body))))
    (list (warcel--warc-record
           `(("WARC-Type" . "response")
             ("WARC-Record-ID" . ,response-id)
             ("WARC-Date" . ,date)
             ("WARC-Target-URI" . ,url)
             ,@(and ip `(("WARC-IP-Address" . ,ip)))
             ("WARC-Block-Digest" . ,(warcel--digest response))
             ("WARC-Payload-Digest" . ,(warcel--digest payload))
             ("Content-Type" . "application/http; msgtype=response"))
           (warcel--http-parts response))
          (warcel--warc-record
           `(("WARC-Type" . "request")
             ("WARC-Record-ID" . ,(warcel--record-id))
             ("WARC-Date" . ,date)
             ("WARC-Target-URI" . ,url)
             ("WARC-Concurrent-To" . ,response-id)
             ("WARC-Block-Digest" . ,(warcel--digest request))
             ("Content-Type" . "application/http; msgtype=request"))
           (warcel--http-parts request)))))

(defun warcel--write-capture (file exchanges)
  "Add the records of EXCHANGES to the archive FILE, creating it if needed.
A new archive is compressed if FILE ends in .gz; records are added to
an existing archive in its own format."
  (let* ((new (or (not (file-exists-p file))
                  (zerop (file-attribute-size (file-attributes file)))))
         (compress (if new
                       (string-match-p "\\.gz\\'" file)
                     (eq (warcel--archive-format file) 'gzip)))
         (records (append (and new (list (warcel--warcinfo-record file)))
                          (mapcan #'warcel--exchange-records exchanges))))
    (make-directory (file-name-directory file) t)
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (dolist (record records)
        (if compress
            ;; One gzip member per record, as other tools write them.
            (insert (warcel--gzip record))
          (dolist (part record)
            (insert (car part)))))
      (let ((coding-system-for-write 'no-conversion))
        (write-region nil nil file (not new) 'silent)))))

;;;###autoload
(defun warcel-capture (url &optional file)
  "Save the web page at URL in a WARC archive and show it.
Interactively, ask for URL, offering the URL in the clipboard.

The page goes to a new archive in `warcel-capture-directory', named
after the host and the time, and is shown.  With a prefix argument,
ask for the archive FILE; the page is added to it if it already
exists, and the records of the archive are listed.

The page is saved with the redirects that lead to it, but without
its images, styles and scripts, which warcel does not show.  New
archives are compressed when `warcel-capture-compress' is non-nil, or
when FILE ends in .gz."
  (interactive
   (let ((url (warcel--capture-url (warcel--read-capture-url))))
     (list url
           (and current-prefix-arg
                (let ((name (file-name-nondirectory (warcel--capture-file-name url))))
                  (read-file-name "Save in archive: " (warcel--capture-directory)
                                  nil nil name))))))
  (let* ((url (warcel--capture-url url))
         (file (if file
                   (warcel--check-capture-file file url)
                 (warcel--capture-file-name url)))
         (exchanges (warcel--fetch-page url))
         (last (car (last exchanges)))
         (status (plist-get last :status)))
    (warcel--write-capture file exchanges)
    (when-let* ((buffer (warcel--listing-buffer file)))
      (with-current-buffer buffer
        (revert-buffer)))
    (warcel-open file)
    (when (derived-mode-p 'warcel-mode)
      (warcel--goto-uri (plist-get last :url)))
    (message "Saved %s in %s%s"
             (plist-get last :url) (abbreviate-file-name file)
             (if (<= 200 status 299) "" (format " (the server answered %d)" status)))
    file))

(defun warcel-capture-in-background (url file &optional callback)
  "Save the web page at URL in a WARC archive, without waiting for it.
The page is fetched in the background and saved as `warcel-capture'
saves it, with the redirects that lead to it.  FILE is the archive, or
a function that returns it: the function is called once the page has
been fetched, with its HTTP status code and its media type, such as
\"text/html\" (\"\" if the server gives none), and may return nil not to
save it.  A new archive is compressed if its name ends in .gz; pages
added to an existing archive keep its format.

When done, call CALLBACK, if non-nil, with three arguments: the archive
the page was saved in (nil if it was not saved), the status code, and
nil; or, if the page could not be fetched or saved, nil, the status
code if known, and an error message.  Nothing is shown.

Signal a `user-error' if URL is not an http or https URL.  Return a
function that stops the capture."
  (warcel--fetch-page-start
   (warcel--capture-url url)
   (lambda (result)
     (let (archive status problem)
       (if (stringp result)
           (setq problem result)
         (let* ((response (plist-get (car (last result)) :response))
                (type (downcase (string-trim
                                 (car (split-string
                                       (or (warcel--field (nth 1 (warcel--split-http response))
                                                          "Content-Type")
                                           "")
                                       ";"))))))
           (setq status (plist-get (car (last result)) :status))
           (condition-case err
               (when (setq archive (if (functionp file) (funcall file status type) file))
                 (warcel--write-capture archive result))
             (error (setq archive nil
                          problem (error-message-string err))))))
       (when callback
         (funcall callback archive status problem))))))

(defun warcel-canonical-url (url)
  "Return URL in the form that `warcel-capture' fetches and archives.
The fragment is dropped, the host name IDNA-encoded and the rest
percent-encoded.  Return nil if URL is not an http or https URL."
  (ignore-error user-error
    (warcel--capture-url url)))

(defun warcel-archived-urls (file)
  "Return the URLs of the pages archived in the WARC FILE.
They are the target URIs of its response and resource records, so they
include the redirects that led to a page, in the form given by
`warcel-canonical-url' for pages saved by warcel."
  (let ((inhibit-message t)
        (message-log-max nil))
    (delete-dups
     (delq nil (mapcar (lambda (record)
                         (and (member (warcel-record-type record) '("response" "resource"))
                              (warcel-record-uri record)))
                       (warcel-archive-records (warcel-read-archive file)))))))

;;;; Visiting compressed archives

;; Emacs visits compressed files with `jka-compr', which runs the gzip
;; program, and would stop after the first gzip member if it used zlib
;; instead.  This file name handler reads .warc.gz files with zlib,
;; member after member.  It only handles the insertion of .warc.gz
;; files, and only loads warcel when it does.

(defun warcel--gunzip-file (file)
  "Return a unibyte buffer holding all the decompressed gzip members of FILE.
Return nil if FILE does not start with a gzip member."
  (let ((reader (warcel--file-reader file))
        (offset 0)
        data)
    (unwind-protect
        (when (eq (warcel--file-format reader) 'gzip)
          (setq data (warcel--unibyte-buffer " *warcel-gunzip*"))
          (while (< offset (warcel--reader-size reader))
            (pcase-let ((`(,end ,_ ,_ ,member) (warcel--gzip-member-end reader offset nil)))
              (warcel--append-member data file offset end member)
              (setq offset end)))
          data)
      (kill-buffer (warcel--reader-buffer reader)))))

(defun warcel--insert-gzip-file (file &optional visit beg end replace)
  "Insert the decompressed contents of the gzip FILE.
VISIT, BEG, END and REPLACE are as for `insert-file-contents'."
  (barf-if-buffer-read-only)
  (when (and visit (or beg end))
    (error "Attempt to visit less than an entire file"))
  (let* ((filename (expand-file-name file))
         (data (warcel--gunzip-file filename))
         size)
    (if (not data)
        ;; Not gzip data after all: let the other handlers try.
        (let ((inhibit-file-name-handlers
               (cons 'warcel-file-handler
                     (and (eq inhibit-file-name-operation 'insert-file-contents)
                          inhibit-file-name-handlers)))
              (inhibit-file-name-operation 'insert-file-contents))
          (insert-file-contents file visit beg end replace))
      (unwind-protect
          (with-current-buffer data
            (when end
              (delete-region (min (1+ end) (point-max)) (point-max)))
            (when beg
              (delete-region (point-min) (min (1+ beg) (point-max))))
            (setq size (buffer-size)))
        (when replace
          (delete-region (point-min) (point-max)))
        (let ((start (point))
              ;; Inserting into a visited buffer would lock the file.
              (buffer-file-name (if visit nil buffer-file-name)))
          (insert-buffer-substring data)
          (goto-char start))
        (kill-buffer data))
      (decode-coding-inserted-region (point) (+ (point) size) filename
                                     visit beg end replace)
      (when visit
        (unlock-buffer)
        (setq buffer-file-name filename)
        ;; Saving compresses the file again, as `jka-compr' does.
        (when (boundp 'jka-compr-really-do-compress)
          (setq jka-compr-really-do-compress t))
        (set-visited-file-modtime))
      (list filename size))))

;;;###autoload
(progn
  (defun warcel-file-handler (operation &rest args)
    "Read .warc.gz files with the zlib built into Emacs.
This file name handler takes over from `jka-compr', which runs the
gzip program, when a .warc.gz file is inserted.  It applies OPERATION
to ARGS as usual in all other cases."
    (if (and (eq operation 'insert-file-contents)
             (string-match-p "\\.warc\\.gz\\'" (car args))
             ;; `insert-file-contents-literally' does not decompress.
             (not (and (eq inhibit-file-name-operation 'insert-file-contents)
                       (memq 'jka-compr-handler inhibit-file-name-handlers)))
             (fboundp 'zlib-available-p)
             (zlib-available-p))
        (progn
          (require 'warcel)
          (apply #'warcel--insert-gzip-file args))
      (let ((inhibit-file-name-handlers
             (cons 'warcel-file-handler
                   (and (eq inhibit-file-name-operation operation)
                        inhibit-file-name-handlers)))
            (inhibit-file-name-operation operation))
        (apply operation args))))
  (put 'warcel-file-handler 'operations '(insert-file-contents))
  ;; The handler whose match starts last wins: this one matches after
  ;; the dot that the `jka-compr' handler matches.
  (add-to-list 'file-name-handler-alist '("gz\\'" . warcel-file-handler)))

;;;###autoload
(progn
  (defun warcel-dired-find-file ()
    "In Dired, show the WARC file at point with `warcel-open'.
Visit other files with `dired-find-file'.  This command replaces
`dired-find-file' in Dired, so that RET opens WARC files this way."
    (interactive nil dired-mode)
    (let ((file (dired-get-file-for-visit)))
      (if (and (string-match-p "\\.warc\\(?:\\.gz\\)?\\'" file)
               (file-regular-p file))
          (warcel-open file)
        (dired-find-file))))
  (with-eval-after-load 'dired
    (keymap-set dired-mode-map "<remap> <dired-find-file>" #'warcel-dired-find-file)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.warc\\'" . warcel-visit))

(provide 'warcel)
;;; warcel.el ends here
