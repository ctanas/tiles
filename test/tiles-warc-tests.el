;;; tiles-warc-tests.el --- Tests for the web archives of TILES  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for the WARC code of tiles.el (prefix `tiles-warc-'), which
;; was warcel until it was folded into TILES.  Run from the repository
;; root with `make test', or:
;;
;;   emacs -Q --batch -L . -l test/tiles-warc-tests.el -f ert-run-tests-batch-and-exit
;;
;; The WARC files used by the tests are built here.  Gzip data is made
;; of stored (uncompressed) deflate blocks, so compressed payloads and
;; embedded gzip files appear as is inside the compressed archives, the
;; way they do in real archives where they cannot be compressed further.

;;; Code:

(require 'ert)
(require 'tiles)
(require 'org)
(require 'org-element)

;; As a user does: this sets up how Emacs opens WARC files (visiting
;; .warc and .warc.gz files, RET in Dired), which some tests rely on.
;; Notes are never saved here, so no page is archived.
(setq tiles-cache-file nil
      tiles-watch-files nil
      tiles-archive-urls t)

;;;; Building WARC files

(defun tiles-warc-test--crc32 (data)
  "Return the CRC-32 of the unibyte string DATA."
  (let ((crc #xffffffff))
    (dotimes (i (length data))
      (setq crc (logxor crc (aref data i)))
      (dotimes (_ 8)
        (setq crc (if (= 1 (logand crc 1))
                      (logxor (ash crc -1) #xedb88320)
                    (ash crc -1)))))
    (logxor crc #xffffffff)))

(defun tiles-warc-test--u32 (n)
  "Return N as 4 little-endian bytes."
  (unibyte-string (logand n 255) (logand (ash n -8) 255)
                  (logand (ash n -16) 255) (logand (ash n -24) 255)))

(defun tiles-warc-test--gzip (data)
  "Return the unibyte string DATA as a gzip member of stored blocks."
  (let ((parts (list (unibyte-string #x1f #x8b 8 0 0 0 0 0 0 255)))
        (pos 0)
        (length (length data)))
    (while (progn
             (let* ((n (min 65535 (- length pos)))
                    (final (if (>= (+ pos n) length) 1 0))
                    (complement (logand (lognot n) #xffff)))
               (push (unibyte-string final (logand n 255) (ash n -8)
                                     (logand complement 255) (ash complement -8))
                     parts)
               (push (substring data pos (+ pos n)) parts)
               (setq pos (+ pos n)))
             (< pos length)))
    (push (tiles-warc-test--u32 (tiles-warc-test--crc32 data)) parts)
    (push (tiles-warc-test--u32 length) parts)
    (apply #'concat (nreverse parts))))

(defun tiles-warc-test--bytes (string)
  "Return STRING encoded in UTF-8, as a unibyte string."
  (if (multibyte-string-p string) (encode-coding-string string 'utf-8) string))

(defun tiles-warc-test--chunked (data size)
  "Return DATA with the chunked transfer coding, in chunks of SIZE bytes."
  (let ((parts nil)
        (pos 0))
    (while (< pos (length data))
      (let ((chunk (substring data pos (min (length data) (+ pos size)))))
        (push (format "%x\r\n" (length chunk)) parts)
        (push chunk parts)
        (push "\r\n" parts)
        (setq pos (+ pos size))))
    (push "0\r\n\r\n" parts)
    (apply #'concat (mapcar #'tiles-warc-test--bytes (nreverse parts)))))

(defun tiles-warc-test--http (status headers body)
  "Return an HTTP response with STATUS, HEADERS (an alist) and BODY."
  (concat (tiles-warc-test--bytes (format "HTTP/1.1 %s\r\n" status))
          (mapconcat (lambda (header)
                       (tiles-warc-test--bytes (format "%s: %s\r\n" (car header) (cdr header))))
                     headers "")
          "\r\n"
          (tiles-warc-test--bytes body)))

(defun tiles-warc-test--record (type uri block &rest fields)
  "Return a WARC record of TYPE for URI with BLOCK and extra header FIELDS.
FIELDS is an alist of header names and values."
  (let ((block (tiles-warc-test--bytes block)))
    (concat (tiles-warc-test--bytes
             (concat "WARC/1.0\r\n"
                     "WARC-Type: " type "\r\n"
                     (if uri (concat "WARC-Target-URI: " uri "\r\n") "")
                     "WARC-Date: 2024-05-06T07:08:09Z\r\n"
                     (mapconcat (lambda (field)
                                  (format "%s: %s\r\n" (car field) (cdr field)))
                                fields "")
                     (format "Content-Length: %d\r\n\r\n" (length block))))
            block
            "\r\n\r\n")))

(defconst tiles-warc-test--home-html
  "<!DOCTYPE html>
<html><head><title>Home  page</title>
<script>alert('x')</script><style>p { color: red }</style>
<link rel=\"stylesheet\" href=\"/style.css\">
</head>
<body onload=\"evil()\">
<h1>Welcome <em>home</em></h1>
<p>Some <b>bold</b>, <i>italic</i> and <a href=\"/about\" onclick=\"x()\">about us</a>.<img src=\"a.png\" alt=\"A picture\"></p>
<p>intra<b>word</b>bold and <code>code</code> and <a href=\"javascript:void(0)\">script link</a></p>
<ul><li>One</li><li>Two<ul><li>Nested</li></ul></li></ul>
<ol start=\"3\"><li>Three</li><li>Four</li></ol>
<dl><dt>Term</dt><dd>Definition</dd></dl>
<pre>* not a heading
#+not a keyword
  indented</pre>
<blockquote><p>Quoted text</p></blockquote>
<table><tr><th>Name</th><th>Value</th></tr><tr><td>a</td><td>1|2</td></tr></table>
<div hidden>Hidden text</div><div style=\"display: none\">Also hidden</div>
<iframe src=\"http://ads.example/\"></iframe><video src=\"v.mp4\">Video fallback</video>
<noscript>Enable JavaScript</noscript>
<p><a href=\"/logo\"><img src=\"logo.png\" alt=\"Logo\"></a>
<a href=\"http://elsewhere.example/\">elsewhere</a>
<a href=\"/moved\">moved page</a> ünïcödé</p>
</body></html>"
  "The home page of the test archive.")

(defconst tiles-warc-test--about-html
  (encode-coding-string
   "<html><head><meta charset=\"iso-8859-1\"><title>About</title></head>
<body><p>Café</p><p><a href=\"/\">home</a></p></body></html>"
   'latin-1)
  "The about page of the test archive, in Latin-1.")

(defconst tiles-warc-test--image
  (concat (unibyte-string #x89) "PNG\r\n" (unibyte-string #x1a #x0a 0 0 #x1f #x8b 8 0))
  "An image payload.")

(defun tiles-warc-test--records ()
  "Return the records of the test archive, as unibyte strings."
  (let ((home (tiles-warc-test--bytes tiles-warc-test--home-html)))
    (list
     (tiles-warc-test--record "warcinfo" nil "software: tiles-warc-tests\r\n"
                          '("Content-Type" . "application/warc-fields"))
     (tiles-warc-test--record
      "response" "http://example.com/"
      (tiles-warc-test--http "200 OK"
                         '(("Content-Type" . "text/html; charset=utf-8")
                           ("Content-Encoding" . "gzip")
                           ("Transfer-Encoding" . "chunked"))
                         (tiles-warc-test--chunked (tiles-warc-test--gzip home) 100))
      '("WARC-Record-ID" . "<urn:uuid:home>")
      '("WARC-Payload-Digest" . "sha1:HOME")
      '("Content-Type" . "application/http; msgtype=response"))
     (tiles-warc-test--record
      "request" "http://example.com/"
      "GET / HTTP/1.1\r\nHost: example.com\r\n\r\n"
      '("Content-Type" . "application/http; msgtype=request"))
     (tiles-warc-test--record
      "response" "http://example.com/about"
      (tiles-warc-test--http "200 OK" '(("Content-Type" . "text/html"))
                         tiles-warc-test--about-html)
      '("Content-Type" . "application/http; msgtype=response"))
     (tiles-warc-test--record
      "revisit" "http://example.com/"
      (tiles-warc-test--http "200 OK" '(("Content-Type" . "text/html")) "")
      '("WARC-Refers-To" . "<urn:uuid:home>")
      '("WARC-Payload-Digest" . "sha1:HOME")
      '("Content-Type" . "application/http; msgtype=response"))
     (tiles-warc-test--record
      "response" "http://example.com/moved"
      (tiles-warc-test--http "301 Moved Permanently" '(("Location" . "/about")) "")
      '("Content-Type" . "application/http; msgtype=response"))
     (tiles-warc-test--record
      "response" "http://example.com/image.png"
      (tiles-warc-test--http "200 OK" '(("Content-Type" . "image/png"))
                         tiles-warc-test--image)
      '("Content-Type" . "application/http; msgtype=response"))
     (tiles-warc-test--record "resource" "http://example.com/notes.txt"
                          "Plain notes\n"
                          '("Content-Type" . "text/plain"))
     ;; A compressed WARC file stored in the archive.
     (tiles-warc-test--record
      "resource" "http://example.com/inner.warc.gz"
      (tiles-warc-test--gzip (tiles-warc-test--record "resource" "http://inner.example/"
                                              "inner"
                                              '("Content-Type" . "text/plain")))
      '("Content-Type" . "application/gzip")))))

(defconst tiles-warc-test--expected
  '(("warcinfo" nil nil) ("response" "http://example.com/" 200)
    ("request" "http://example.com/" nil) ("response" "http://example.com/about" 200)
    ("revisit" "http://example.com/" 200) ("response" "http://example.com/moved" 301)
    ("response" "http://example.com/image.png" 200)
    ("resource" "http://example.com/notes.txt" nil)
    ("resource" "http://example.com/inner.warc.gz" nil))
  "Type, target URI and HTTP status of the records of the test archive.")

(defun tiles-warc-test--split (data size)
  "Return DATA cut into pieces of SIZE bytes."
  (let (pieces)
    (dotimes (i (ceiling (length data) (float size)))
      (push (substring data (* i size) (min (length data) (* (1+ i) size))) pieces))
    (nreverse pieces)))

(defun tiles-warc-test--variants ()
  "Return an alist of (NAME . CONTENTS) for the test archive in all forms."
  (let* ((records (tiles-warc-test--records))
         (plain (apply #'concat records)))
    `(("plain" . ,plain)
      ("per-record" . ,(mapconcat #'tiles-warc-test--gzip records ""))
      ("whole" . ,(tiles-warc-test--gzip plain))
      ("grouped" . ,(concat (tiles-warc-test--gzip (apply #'concat (seq-take records 3)))
                            (tiles-warc-test--gzip (apply #'concat (seq-subseq records 3 6)))
                            (tiles-warc-test--gzip (apply #'concat (seq-drop records 6)))))
      ;; Members that do not start or end with records.
      ("misaligned" . ,(mapconcat #'tiles-warc-test--gzip
                                  (tiles-warc-test--split plain 150) "")))))

(defmacro tiles-warc-test--with-archive (spec &rest body)
  "Run BODY with (FILE-VAR ARCHIVE-VAR CONTENTS) bound as in SPEC.
FILE-VAR is bound to a temporary WARC file holding CONTENTS and
ARCHIVE-VAR to the archive read from it."
  (declare (indent 1))
  (let ((file (nth 0 spec))
        (archive (nth 1 spec))
        (contents (nth 2 spec)))
    `(let ((,file (make-temp-file "tiles-warc-test" nil ".warc")))
       (unwind-protect
           (let ((coding-system-for-write 'no-conversion))
             (with-temp-file ,file
               (set-buffer-multibyte nil)
               (insert ,contents))
             (let ((,archive (tiles-warc-read-archive ,file)))
               (ignore ,archive)
               ,@body))
         (delete-file ,file)))))

(defun tiles-warc-test--record-at (archive index)
  "Return the record number INDEX (from 1) of ARCHIVE."
  (aref (tiles-warc-archive-records archive) (1- index)))

(defun tiles-warc-test--home-org ()
  "Return the Org text of the home page of the test archive."
  (tiles-warc-test--with-archive (_file archive (cdr (assoc "per-record" (tiles-warc-test--variants))))
    (tiles--warc-page-org (tiles--warc-page archive (tiles-warc-test--record-at archive 2)))))

;;;; Reading

(ert-deftest tiles-warc-test-read-variants ()
  "All forms of the archive give the same records and payloads."
  (dolist (variant (tiles-warc-test--variants))
    (tiles-warc-test--with-archive (_file archive (cdr variant))
      (should (equal (mapcar (lambda (record)
                               (list (tiles-warc-record-type record)
                                     (tiles-warc-record-uri record)
                                     (tiles-warc-record-http-status record)))
                             (tiles-warc-archive-records archive))
                     tiles-warc-test--expected))
      (should (equal (tiles--warc-record-payload archive (tiles-warc-test--record-at archive 2))
                     (tiles-warc-test--bytes tiles-warc-test--home-html)))
      (should (equal (tiles--warc-record-payload archive (tiles-warc-test--record-at archive 4))
                     tiles-warc-test--about-html))
      (should (equal (tiles--warc-record-payload archive (tiles-warc-test--record-at archive 7))
                     tiles-warc-test--image))
      (should (equal (tiles--warc-record-payload archive (tiles-warc-test--record-at archive 9))
                     (tiles-warc-test--gzip
                      (tiles-warc-test--record "resource" "http://inner.example/" "inner"
                                           '("Content-Type" . "text/plain"))))))))

(ert-deftest tiles-warc-test-members ()
  "Records of an archive compressed per record have their own members."
  (tiles-warc-test--with-archive (_file archive (cdr (assoc "per-record" (tiles-warc-test--variants))))
    (let ((offset 0))
      (seq-doseq (record (tiles-warc-archive-records archive))
        (should (equal (car (tiles-warc-record-member record)) offset))
        (should (zerop (tiles-warc-record-offset record)))
        (setq offset (cdr (tiles-warc-record-member record)))))))

(ert-deftest tiles-warc-test-truncated ()
  "A truncated archive keeps its complete records."
  (let ((plain (apply #'concat (tiles-warc-test--records))))
    (tiles-warc-test--with-archive (_file archive (substring plain 0 (- (length plain) 30)))
      (should (= (length (tiles-warc-archive-records archive)) 9)))))

(ert-deftest tiles-warc-test-not-warc ()
  "Files that are not WARC files are rejected."
  (should-error (tiles-warc-test--with-archive (_file _archive "Hello, world\n")))
  (should-error (tiles-warc-test--with-archive (_file _archive (tiles-warc-test--gzip "Hello\n")))))

;;;; Payloads

(ert-deftest tiles-warc-test-dechunk ()
  (should (equal (tiles--warc-dechunk (tiles-warc-test--chunked "Hello, world" 5))
                 "Hello, world"))
  ;; Bodies wrongly declared as chunked are left alone.
  (should (equal (tiles--warc-dechunk "<html>not chunked</html>")
                 "<html>not chunked</html>")))

(ert-deftest tiles-warc-test-inflate ()
  (should (equal (tiles--warc-inflate (tiles-warc-test--gzip "gzip data")) "gzip data"))
  (should-not (tiles--warc-inflate "plain data")))

(ert-deftest tiles-warc-test-charset ()
  (should (eq (tiles--warc-charset "text/html; charset=ISO-8859-1" "") 'windows-1252))
  (should (eq (tiles--warc-charset "text/html" "<meta charset=\"utf-8\">") 'utf-8))
  (should (eq (tiles--warc-charset "text/html; charset=nonsense" "") 'prefer-utf-8))
  (tiles-warc-test--with-archive (_file archive (apply #'concat (tiles-warc-test--records)))
    (should (string-match-p "Café" (tiles--warc-payload-text
                                    archive (tiles-warc-test--record-at archive 4))))))

;;;; Resolving records

(ert-deftest tiles-warc-test-lookup ()
  (tiles-warc-test--with-archive (_file archive (apply #'concat (tiles-warc-test--records)))
    (let ((home (tiles-warc-test--record-at archive 2))
          (about (tiles-warc-test--record-at archive 4)))
      (should (eq (tiles--warc-lookup archive "http://example.com/") home))
      (should (eq (tiles--warc-lookup archive "http://EXAMPLE.com:80") home))
      (should (eq (tiles--warc-lookup archive "https://example.com/about/") about))
      (should (eq (tiles--warc-lookup archive "http://example.com/about#team") about))
      (should-not (tiles--warc-lookup archive "http://example.com/missing")))))

(ert-deftest tiles-warc-test-resolve ()
  (tiles-warc-test--with-archive (_file archive (apply #'concat (tiles-warc-test--records)))
    (let ((home (tiles-warc-test--record-at archive 2))
          (about (tiles-warc-test--record-at archive 4))
          (revisit (tiles-warc-test--record-at archive 5))
          (moved (tiles-warc-test--record-at archive 6)))
      (should (eq (tiles--warc-resolve archive revisit) home))
      (should (eq (tiles--warc-resolve archive moved) about))
      (should (eq (tiles--warc-resolve archive moved t) moved)))))

;;;; HTML

(ert-deftest tiles-warc-test-mark-hidden ()
  (should (equal (tiles--warc-mark-hidden
                  "<div hidden>a</div><p title='x hidden y'>b hidden c</p><i HIDDEN/>")
                 "<div hidden=\"\">a</div><p title='x hidden y'>b hidden c</p><i HIDDEN=\"\"/>")))

(ert-deftest tiles-warc-test-sanitize ()
  (let* ((dom (tiles--warc-sanitize (tiles--warc-parse-html tiles-warc-test--home-html)))
         (text (tiles--warc-dom-text dom)))
    (dolist (tag '(script style link img iframe video noscript))
      (should-not (dom-by-tag dom tag)))
    (should-not (dom-search dom (lambda (node)
                                  (seq-some (lambda (attribute)
                                              (not (memq (car attribute)
                                                         tiles--warc-kept-attributes)))
                                            (dom-attributes node)))))
    (dolist (hidden '("Hidden text" "Also hidden" "Enable JavaScript"
                      "Video fallback" "alert"))
      (should-not (string-search hidden text)))
    (should (string-search "Logo" text))
    (should-not (dom-attr (seq-find (lambda (a) (equal (tiles--warc-dom-text a) "script link"))
                                    (dom-by-tag dom 'a))
                          'href))))

(ert-deftest tiles-warc-test-org ()
  (let ((org (tiles-warc-test--home-org)))
    (dolist (expected
             '(":WARC-Target-URI: http://example.com/\n"
               "#+title: Home page\n"
               "\n* Welcome /home/\n"
               "\nSome *bold*, /italic/ and [[http://example.com/about][about us]].\n"
               "\nintra​*word*​bold and ~code~ and script link\n"
               "\n- One\n- Two\n  - Nested\n"
               "\n3. Three\n4. Four\n"
               "\n- Term :: Definition\n"
               "\n#+begin_example\n,* not a heading\n,#+not a keyword\n  indented\n#+end_example\n"
               "\n#+begin_quote\nQuoted text\n#+end_quote\n"
               "\n| Name | Value |\n|-\n| a | 1¦2 |\n"
               "[[http://example.com/logo][Logo]]"
               "[[http://elsewhere.example/][elsewhere]]"
               "ünïcödé"))
      (should (string-search expected org)))
    (dolist (unexpected '("Hidden" "Enable JavaScript" "Video fallback" "alert"
                          "A picture" "javascript:"))
      (should-not (string-search unexpected org)))
    ;; Org reads the markup as intended.  Adjacent lists are one list
    ;; for Org, as only two blank lines would end them.
    (with-temp-buffer
      (insert org)
      (org-mode)
      (let ((counts nil))
        (org-element-map (org-element-parse-buffer)
            '(headline bold italic link plain-list table example-block quote-block code)
          (lambda (element) (cl-incf (alist-get (org-element-type element) counts 0))))
        (should (equal (sort counts (lambda (a b) (string< (car a) (car b))))
                       '((bold . 2) (code . 1) (example-block . 1) (headline . 1)
                         (italic . 2) (link . 4) (plain-list . 2) (quote-block . 1)
                         (table . 1))))))))

(ert-deftest tiles-warc-test-org-layout-table ()
  "Tables used for layout are shown as a sequence of blocks."
  (let ((org (tiles--warc-page-org
              (list :uri "http://example.com/" :title "Layout" :base "http://example.com/"
                    :dom (tiles--warc-sanitize
                          (tiles--warc-parse-html
                           "<table><tr><td><h2>Menu</h2><ul><li>A</li></ul></td>
<td><p>Main text</p><table><tr><td>x</td><td>y</td></tr></table></td></tr></table>"))))))
    ;; The data table inside it is still a table.
    (should (string-search "\n** Menu\n\n- A\n\nMain text\n\n| x | y |\n" org))))

(ert-deftest tiles-warc-test-org-escapes ()
  "Text that looks like Org markup is kept as text."
  (let ((org (tiles--warc-page-org
              (list :uri "http://example.com/" :title "Escapes" :base "http://example.com/"
                    :dom (tiles--warc-parse-html
                          "<p>* not a heading<br>#+TITLE: not a keyword<br>| not a table</p>")))))
    (with-temp-buffer
      (insert org)
      (org-mode)
      (let ((tree (org-element-parse-buffer)))
        (should-not (org-element-map tree 'headline #'identity))
        (should-not (org-element-map tree 'table #'identity))
        (should (equal (org-element-map tree 'keyword
                         (lambda (keyword) (org-element-property :key keyword)))
                       '("TITLE")))))))

(ert-deftest tiles-warc-test-org-link-schemes ()
  "Only web and mail links of a page become Org links."
  (let* ((html "<p><a href=\"elisp:(delete-file x)\">Read more</a>
<a href=\"shell:touch x\">docs</a> <a href=\"file+sys:/Applications\">apps</a>
<a href=\"FILE:/etc/passwd\">file</a> <a href=\" javascript:x()\">script</a>
<a href=\"/about\">about</a> <a href=\"HTTPS://other.example/\">other</a>
<a href=\"mailto:a@example.com\">mail</a></p>")
         (org (tiles--warc-page-org
               (list :uri "http://example.com/" :title "Links" :base "http://example.com/"
                     :dom (tiles--warc-sanitize (tiles--warc-parse-html html)))))
         (links (lambda (org)
                  (with-temp-buffer
                    (insert org)
                    (org-mode)
                    (org-element-map (org-element-parse-buffer) 'link
                      (lambda (link) (org-element-property :raw-link link)))))))
    (should (equal (funcall links org)
                   '("http://example.com/about" "HTTPS://other.example/"
                     "mailto:a@example.com")))
    ;; The text of the other links stays.
    (dolist (text '("Read more" "docs" "apps" "file" "script"))
      (should (string-search text org)))
    ;; A base with another scheme cannot turn relative links into it,
    ;; and relative links without a base are not taken for files.
    (dolist (base '("elisp:(x)/" ""))
      (should-not (funcall links
                           (tiles--warc-page-org
                            (list :uri "http://example.com/" :title "Base" :base base
                                  :dom (tiles--warc-sanitize
                                        (tiles--warc-parse-html
                                         "<p><a href=\"/about\">about</a></p>")))))))))

;;;; Commands

(defmacro tiles-warc-test--with-listing (&rest body)
  "Run BODY in the list of records of the test archive."
  (declare (indent 0))
  `(tiles-warc-test--with-archive (file _archive (cdr (assoc "per-record" (tiles-warc-test--variants))))
     (let ((buffer (progn (tiles-warc-open file) (current-buffer))))
       (unwind-protect
           (progn ,@body)
         (dolist (other (buffer-list))
           (when (string-prefix-p "*tiles-warc" (buffer-name other))
             (kill-buffer other)))
         (when (buffer-live-p buffer)
           (kill-buffer buffer))))))

(defun tiles-warc-test--goto-uri (uri)
  "Move to the listed record of URI."
  (goto-char (point-min))
  (while (and (not (eobp))
              (not (equal (tiles-warc-record-uri (tabulated-list-get-id)) uri)))
    (forward-line 1))
  (should (tabulated-list-get-id)))

(ert-deftest tiles-warc-test-listing ()
  (tiles-warc-test--with-listing
    (should (derived-mode-p 'tiles-warc-mode))
    (should (= (length tabulated-list-entries) 7))
    (tiles-warc-toggle-all-records)
    (should (= (length tabulated-list-entries) 9))
    (tiles-warc-filter "about")
    (should (= (length tabulated-list-entries) 1))
    (tiles-warc-filter "")
    (should (= (length tabulated-list-entries) 9))))

(ert-deftest tiles-warc-test-org-view ()
  (tiles-warc-test--with-listing
    (tiles-warc-test--goto-uri "http://example.com/")
    (tiles-warc-show-org)
    (should (derived-mode-p 'org-mode))
    (should tiles-warc-view-mode)
    (should buffer-read-only)
    (should (string-search "#+title: Welcome home" (buffer-string)))
    ;; Links to archived pages open them.
    (goto-char (point-min))
    (search-forward "about us")
    (backward-char 2)
    (org-open-at-point)
    (should (equal (tiles-warc-record-uri tiles--warc-record) "http://example.com/about"))
    (should (string-search "#+title: About" (buffer-string)))
    (should (string-search "Café" (buffer-string)))
    ;; Links to other pages are not opened without asking.
    (let ((tiles-warc-follow-live-links nil)
          (browsed nil))
      (cl-letf (((symbol-function 'browse-url-default-browser)
                 (lambda (url &rest _) (setq browsed url))))
        (tiles--warc-follow-url "http://elsewhere.example/"))
      (should-not browsed))))

(ert-deftest tiles-warc-test-eww-view ()
  (tiles-warc-test--with-listing
    (tiles-warc-test--goto-uri "http://example.com/")
    ;; Without a display, shr cannot measure text drawn with fonts.
    (let ((shr-use-fonts nil))
      (tiles-warc-show-eww))
    (should (derived-mode-p 'tiles-warc-eww-mode))
    (should (string-search "Welcome home" (buffer-string)))
    (should-not (string-search "Hidden" (buffer-string)))
    (should (equal (plist-get eww-data :url) "http://example.com/"))
    (goto-char (point-min))
    (search-forward "about us")
    (backward-char 2)
    (should (eq (key-binding (kbd "RET")) #'tiles-warc-eww-follow-link))
    (let ((shr-use-fonts nil))
      (tiles-warc-eww-follow-link))
    (should (equal (plist-get eww-data :url) "http://example.com/about"))
    (should (string-search "Café" (buffer-string)))
    (eww-back-url)
    (should (equal (plist-get eww-data :url) "http://example.com/"))
    (should (string-search "Welcome home" (buffer-string)))))

(ert-deftest tiles-warc-test-open-records ()
  (tiles-warc-test--with-listing
    (tiles-warc-test--goto-uri "http://example.com/moved")
    (tiles-warc-show-org)
    (should (equal (tiles-warc-record-uri tiles--warc-record) "http://example.com/about"))
    (set-buffer buffer)
    (tiles-warc-test--goto-uri "http://example.com/notes.txt")
    (tiles-warc-open-record)
    (should (equal (buffer-string) "Plain notes\n"))
    (should (derived-mode-p 'text-mode))
    (set-buffer buffer)
    (tiles-warc-test--goto-uri "http://example.com/image.png")
    (should-error (tiles-warc-open-record) :type 'user-error)
    (tiles-warc-view-source)
    (should (string-search "WARC-Type: response" (buffer-string)))
    (should (string-search "HTTP/1.1 200 OK" (buffer-string)))
    (should (string-search "of image/png data]" (buffer-string)))))

(ert-deftest tiles-warc-test-save-payload ()
  (tiles-warc-test--with-listing
    (tiles-warc-test--goto-uri "http://example.com/")
    (let ((target (make-temp-file "tiles-warc-payload")))
      (unwind-protect
          (progn
            (delete-file target)
            (tiles-warc-save-payload (tiles--warc-current-archive) (tiles--warc-current-record)
                                 target)
            (should (equal (with-temp-buffer
                             (set-buffer-multibyte nil)
                             (insert-file-contents-literally target)
                             (buffer-string))
                           (tiles-warc-test--bytes tiles-warc-test--home-html))))
        (when (file-exists-p target)
          (delete-file target))))))

(ert-deftest tiles-warc-test-visit-file ()
  "Visiting a WARC file lists its records and never saves over it."
  (tiles-warc-test--with-archive (file _archive (apply #'concat (tiles-warc-test--records)))
    (let ((buffer (find-file-noselect file)))
      (unwind-protect
          (with-current-buffer buffer
            (should (derived-mode-p 'tiles-warc-mode))
            (should (= (length tabulated-list-entries) 7))
            (should-not buffer-file-name)
            (should-not (buffer-modified-p)))
        (kill-buffer buffer)))))

;;;; Single page archives

(defun tiles-warc-test--page-record (uri html &optional status)
  "Return a response record for URI with HTML and STATUS (200 by default)."
  (tiles-warc-test--record
   "response" uri
   (tiles-warc-test--http (format "%d X" (or status 200)) '(("Content-Type" . "text/html")) html)
   '("Content-Type" . "application/http; msgtype=response")))

(defun tiles-warc-test--file-record (uri type body)
  "Return a response record for URI with the content TYPE and BODY."
  (tiles-warc-test--record
   "response" uri
   (tiles-warc-test--http "200 OK" (list (cons "Content-Type" type)) body)
   '("Content-Type" . "application/http; msgtype=response")))

(defun tiles-warc-test--single-page-uri (&rest records)
  "Return the URI of the single page of an archive of RECORDS, or nil."
  (tiles-warc-test--with-archive (_file archive (apply #'concat records))
    (let ((page (tiles--warc-single-page archive)))
      (and page (tiles-warc-record-uri page)))))

(ert-deftest tiles-warc-test-single-page ()
  ;; The files that come with a page do not count.
  (should (equal (tiles-warc-test--single-page-uri
                  (tiles-warc-test--file-record "http://a.example/robots.txt" "text/plain" "x")
                  (tiles-warc-test--page-record "http://a.example/" "<h1>A</h1>")
                  (tiles-warc-test--file-record "http://a.example/a.css" "text/css" "p{}")
                  (tiles-warc-test--file-record "http://a.example/a.js" "application/javascript" "x()")
                  (tiles-warc-test--file-record "http://a.example/a.png" "image/png" "PNG")
                  (tiles-warc-test--page-record "http://a.example/b.png" "Too many" 429))
                 "http://a.example/"))
  ;; Redirects count as the page they lead to.
  (should (equal (tiles-warc-test--single-page-uri
                  (tiles-warc-test--record
                   "response" "http://a.example/old"
                   (tiles-warc-test--http "301 Moved" '(("Location" . "/new")) "")
                   '("Content-Type" . "application/http; msgtype=response"))
                  (tiles-warc-test--page-record "http://a.example/new" "<h1>New</h1>"))
                 "http://a.example/new"))
  ;; Error pages count when there is nothing else, and so does text.
  (should (equal (tiles-warc-test--single-page-uri
                  (tiles-warc-test--page-record "http://a.example/gone" "Gone" 404))
                 "http://a.example/gone"))
  (should (equal (tiles-warc-test--single-page-uri
                  (tiles-warc-test--file-record "http://a.example/a.txt" "text/plain" "Text")
                  (tiles-warc-test--file-record "http://a.example/a.png" "image/png" "PNG"))
                 "http://a.example/a.txt"))
  ;; Several pages, or nothing to show.
  (should-not (tiles-warc-test--single-page-uri
               (tiles-warc-test--page-record "http://a.example/" "<h1>A</h1>")
               (tiles-warc-test--page-record "http://a.example/b" "<h1>B</h1>")))
  (should-not (tiles-warc-test--single-page-uri
               (tiles-warc-test--file-record "http://a.example/a.png" "image/png" "PNG")))
  (should-not (tiles-warc-test--single-page-uri
               (tiles-warc-test--file-record "http://a.example/a.txt" "text/plain" "A")
               (tiles-warc-test--file-record "http://a.example/b.txt" "text/plain" "B"))))

(defconst tiles-warc-test--single-archive
  (concat (tiles-warc-test--record "warcinfo" nil "software: tiles-warc-tests\r\n"
                               '("Content-Type" . "application/warc-fields"))
          (tiles-warc-test--page-record
           "http://a.example/"
           "<title>Single</title><h1>Only page</h1><p><a href=\"/\">self</a></p>
<pre>;; Local Variables:
;; fill-column: 12
;; End:</pre>")
          (tiles-warc-test--file-record "http://a.example/a.png" "image/png" "PNG"))
  "An archive holding a single page.")

(defmacro tiles-warc-test--with-single (file &rest body)
  "Run BODY with FILE bound to an archive holding a single page."
  (declare (indent 1))
  `(tiles-warc-test--with-archive (,file _archive tiles-warc-test--single-archive)
     (unwind-protect
         (progn ,@body)
       (dolist (buffer (buffer-list))
         (when (or (string-prefix-p "*tiles-warc" (buffer-name buffer))
                   (string-suffix-p ".warc" (buffer-name buffer)))
           (kill-buffer buffer))))))

(ert-deftest tiles-warc-test-open-single-page ()
  (tiles-warc-test--with-single file
    (tiles-warc-open file)
    (should (derived-mode-p 'org-mode))
    (should tiles-warc-view-mode)
    (should (equal (tiles-warc-record-uri tiles--warc-record) "http://a.example/"))
    (should (string-search "Only page" (buffer-string)))
    ;; ^ lists the records, with point on the page.
    (should (eq (key-binding "^") #'tiles-warc-list-records))
    (tiles-warc-list-records)
    (should (derived-mode-p 'tiles-warc-mode))
    (should (= (length tabulated-list-entries) 2))
    (should (equal (tiles-warc-record-uri (tabulated-list-get-id)) "http://a.example/"))
    ;; Asking for the list gives the list.
    (tiles-warc-open file t)
    (should (derived-mode-p 'tiles-warc-mode))))

(ert-deftest tiles-warc-test-open-single-page-eww ()
  (tiles-warc-test--with-single file
    (let ((tiles-warc-default-view 'eww)
          (shr-use-fonts nil))
      (tiles-warc-open file))
    (should (derived-mode-p 'tiles-warc-eww-mode))
    (should (string-search "Only page" (buffer-string)))
    (should (eq (key-binding "^") #'tiles-warc-list-records))
    (tiles-warc-list-records)
    (should (derived-mode-p 'tiles-warc-mode))
    (should (equal (tiles-warc-record-uri (tabulated-list-get-id)) "http://a.example/"))))

(ert-deftest tiles-warc-test-visit-single-page ()
  "Visiting an archive holding a single page shows the page."
  (tiles-warc-test--with-single file
    (let ((buffer (find-file-noselect file)))
      (with-current-buffer buffer
        (should (derived-mode-p 'org-mode))
        (should tiles-warc-view-mode)
        (should-not buffer-file-name)
        (should-not (buffer-modified-p))
        (should (equal (buffer-name) (file-name-nondirectory file)))
        (should (string-search "Only page" (buffer-string)))
        ;; The archived page holds a local variables section, which
        ;; would take effect in a file...
        (let ((text (buffer-string)))
          (with-temp-buffer
            (insert text)
            (setq buffer-file-name (expand-file-name "page.org" temporary-file-directory))
            (hack-local-variables)
            (should (= fill-column 12))
            (setq buffer-file-name nil)))
        ;; ...but not here.
        (should-not (local-variable-p 'fill-column))
        (tiles-warc-list-records)
        (should (derived-mode-p 'tiles-warc-mode))))))

(ert-deftest tiles-warc-test-visit-new-file ()
  "Visiting a WARC file that does not exist yet gives an empty buffer."
  (let* ((file (make-temp-name (expand-file-name "tiles-warc-new" temporary-file-directory)))
         (file (concat file ".warc"))
         (buffer (find-file-noselect file)))
    (unwind-protect
        (with-current-buffer buffer
          (should (eq major-mode 'fundamental-mode))
          (should (equal buffer-file-name file)))
      (kill-buffer buffer))))

;;;; Visiting compressed archives

(defmacro tiles-warc-test--without-programs (&rest body)
  "Run BODY, failing if it runs an external program."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'call-process)
              (lambda (program &rest _) (error "Ran %s" program)))
             ((symbol-function 'call-process-region)
              (lambda (_start _end program &rest _) (error "Ran %s" program))))
     ,@body))

(defun tiles-warc-test--write-file (file contents)
  "Write the unibyte string CONTENTS to FILE."
  (let ((coding-system-for-write 'no-conversion))
    (with-temp-file file
      (set-buffer-multibyte nil)
      (insert contents))))

(ert-deftest tiles-warc-test-insert-gzip-file ()
  "Inserting a .warc.gz file inflates all its members with zlib."
  (let ((file (make-temp-file "tiles-warc-test" nil ".warc.gz"))
        (plain (cdr (assoc "plain" (tiles-warc-test--variants)))))
    (unwind-protect
        (dolist (variant '("per-record" "whole" "grouped" "misaligned"))
          (tiles-warc-test--write-file file (cdr (assoc variant (tiles-warc-test--variants))))
          (tiles-warc-test--without-programs
            (should (equal (with-temp-buffer
                             (set-buffer-multibyte nil)
                             (insert-file-contents file)
                             (buffer-string))
                           plain))
            ;; Parts of the file, as `insert-file-contents' gives them.
            (should (equal (with-temp-buffer
                             (set-buffer-multibyte nil)
                             (insert-file-contents file nil 100 400)
                             (buffer-string))
                           (substring plain 100 400)))
            ;; Reading the file literally does not inflate it.
            (should (equal (with-temp-buffer
                             (set-buffer-multibyte nil)
                             (insert-file-contents-literally file)
                             (buffer-string))
                           (cdr (assoc variant (tiles-warc-test--variants)))))))
      (delete-file file))))

(ert-deftest tiles-warc-test-visit-gzip-file ()
  "Visiting a .warc.gz file runs no program."
  (let ((file (make-temp-file "tiles-warc-test" nil ".warc.gz")))
    (unwind-protect
        (progn
          (tiles-warc-test--write-file file (tiles--warc-gzip `((,tiles-warc-test--single-archive . t))))
          (let ((buffer (tiles-warc-test--without-programs (find-file-noselect file))))
            (unwind-protect
                (with-current-buffer buffer
                  (should (derived-mode-p 'org-mode))
                  (should (string-search "Only page" (buffer-string)))
                  (should-not buffer-file-name))
              (kill-buffer buffer))))
      (delete-file file))))

;;;; Dired

(ert-deftest tiles-warc-test-dired-find-file ()
  (let* ((dir (make-temp-file "tiles-warc-dired" t))
         (single (expand-file-name "single.warc.gz" dir))
         (several (expand-file-name "several.warc" dir))
         (other (expand-file-name "notes.txt" dir))
         (dired-buffer nil))
    (unwind-protect
        (progn
          (tiles-warc-test--write-file single (tiles--warc-gzip `((,tiles-warc-test--single-archive . t))))
          (tiles-warc-test--write-file several (apply #'concat (tiles-warc-test--records)))
          (with-temp-file other (insert "Notes\n"))
          (setq dired-buffer (dired-noselect dir))
          ;; RET runs the command in Dired.
          (with-current-buffer dired-buffer
            (should (eq (key-binding (kbd "RET")) #'tiles-warc-dired-find-file)))
          (cl-flet ((open (file)
                      (set-buffer dired-buffer)
                      (dired-goto-file file)
                      (tiles-warc-dired-find-file)
                      (current-buffer)))
            (with-current-buffer (open single)
              (should (derived-mode-p 'org-mode))
              (should (string-search "Only page" (buffer-string))))
            (let ((listing (open several)))
              (with-current-buffer listing
                (should (derived-mode-p 'tiles-warc-mode))
                (should (= (length tabulated-list-entries) 7)))
              ;; The list of an archive is shown again, not read again.
              (should (eq (open several) listing)))
            (with-current-buffer (open other)
              (should (equal buffer-file-name other))
              (should (equal (buffer-string) "Notes\n")))))
      (dolist (buffer (buffer-list))
        (when (or (string-prefix-p "*tiles-warc" (buffer-name buffer))
                  (equal (buffer-local-value 'default-directory buffer)
                         (file-name-as-directory dir)))
          (kill-buffer buffer)))
      (delete-directory dir t))))

;;;; Compressing

(defun tiles-warc-test--gunzip (data)
  "Return DATA inflated by Emacs's zlib, or nil if it is not valid gzip data."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert data)
    (and (eq t (zlib-decompress-region (point-min) (point-max)))
         (buffer-string))))

(defun tiles-warc-test--random-bytes (size seed)
  "Return SIZE pseudo-random bytes made from SEED."
  (let ((bytes (make-string size 0))
        (state seed))
    (dotimes (i size)
      (setq state (logand (+ (* state 1103515245) 12345) #x7fffffff))
      (aset bytes i (logand (ash state -16) 255)))
    bytes))

(defconst tiles-warc-test--text
  (let ((words ["the" "archive" "record" "page" "Emacs" "<p>" "</p>" "<a href=\"/x\">"
                "and" "of" "gzip" "deflate" "\n" "WARC" "header" "body"]))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (dotimes (i 60000)
        (insert (aref words (% (* i 7919) (length words))) " "))
      (buffer-string)))
  "Text that compresses well, longer than a block of symbols.")

(ert-deftest tiles-warc-test-crc32 ()
  (tiles--warc-deflate-tables)
  (should (= (tiles--warc-crc32 "123456789") #xcbf43926))
  (should (= (tiles--warc-crc32 "") 0))
  (should (= (tiles--warc-crc32 "56789" (tiles--warc-crc32 "1234")) #xcbf43926)))

(ert-deftest tiles-warc-test-gzip-round-trip ()
  "Data compressed by TILES is inflated back by zlib."
  (dolist (parts
           `((("" . t))
             (("x" . t))
             ((,(make-string 70000 ?a) . t))
             ((,(apply #'unibyte-string (number-sequence 0 255)) . t))
             ((,(tiles-warc-test--random-bytes 70000 1) . t))
             ((,(tiles-warc-test--random-bytes 140000 2) . nil))
             ((,tiles-warc-test--text . t))
             (("HTTP/1.1 200 OK\r\n\r\n" . t)
              (,(tiles-warc-test--random-bytes 1000 3) . nil)
              ("" . t)
              (,(substring tiles-warc-test--text 0 5000) . t))))
    (let ((data (apply #'concat (mapcar #'car parts))))
      (should (equal (tiles-warc-test--gunzip (tiles--warc-gzip parts)) data)))))

(ert-deftest tiles-warc-test-gzip-ratio ()
  "Text shrinks, and data that does not shrink grows only a little."
  (let ((text (tiles--warc-gzip `((,tiles-warc-test--text . t))))
        (noise (tiles-warc-test--random-bytes 50000 4)))
    (should (< (length text) (/ (length tiles-warc-test--text) 10)))
    (should (< (length (tiles--warc-gzip `((,noise . t)))) (+ (length noise) 64)))
    (should (< (length (tiles--warc-gzip `((,noise . nil)))) (+ (length noise) 64)))))

(ert-deftest tiles-warc-test-compressible-p ()
  (should (tiles--warc-compressible-p '(("Content-Type" . "text/html"))))
  (should (tiles--warc-compressible-p '(("Content-Type" . "image/svg+xml"))))
  (should (tiles--warc-compressible-p nil))
  (should-not (tiles--warc-compressible-p '(("Content-Type" . "text/html")
                                        ("Content-Encoding" . "gzip"))))
  (should-not (tiles--warc-compressible-p '(("Content-Type" . "image/png"))))
  (should-not (tiles--warc-compressible-p '(("Content-Type" . "application/zip")))))

;;;; Capturing

(defvar tiles-warc-test--requests nil
  "Requests received by the test server, newest first.")

(defun tiles-warc-test--server (routes)
  "Start an HTTP server on the loopback interface and return it.
ROUTES is an alist of (PATH . RESPONSE).  A RESPONSE of `hang' never
answers, and one of (keep-open . TEXT) answers TEXT without closing
the connection; other responses are sent before closing it."
  (make-network-process
   :name "tiles-warc-test-server" :server t :host "127.0.0.1" :service t
   :family 'ipv4 :coding 'binary :noquery t :sentinel #'ignore
   :filter (lambda (process string)
             (let ((data (concat (or (process-get process :data) "") string)))
               (process-put process :data data)
               (when (string-match "\\`GET \\([^ ]+\\) [^\r\n]*\r\n\\(?:[^\r\n]+\r\n\\)*\r\n" data)
                 (push data tiles-warc-test--requests)
                 (pcase (cdr (assoc (match-string 1 data) routes))
                   ('hang nil)
                   (`(keep-open . ,text) (process-send-string process text))
                   (response
                    (process-send-string
                     process (or response "HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\n\r\n"))
                    (delete-process process))))))))

(defmacro tiles-warc-test--with-server (spec &rest body)
  "Run BODY with a test server; SPEC is (BASE-VAR ROUTES).
BASE-VAR is bound to the server's URL, without the final slash.  New
archives go to a temporary directory, bound to `tiles-warc-test--dir'."
  (declare (indent 1))
  `(let* ((server (tiles-warc-test--server ,(nth 1 spec)))
          (,(car spec) (format "http://127.0.0.1:%d" (process-contact server :service)))
          (tiles-warc-test--requests nil)
          (tiles-warc-test--dir (make-temp-file "tiles-warc-capture" t))
          (tiles-warc-capture-directory tiles-warc-test--dir))
     (unwind-protect
         (progn ,@body)
       (delete-process server)
       (dolist (buffer (buffer-list))
         (when (string-prefix-p "*tiles-warc" (buffer-name buffer))
           (kill-buffer buffer)))
       (delete-directory tiles-warc-test--dir t))))

(defvar tiles-warc-test--dir nil
  "Directory where the capture tests save archives.")

(defun tiles-warc-test--response (status headers body)
  "Return an HTTP response with STATUS, HEADERS and BODY, with its length."
  (tiles-warc-test--http status
                     (append headers (list (cons "Content-Length"
                                                 (length (tiles-warc-test--bytes body)))))
                     body))

(defconst tiles-warc-test--page
  "<html><head><title>Captured</title><script>x()</script></head>
<body><h1>Captured page</h1><p>Some <b>text</b>.</p></body></html>"
  "The page served by the test server.")

(defun tiles-warc-test--routes ()
  "Return the routes of the test server."
  (let ((page (tiles-warc-test--gzip (tiles-warc-test--bytes tiles-warc-test--page))))
    `(("/" . ,(tiles-warc-test--http "200 OK"
                                 '(("Content-Type" . "text/html; charset=utf-8")
                                   ("Content-Encoding" . "gzip")
                                   ("Transfer-Encoding" . "chunked"))
                                 (tiles-warc-test--chunked page 20)))
      ("/old" . ,(tiles-warc-test--response "301 Moved Permanently" '(("Location" . "/new")) ""))
      ("/new" . ,(tiles-warc-test--response "302 Found" '(("Location" . "/final")) ""))
      ("/final" . ,(tiles-warc-test--response "200 OK" '(("Content-Type" . "text/html"))
                                          "<h1>Final</h1>"))
      ("/loop" . ,(tiles-warc-test--response "302 Found" '(("Location" . "/loop")) ""))
      ("/open" . (keep-open . ,(tiles-warc-test--response "200 OK" '(("Content-Type" . "text/plain"))
                                                      "still open")))
      ("/hang" . hang))))

(defun tiles-warc-test--types (archive)
  "Return the types of the records of ARCHIVE."
  (mapcar #'tiles-warc-record-type (tiles-warc-archive-records archive)))

(ert-deftest tiles-warc-test-base32 ()
  (should (equal (tiles--warc-base32 "") ""))
  (should (equal (tiles--warc-base32 "f") "MY"))
  (should (equal (tiles--warc-base32 "fooba") "MZXW6YTB"))
  (should (equal (tiles--warc-base32 "foobar") "MZXW6YTBOI"))
  (should (equal (tiles--warc-digest "")
                 "sha1:3I42H3S6NNFQ2MSVX7XZKYAYSCX5QBYJ")))

(ert-deftest tiles-warc-test-record-id ()
  (let ((id (tiles--warc-record-id)))
    (should (string-match-p
             "\\`<urn:uuid:[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-4[0-9a-f]\\{3\\}-[89ab][0-9a-f]\\{3\\}-[0-9a-f]\\{12\\}>\\'"
             id))
    (should-not (equal id (tiles--warc-record-id)))))

(ert-deftest tiles-warc-test-capture-url ()
  (should (equal (tiles--warc-capture-url " example.com/a b#top ")
                 "https://example.com/a%20b"))
  (should (equal (tiles--warc-capture-url "HTTP://Example.COM") "http://example.com/"))
  (should (equal (tiles--warc-capture-url "https://example.com?x=1") "https://example.com/?x=1"))
  (should (equal (tiles--warc-capture-url "https://münchen.de/ä")
                 "https://xn--mnchen-3ya.de/%C3%A4"))
  (let ((url (tiles--warc-capture-url "https://example.com/a%20b?q=%26")))
    (should (equal (tiles--warc-capture-url url) url)))
  (should-error (tiles--warc-capture-url "ftp://example.com/") :type 'user-error)
  (should-error (tiles--warc-capture-url "mailto:someone@example.com") :type 'user-error)
  (should-error (tiles--warc-capture-url "https://") :type 'user-error)
  (should-error (tiles--warc-capture-url "  ") :type 'user-error))

(ert-deftest tiles-warc-test-clipboard-url ()
  (cl-letf (((symbol-function 'gui-get-selection)
             (lambda (&rest _) " https://example.com/from-clipboard\n")))
    (let ((kill-ring '("https://example.com/killed")))
      (should (equal (tiles--warc-clipboard-url) "https://example.com/from-clipboard"))))
  (cl-letf (((symbol-function 'gui-get-selection) (lambda (&rest _) "not a URL")))
    (let ((kill-ring '("https://example.com/killed")))
      (should (equal (tiles--warc-clipboard-url) "https://example.com/killed")))
    (let ((kill-ring '("two words https://example.com/")))
      (should-not (tiles--warc-clipboard-url))))
  (cl-letf (((symbol-function 'gui-get-selection) (lambda (&rest _) (error "No display"))))
    (let ((kill-ring nil))
      (should-not (tiles--warc-clipboard-url)))))

(ert-deftest tiles-warc-test-capture ()
  (tiles-warc-test--with-server (base (tiles-warc-test--routes))
    (let* ((file (tiles-warc-capture (concat base "/#top")))
           (archive (tiles-warc-read-archive file))
           (response (tiles-warc-test--record-at archive 2))
           (request (tiles-warc-test--record-at archive 3)))
      (should (file-in-directory-p file tiles-warc-test--dir))
      (should (string-match-p "/127\\.0\\.0\\.1-[0-9]\\{8\\}-[0-9]\\{6\\}\\.warc\\.gz\\'" file))
      (should (equal (tiles-warc-test--types archive) '("warcinfo" "response" "request")))
      ;; Each record is compressed on its own.
      (seq-doseq (record (tiles-warc-archive-records archive))
        (should (consp (tiles-warc-record-member record)))
        (should (zerop (tiles-warc-record-offset record))))
      ;; The request that was sent is archived.
      (should (equal (length tiles-warc-test--requests) 1))
      (should (equal (tiles--warc-record-block archive request) (car tiles-warc-test--requests)))
      (should (string-prefix-p (format "GET / HTTP/1.1\r\nHost: %s\r\n"
                                       (string-remove-prefix "http://" base))
                               (car tiles-warc-test--requests)))
      (should (string-search "Accept-Encoding: gzip\r\n" (car tiles-warc-test--requests)))
      ;; The response is archived as received.
      (should (equal (tiles-warc-record-uri response) (concat base "/")))
      (should (equal (tiles--warc-record-block archive response)
                     (cdr (assoc "/" (tiles-warc-test--routes)))))
      (should (equal (tiles-warc-record-header response "WARC-IP-Address") "127.0.0.1"))
      (should (equal (tiles-warc-record-header request "WARC-Concurrent-To")
                     (tiles-warc-record-header response "WARC-Record-ID")))
      (should (equal (tiles-warc-record-header response "WARC-Block-Digest")
                     (tiles--warc-digest (tiles--warc-record-block archive response))))
      (should (equal (tiles-warc-record-header response "WARC-Payload-Digest")
                     (tiles--warc-digest (tiles-warc-test--gzip
                                      (tiles-warc-test--bytes tiles-warc-test--page)))))
      (should (equal (tiles-warc-record-header request "WARC-Block-Digest")
                     (tiles--warc-digest (car tiles-warc-test--requests))))
      (should (string-match-p "\\`[0-9-]\\{10\\}T[0-9:]\\{8\\}Z\\'"
                              (tiles-warc-record-header response "WARC-Date")))
      ;; The viewer reads the page back.
      (should (equal (tiles--warc-record-payload archive response)
                     (tiles-warc-test--bytes tiles-warc-test--page)))
      (should (string-search "* Captured page"
                             (tiles--warc-page-org (tiles--warc-page archive response))))
      ;; The page is shown.
      (should (derived-mode-p 'org-mode))
      (should (equal (tiles-warc-record-uri tiles--warc-record) (concat base "/"))))))

(ert-deftest tiles-warc-test-capture-redirects ()
  (tiles-warc-test--with-server (base (tiles-warc-test--routes))
    (let* ((file (tiles-warc-capture (concat base "/old")))
           (archive (tiles-warc-read-archive file)))
      (should (equal (seq-filter #'identity
                                 (mapcar #'tiles-warc-record-uri (tiles-warc-archive-records archive)))
                     (mapcar (lambda (path) (concat base path))
                             '("/old" "/old" "/new" "/new" "/final" "/final"))))
      (should (equal (tiles-warc-record-uri
                      (tiles--warc-resolve archive (tiles--warc-lookup archive (concat base "/old"))))
                     (concat base "/final")))
      (should (equal (tiles-warc-record-uri tiles--warc-record) (concat base "/final"))))))

(ert-deftest tiles-warc-test-capture-errors ()
  (tiles-warc-test--with-server (base (tiles-warc-test--routes))
    (should-error (tiles-warc-capture (concat base "/loop")) :type 'user-error)
    (let ((tiles-warc-capture-timeout 1))
      (should-error (tiles-warc-capture (concat base "/hang")) :type 'user-error))
    (should-error (tiles-warc-capture "http://127.0.0.1:1/") :type 'user-error)
    (should-not (directory-files tiles-warc-test--dir nil "\\.warc\\(?:\\.gz\\)?\\'"))))

(ert-deftest tiles-warc-test-capture-keep-open ()
  "A response of known length is complete even if the server keeps the connection."
  (tiles-warc-test--with-server (base (tiles-warc-test--routes))
    (let* ((start (float-time))
           (file (tiles-warc-capture (concat base "/open")))
           (archive (tiles-warc-read-archive file)))
      (should (< (- (float-time) start) 5))
      (should (equal (tiles--warc-record-payload archive (tiles-warc-test--record-at archive 2))
                     "still open")))))

(ert-deftest tiles-warc-test-capture-add ()
  "Pages can be added to an existing archive."
  (tiles-warc-test--with-server (base (tiles-warc-test--routes))
    (let ((file (expand-file-name "collection.warc" tiles-warc-test--dir)))
      (should (equal (tiles-warc-capture (concat base "/") file) file))
      (should (derived-mode-p 'org-mode))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (tiles-warc-capture (concat base "/final") file))
      (should (equal (tiles-warc-test--types (tiles-warc-read-archive file))
                     '("warcinfo" "response" "request" "response" "request")))
      ;; The listing shows the new page.
      (should (= (length tabulated-list-entries) 2))
      (should (equal (tiles-warc-record-uri (tabulated-list-get-id)) (concat base "/final")))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
        (should-error (tiles-warc-capture (concat base "/") file) :type 'user-error))
      ;; The added records are not compressed either.
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally file)
        (should (string-prefix-p "WARC/1.1\r\n" (buffer-string))))
      ;; Pages are added to compressed archives compressed.
      (let ((compressed (expand-file-name "collection.warc.gz" tiles-warc-test--dir)))
        (tiles-warc-capture (concat base "/") compressed)
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (tiles-warc-capture (concat base "/final") compressed))
        (let ((archive (tiles-warc-read-archive compressed)))
          (should (equal (tiles-warc-test--types archive)
                         '("warcinfo" "response" "request" "response" "request")))
          (seq-doseq (record (tiles-warc-archive-records archive))
            (should (consp (tiles-warc-record-member record))))))
      ;; Other files are left alone.
      (let ((other (expand-file-name "notes.txt" tiles-warc-test--dir))
            (other-gzip (expand-file-name "notes.txt.gz" tiles-warc-test--dir)))
        (with-temp-file other (insert "Not an archive\n"))
        (let ((coding-system-for-write 'no-conversion))
          (with-temp-file other-gzip
            (set-buffer-multibyte nil)
            (insert (tiles--warc-gzip '(("Not an archive\n" . t))))))
        (should-error (tiles-warc-capture (concat base "/") other) :type 'user-error)
        (should-error (tiles-warc-capture (concat base "/") other-gzip) :type 'user-error)))))

(ert-deftest tiles-warc-test-capture-uncompressed ()
  (tiles-warc-test--with-server (base (tiles-warc-test--routes))
    (let* ((tiles-warc-capture-compress nil)
           (file (tiles-warc-capture (concat base "/final"))))
      (should (string-suffix-p ".warc" file))
      (should (eq (tiles--warc-archive-format file) 'warc))
      (should (equal (tiles-warc-test--types (tiles-warc-read-archive file))
                     '("warcinfo" "response" "request"))))))

(ert-deftest tiles-warc-test-capture-interactive ()
  "The command offers the URL in the clipboard."
  (tiles-warc-test--with-server (base (tiles-warc-test--routes))
    (let (prompt)
      (cl-letf (((symbol-function 'gui-get-selection) (lambda (&rest _) (concat base "/final")))
                ((symbol-function 'read-string)
                 (lambda (text &optional _initial _history default &rest _)
                   (setq prompt text)
                   default)))
        (let ((current-prefix-arg nil))
          (call-interactively #'tiles-warc-capture)))
      (should (string-search (concat base "/final") prompt))
      (should (equal (length (directory-files tiles-warc-test--dir nil "\\.warc\\(?:\\.gz\\)?\\'")) 1))
      (should (equal (tiles-warc-record-uri tiles--warc-record) (concat base "/final")))
      ;; With a prefix argument, the archive is asked for.
      (let ((chosen (expand-file-name "chosen.warc" tiles-warc-test--dir))
            initial)
        (cl-letf (((symbol-function 'read-string)
                   (lambda (&rest _) (concat base "/")))
                  ((symbol-function 'read-file-name)
                   (lambda (_prompt _dir _default _mustmatch name &rest _)
                     (setq initial name)
                     chosen)))
          (let ((current-prefix-arg '(4)))
            (call-interactively #'tiles-warc-capture)))
        (should (string-match-p "\\`127\\.0\\.0\\.1-.*\\.warc\\.gz\\'" initial))
        (should (file-exists-p chosen))))))

(defun tiles-warc-test--capture-in-background (url file)
  "Capture URL in FILE with `tiles-warc-capture-in-background' and wait.
Return the list of arguments the callback got."
  (let (result)
    (tiles-warc-capture-in-background url file (lambda (&rest args) (setq result args)))
    ;; The capture has only started.
    (should-not result)
    (with-timeout (10 (error "The capture did not finish"))
      (while (not result)
        (accept-process-output nil 0.05)))
    result))

(ert-deftest tiles-warc-test-capture-in-background ()
  (tiles-warc-test--with-server (base (tiles-warc-test--routes))
    (let* ((buffers (buffer-list))
           (file (expand-file-name "sub/page.warc.gz" tiles-warc-test--dir)))
      (should (equal (tiles-warc-test--capture-in-background (concat base "/old#top") file)
                     (list file 200 nil)))
      ;; Nothing is shown, and nothing is left behind.
      (should (equal (buffer-list) buffers))
      (should-not (seq-some (lambda (process)
                              (string-prefix-p "tiles-warc-capture" (process-name process)))
                            (process-list)))
      (should (eq (tiles--warc-archive-format file) 'gzip))
      (should (equal (tiles-warc-archived-urls file)
                     (mapcar (lambda (path) (concat base path)) '("/old" "/new" "/final"))))
      ;; FILE may be a function of the status code and media type.
      (let (calls)
        (should (equal (tiles-warc-test--capture-in-background
                        (concat base "/missing")
                        (lambda (status type) (push (cons status type) calls) nil))
                       '(nil 404 nil)))
        (should (equal (tiles-warc-test--capture-in-background
                        (concat base "/")
                        (lambda (status type) (push (cons status type) calls) file))
                       (list file 200 nil)))
        (should (equal calls '((200 . "text/html") (404 . "")))))
      (should (equal (tiles-warc-archived-urls file)
                     (mapcar (lambda (path) (concat base path))
                             '("/old" "/new" "/final" "/"))))
      (should (equal (length (directory-files tiles-warc-test--dir nil "\\.warc")) 0)))))

(ert-deftest tiles-warc-test-capture-in-background-errors ()
  (tiles-warc-test--with-server (base (tiles-warc-test--routes))
    (let ((file (expand-file-name "page.warc.gz" tiles-warc-test--dir)))
      (pcase-let ((`(,archive ,status ,problem)
                   (tiles-warc-test--capture-in-background (concat base "/loop") file)))
        (should-not archive)
        (should-not status)
        (should (string-prefix-p "Redirect loop" problem)))
      (let ((tiles-warc-capture-timeout 1))
        (should (string-prefix-p "No data from"
                                 (nth 2 (tiles-warc-test--capture-in-background
                                         (concat base "/hang") file)))))
      (should (string-prefix-p "Cannot connect"
                               (nth 2 (tiles-warc-test--capture-in-background
                                       "http://127.0.0.1:1/" file))))
      (should-not (file-exists-p file))
      (should-error (tiles-warc-capture-in-background "mailto:someone@example.com" file)
                    :type 'user-error))))

(ert-deftest tiles-warc-test-capture-in-background-stop ()
  "The returned function stops the capture, and the callback is not called."
  (tiles-warc-test--with-server (base (tiles-warc-test--routes))
    (let* ((called nil)
           (stop (tiles-warc-capture-in-background
                  (concat base "/hang") (expand-file-name "page.warc" tiles-warc-test--dir)
                  (lambda (&rest _) (setq called t)))))
      (with-timeout (10 (error "The server got no request"))
        (while (not tiles-warc-test--requests)
          (accept-process-output nil 0.05)))
      (funcall stop)
      (let ((tiles-warc-capture-timeout 0))
        (accept-process-output nil 0.5))
      (should-not called)
      (should-not (seq-some (lambda (process)
                              (string-prefix-p "tiles-warc-capture" (process-name process)))
                            (process-list))))))

;;;; Main content

(defun tiles-warc-test--long (words)
  "Return a sentence long enough to be a paragraph, starting with WORDS."
  (concat words ", which goes on long enough to count as a real paragraph of text,"
          " with commas, as articles have, and more words, and more, to be sure of it."))

(defun tiles-warc-test--article (&optional wrapper)
  "Return a page whose article is surrounded by what is not content.
WRAPPER, if non-nil, is the opening tag of an element around the article."
  (concat
   "<html><head><title>Site: The title</title>"
   "<style>" (make-string 30 ?x) ", a, b, c, d, e, f, g, h, i, j, k</style></head><body>"
   "<div class=\"cookie-consent\">We use cookies, please accept them all.</div>"
   "<header class=\"site-header\"><ul><li><a href=\"/n\">News</a></li>"
   "<li><a href=\"/a\">About</a></li></ul></header>"
   "<nav><a href=\"/m1\">Menu one</a> <a href=\"/m2\">Menu two</a></nav>"
   (or wrapper "<div>")
   "<article><header><h1>The real title</h1><p>By A. Writer</p></header>"
   "<p>" (tiles-warc-test--long "First paragraph") "</p>"
   "<div class=\"share-buttons\"><a href=\"/s1\">Share on X</a> <a href=\"/s2\">Share it</a></div>"
   "<p>" (tiles-warc-test--long "Second paragraph") "</p>"
   "<p>" (tiles-warc-test--long "Third paragraph") "</p>"
   "<div class=\"related-posts\"><article><h3><a href=\"/r1\">Related story</a></h3></article></div>"
   "</article></div>"
   "<aside><p>" (tiles-warc-test--long "Sidebar text") "</p></aside>"
   "<section id=\"comments\">"
   "<article class=\"comment\"><p>" (tiles-warc-test--long "First comment") "</p></article>"
   "<article class=\"comment\"><p>" (tiles-warc-test--long "Second comment") "</p></article>"
   "</section><footer><p>Copyright notice of the site</p></footer></body></html>"))

(defun tiles-warc-test--page-org (html &optional readable)
  "Return the Org document for the page HTML, READABLE or not."
  (tiles-warc-test--with-archive (_file archive (tiles-warc-test--page-record "http://site.example/" html))
    (tiles--warc-page-org (tiles--warc-page archive (tiles-warc-test--record-at archive 1) readable))))

(ert-deftest tiles-warc-test-readable ()
  (let ((org (tiles-warc-test--page-org (tiles-warc-test--article) t)))
    (should (string-search "#+title: The real title\n" org))
    ;; The title is not repeated as a heading.
    (should-not (string-search "* The real title" org))
    (dolist (kept '("By A. Writer" "First paragraph" "Second paragraph" "Third paragraph"))
      (should (string-search kept org)))
    (dolist (dropped '("cookies" "News" "Menu one" "Share on X" "Related story"
                       "Sidebar text" "First comment" "Copyright" "xxxx"))
      (should-not (string-search dropped org))))
  ;; Without it, all of the page is shown.
  (let ((org (tiles-warc-test--page-org (tiles-warc-test--article))))
    (should (string-search "#+title: Site: The title\n" org))
    (dolist (text '("Menu one" "Sidebar text" "First comment" "Third paragraph"))
      (should (string-search text org)))))

(ert-deftest tiles-warc-test-readable-keeps-the-article ()
  ;; Class names that look like clutter, around the article.
  (should (string-search "Second paragraph"
                         (tiles-warc-test--page-org
                          (tiles-warc-test--article "<div class=\"grid-container-extrawide sidebar-layout\">")
                          t)))
  ;; Broken markup that puts the page in its header.
  (should (string-search "Second paragraph"
                         (tiles-warc-test--page-org
                          (tiles-warc-test--article "<header class=\"site-header\"><main>") t)))
  ;; A page all in a form, with a table of data with links.
  (let ((org (tiles-warc-test--page-org
              (concat "<html><body><nav><a href=\"/x\">Menu</a></nav><form>"
                      "<p>" (tiles-warc-test--long "The list below") "</p>"
                      "<table><tr><td><a href=\"/o1\">Object one</a></td><td>12</td></tr>"
                      "<tr><td><a href=\"/o2\">Object two</a></td><td>34</td></tr></table>"
                      "</form></body></html>")
              t)))
    (should (string-search "Object two" org))
    (should-not (string-search "Menu" org)))
  ;; No article, as behind a paywall: the page without its clutter.
  (let ((org (tiles-warc-test--page-org
              "<html><body><header><a href=\"/\">Site menu</a></header><p>Only a teaser.</p>
<footer>Footer text</footer></body></html>"
              t)))
    (should (string-search "Only a teaser" org))
    (should-not (string-search "Site menu" org))
    (should-not (string-search "Footer text" org))))

(ert-deftest tiles-warc-test-toggle-readable ()
  (tiles-warc-test--with-archive (file _archive (tiles-warc-test--page-record "http://site.example/"
                                                                    (tiles-warc-test--article)))
    (unwind-protect
        (progn
          (tiles-warc-open file)
          (should (derived-mode-p 'org-mode))
          (should tiles--warc-main-content-shown)
          (should-not (string-search "Menu one" (buffer-string)))
          (should (eq (key-binding "R") #'tiles-warc-toggle-readable))
          (tiles-warc-toggle-readable)
          (should-not tiles--warc-main-content-shown)
          (should (string-search "Menu one" (buffer-string)))
          (should (derived-mode-p 'org-mode))
          (should buffer-read-only)
          (tiles-warc-toggle-readable)
          (should tiles--warc-main-content-shown)
          (should-not (string-search "Menu one" (buffer-string)))
          ;; With `tiles-warc-readable' off, pages open whole.
          (kill-buffer)
          (let ((tiles-warc-readable nil))
            (tiles-warc-open file)
            (should (string-search "Menu one" (buffer-string)))))
      (dolist (buffer (buffer-list))
        (when (string-prefix-p "*tiles-warc" (buffer-name buffer))
          (kill-buffer buffer))))))

(ert-deftest tiles-warc-test-canonical-url ()
  (should (equal (tiles-warc-canonical-url "https://Example.com/a b#top") "https://example.com/a%20b"))
  (should-not (tiles-warc-canonical-url "mailto:someone@example.com"))
  (should-not (tiles-warc-canonical-url "file:///etc/hosts")))

(provide 'tiles-warc-tests)
;;; tiles-warc-tests.el ends here
