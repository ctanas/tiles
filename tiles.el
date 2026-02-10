;;; tiles.el --- Tagged Instant Lightweight Emacs Snippets -*- lexical-binding: t; -*-

;; Copyright 2026 Claudiu Tănăselia
;; distributed under the terms of the GNU General Public License (Version 3, 29 June 2007)

;; Author: Claudiu Tănăselia
;; Version: 0.3.3
;; Package-Requires: ((emacs "27.1"))
;; Keywords: notes, org
;; URL: https://github.com/ctanas/tiles

;;; Commentary:

;; TILES (Tagged Instant Lightweight Emacs Snippets) is a note-taking
;; system where each note is a single paragraph stored in its own file.
;; Notes are organized through tags and bold keywords rather than
;; hierarchies or titles.
;;
;; Note file format (TYYYYMMDDHHMMSS.org):
;;
;;   This is the paragraph.  *Bold words* are searchable keywords.
;;   /Italic/ and [[links]] are supported.  Inline footnotes[fn:: like
;;   this] are stripped from previews.
;;
;;   && This is a private paragraph.  It is hidden from previews,
;;   stitched views, and dynamic blocks.  Only visible when expanding
;;   a note with TAB in the dashboard or editing the file directly.
;;
;;   tag1/tag2/tag3
;;
;; The last non-empty line is always the tag line; everything above
;; (separated by a blank line) is content.  Multi-paragraph notes work.
;; Paragraphs starting with && are private (hidden from all views
;; except TAB expansion and direct editing).
;;
;; Features:
;; - Atomic notes: one paragraph per file, no titles needed
;; - Timestamp naming: files named TYYYYMMDDHHMMSS.org
;; - Tag-based organization via slash-separated tag lines
;; - Keyword extraction: bold words (*word*) are searchable; hyphens
;;   are normalized to spaces for matching (e.g. *Falcon-9* and
;;   *Falcon 9* are the same keyword) but note content is unchanged
;; - Dashboard (tiles-show-notes): chronological listing with
;;   color-coded timestamps, inline org preview, editable split,
;;   tag/keyword filtering, tag exclusion, and date editing
;; - Moon phase in dashboard status line (days until next New/Full Moon)
;; - Tag search with AND/OR logic (/ = AND, space = OR)
;; - Keyword search with OR logic (space-separated terms)
;; - Two-panel search view with live preview
;; - Stitched view: search results as flowing text
;; - Quick capture via minibuffer (tiles-quick, tiles-yank)
;; - Touch command to bump a note's timestamp (tiles-touch)
;; - Private paragraphs: && prefix hides content from all views
;;   except TAB expansion and direct file editing
;; - Focus mode: distraction-free editing with centered ~80-char
;;   body, olivetti-style (tiles-focus-mode, enabled by default)
;; - Tag/keyword listing with occurrence counts and sorting
;; - In-memory cache with mtime invalidation for fast repeated access
;; - Org dynamic blocks for embedding note lists/contents in org files
;;
;; Global keybindings (under C-c m):
;;   C-c m m   - Dashboard (tiles-show-notes)
;;   C-c m n   - New note (tiles-new)
;;   C-c m q   - Quick capture via minibuffer (tiles-quick)
;;   C-c m y   - Quick capture from clipboard (tiles-yank)
;;   C-c m t   - Tag search (tiles-tag-search)
;;   C-c m k   - Keyword search (tiles-keyword-search)
;;
;; Dashboard keybindings:
;;   n/p       - Navigate notes
;;   SPC       - Open editable preview split (updates on navigation)
;;   RET       - Open note file
;;   TAB       - Toggle expanded view (private &&, keywords, stats)
;;   M-up/down - Reorder notes
;;   d         - Change note date/timestamp
;;   u         - Touch (update timestamp to now)
;;   D         - Delete note (with confirmation)
;;   t         - Filter by tag
;;   k         - Filter by keyword
;;   T         - List all tags
;;   K         - List all keywords
;;   F         - Exclude tags (hide notes with specified tags)
;;   c         - Clear search filter (keeps exclusion)
;;   C         - Clear tag exclusion (keeps search filter)
;;   f         - Toggle raw preview (strip org formatting)
;;   +         - Load next batch of notes
;;   0         - Stitch displayed notes (respects reordering)
;;   l         - New note (same as C-c m n)
;;   g         - Refresh
;;   q         - Quit
;;
;; Capture keybindings:
;;   C-c C-c   - Save note (prompts for tags if missing)
;;   C-c C-k   - Cancel and discard
;;
;; Search view keybindings:
;;   n/p       - Navigate results
;;   RET       - Open note
;;   SPC       - Toggle to stitched view
;;   r         - Refine search
;;   t/k       - Switch to tag/keyword search
;;   q         - Quit
;;
;; Stitched view keybindings:
;;   n/p       - Jump between note boundaries
;;   RET/e     - Open source file at point
;;   SPC       - Toggle back to two-panel view
;;   r         - Refine search
;;   q         - Quit
;;
;; Tag search syntax:
;;   /         - AND (all tags in group must match)
;;   space     - OR (any group matches)
;;   Example: "b218/lx2026 misc" = (b218 AND lx2026) OR misc
;;
;; Keyword search syntax:
;;   space     - OR (any term matches)
;;   Example: "emacs lisp" = emacs OR lisp
;;
;; Other commands (M-x):
;;   tiles-touch       - Update a note's timestamp to now (renames file)
;;   tiles-focus-mode  - Toggle distraction-free editing (~80-char centered)
;;   tiles-list-tags   - Browse all unique tags with occurrence counts
;;   tiles-list-keywords - Browse all unique keywords with occurrence counts
;;   tiles-clear-cache - Force reload of all note data from disk
;;   tiles-show-notes  - Open the dashboard (also C-c m m)
;;   tiles-new         - Create a new note (also C-c m n)
;;   tiles-focus-mode  - Toggle focus mode for distraction-free editing
;;   tiles-capture     - Alias for tiles-new
;;   tiles-quick       - Quick capture via minibuffer (also C-c m q)
;;   tiles-yank        - Quick capture from clipboard (also C-c m y)
;;   tiles-tag-search  - Search by tags (also C-c m t)
;;   tiles-keyword-search - Search by keywords (also C-c m k)
;;
;; Org dynamic blocks:
;;   #+BEGIN: tiles-notes :tags "b218/lx2026" :sort "newest" :limit 10
;;   (generates a linked list of matching notes)
;;   #+END:
;;
;;   #+BEGIN: tiles-files :tags "journal" :separator "\n-----\n"
;;   (embeds note contents directly)
;;   #+END:
;;
;; Update blocks with C-c C-x C-u, insert with C-c C-x x.
;;
;; Changelog:
;;
;;   0.3.2 - Focus mode for distraction-free editing (~80-char centered).
;;           Interactive tag/keyword lists with occurrence counts and
;;           sorting (alphabetical/occurrence, ascending/descending).
;;           Dashboard keybindings: T list tags, K list keywords,
;;           u touch, L new note with focus mode.  Stitch confirmation
;;           when no filter is active.
;;   0.3.1 - Red & indicator for notes with private paragraphs.
;;   0.3   - Private paragraphs: paragraphs starting with && are hidden
;;           from dashboard previews, stitched views, search panels, and
;;           dynamic blocks.  Only visible when expanding a note with TAB
;;           in the dashboard or editing the file directly.
;;   0.2   - Initial public release.

;;; Code:

(require 'org)
(require 'lunar)

(defconst tiles-version "0.3.3"
  "Current version of TILES.")

;;; Customization

(defgroup tiles nil
  "Tagged Instant Lightweight Emacs Snippets."
  :group 'text
  :prefix "tiles-")

(defcustom tiles-directory "~/notes/tiles/"
  "Directory where TILES notes are stored.
Notes are loaded recursively from subdirectories."
  :type 'directory
  :group 'tiles)

(defcustom tiles-preview-length 105
  "Maximum number of characters for the inline note preview in the dashboard."
  :type 'integer
  :group 'tiles)

(defcustom tiles-preview-raw t
  "When non-nil, strip all org-mode formatting from dashboard previews.
Shows plain text without bold, italic, links, or footnotes markup."
  :type 'boolean
  :group 'tiles)

(defcustom tiles-dashboard-limit 50
  "Maximum number of notes to display per page in the dashboard.
Press `+' to load the next batch.  Set to nil for unlimited."
  :type '(choice integer (const nil))
  :group 'tiles)

(defcustom tiles-line-padding 22
  "Extra padding added to the line width beyond preview and tags.
The total dashboard line width is `tiles-preview-length' + this value
+ the length of the longest tag string."
  :type 'integer
  :group 'tiles)

(defcustom tiles-focus-default t
  "When non-nil, enable focus mode by default when creating new notes.
Set to nil to disable: (setq tiles-focus-default nil)"
  :type 'boolean
  :group 'tiles)

(defcustom tiles-fancy-separators t
  "When non-nil, use Unicode box-drawing characters for dashboard separators.
Uses ═ and ─ instead of = and -.  Set to nil for plain ASCII."
  :type 'boolean
  :group 'tiles)

;;; Internal Variables

(defvar tiles--current-search-results nil
  "Current list of search results (file paths).")

(defvar tiles--current-search-query nil
  "Current search query string.")

(defvar tiles--current-search-type nil
  "Current search type: `tag' or `keyword'.")

(defvar tiles--cache (make-hash-table :test 'equal)
  "Cache of parsed note data, keyed by filepath.
Values are (MTIME . PARSED-DATA).")

(defvar tiles--preview-buffer-name "*Tile Preview*"
  "Name of the preview buffer.")

(defvar tiles--search-buffer-name "*Tiles Search Results*"
  "Name of the search results buffer.")

(defvar tiles--stitched-buffer-name "*Tiles Stitched View*"
  "Name of the stitched view buffer.")

;;; Utility Functions

(defun tiles--next-lunar-event ()
  "Return a string describing the next New Moon or Full Moon.
Uses Emacs's built-in lunar phase computation."
  (let* ((now (current-time))
         (today (decode-time now))
         (month (nth 4 today))
         (year (nth 5 today))
         (best-name nil)
         (best-date nil)
         (best-days nil))
    ;; Check this month and next 2 months
    (catch 'found
      (dolist (m (list (list month year)
                       (list (1+ (mod month 12)) (if (= month 12) (1+ year) year))
                       (list (1+ (mod (1+ month) 12)) (if (>= month 11) (1+ year) year))))
        (let ((phases (lunar-phase-list (car m) (cadr m))))
          (dolist (phase phases)
            (let* ((date-list (car phase))
                   (raw-phase (nth 2 phase))
                   (name (if (stringp raw-phase) raw-phase
                           (nth raw-phase '("New Moon" "First Quarter Moon"
                                            "Full Moon" "Last Quarter Moon"))))
                   (phase-time (encode-time 0 0 12
                                            (nth 1 date-list)
                                            (nth 0 date-list)
                                            (nth 2 date-list)))
                   (diff-days (floor (/ (float-time (time-subtract phase-time now)) 86400.0))))
              (when (and name (>= diff-days 0)
                         (or (string-match-p "New Moon" name)
                             (string-match-p "Full Moon" name))
                         (or (not best-days) (< diff-days best-days)))
                (setq best-name name
                      best-date (format-time-string "%a, %d %B %Y" phase-time)
                      best-days diff-days)))))))
    (when best-name
      (let ((short (if (string-match-p "New" best-name) "NEW" "FULL")))
        (cond
         ((= best-days 0)
          (format "%s MOON TODAY!" short))
         ((= best-days 1)
          (format "%s Moon Tomorrow!" best-name))
         (t
          (format "%d days until %s: %s" best-days best-name best-date)))))))

(defun tiles--ensure-directory ()
  "Ensure `tiles-directory' exists."
  (unless (file-exists-p tiles-directory)
    (make-directory tiles-directory t)))

(defun tiles--generate-timestamp ()
  "Generate a timestamp string for file naming."
  (format-time-string "T%Y%m%d%H%M%S"))

(defun tiles--generate-filename ()
  "Generate a new filename with timestamp."
  (concat (tiles--generate-timestamp) ".org"))

(defun tiles--get-all-tile-files ()
  "Get all tile files from `tiles-directory' and subdirectories, sorted newest first."
  (when (file-exists-p tiles-directory)
    (let ((files (directory-files-recursively tiles-directory "^T[0-9]\\{14\\}\\.org$")))
      (sort files (lambda (a b)
                    (string> (file-name-nondirectory a)
                             (file-name-nondirectory b)))))))

(defun tiles--parse-note-file (filepath)
  "Parse a TILES note file at FILEPATH.
Returns a plist with :content, :tags, and :keywords.
Results are cached and only re-parsed when the file's mtime changes."
  (when (file-exists-p filepath)
    (let* ((mtime (file-attribute-modification-time (file-attributes filepath)))
           (cached (gethash filepath tiles--cache)))
      (if (and cached (equal (car cached) mtime))
          (cdr cached)
        (let ((result (tiles--parse-note-file-uncached filepath)))
          (puthash filepath (cons mtime result) tiles--cache)
          result)))))

(defun tiles--parse-note-file-uncached (filepath)
  "Parse a TILES note file at FILEPATH without caching.
The last non-empty line is always treated as the tag line.
Everything before it (minus the trailing blank separator) is content."
  (with-temp-buffer
    (insert-file-contents filepath)
    (let* ((content (buffer-string))
           (lines (split-string content "\n"))
           (tag-line nil)
           (paragraph-lines nil))
      ;; Find the last non-empty line as the tag line
      (let ((i (1- (length lines))))
        (while (and (>= i 0) (string-empty-p (string-trim (nth i lines))))
          (setq i (1- i)))
        (when (>= i 0)
          (setq tag-line (string-trim (nth i lines)))
          ;; Everything before it is content (strip trailing blank lines)
          (setq i (1- i))
          (while (and (>= i 0) (string-empty-p (string-trim (nth i lines))))
            (setq i (1- i)))
          (setq paragraph-lines (seq-take lines (1+ i)))))
      (let* ((paragraph (mapconcat #'identity paragraph-lines "\n"))
             (tags (when tag-line
                     (split-string tag-line "/" t "[ \t]+")))
             (keywords (tiles--extract-keywords
                       (tiles--strip-private-paragraphs paragraph))))
        (list :content paragraph
              :tags tags
              :keywords keywords
              :filepath filepath)))))

(defun tiles-clear-cache ()
  "Clear the tiles note cache."
  (interactive)
  (clrhash tiles--cache)
  (message "Tiles cache cleared"))

(defun tiles--private-paragraph-p (paragraph)
  "Return non-nil if PARAGRAPH starts with &&."
  (string-prefix-p "&&" (string-trim-left paragraph)))

(defun tiles--strip-private-paragraphs (content)
  "Remove paragraphs starting with && from CONTENT."
  (let* ((paras (split-string content "\n\n+" t))
         (public (seq-remove #'tiles--private-paragraph-p paras)))
    (mapconcat #'identity public "\n\n")))

(defun tiles--extract-private-paragraphs (content)
  "Extract paragraphs starting with && from CONTENT, with && prefix removed."
  (let* ((paras (split-string content "\n\n+" t))
         (private (seq-filter #'tiles--private-paragraph-p paras)))
    (mapcar (lambda (p) (string-trim (substring (string-trim-left p) 2)))
            private)))

(defun tiles--normalize-keyword (kw)
  "Normalize keyword KW by replacing hyphens with spaces."
  (replace-regexp-in-string "-" " " kw))

(defun tiles--extract-keywords (text)
  "Extract bold keywords from TEXT.
Bold words in org-mode are surrounded by *asterisks*.
Hyphens are normalized to spaces so \"Falcon-9\" and \"Falcon 9\" are equivalent."
  (let ((keywords nil)
        (pos 0))
    (while (string-match "\\*\\([^*\n]+\\)\\*" text pos)
      (push (tiles--normalize-keyword (match-string 1 text)) keywords)
      (setq pos (match-end 0)))
    (delete-dups (nreverse keywords))))

(defun tiles--parse-tag-query (query)
  "Parse QUERY into AND-groups for tag matching.
`/' means AND (all tags must match), space means OR (any group matches).
\"b218/lx2026 misc\" becomes ((\"b218\" \"lx2026\") (\"misc\"))."
  (let ((or-parts (split-string query " " t)))
    (mapcar (lambda (part) (split-string part "/" t))
            or-parts)))

(defun tiles--tag-present-p (qtag note-tags)
  "Check if QTAG matches any tag in NOTE-TAGS."
  (seq-some (lambda (ntag)
              (string-match-p (regexp-quote qtag) ntag))
            note-tags))

(defun tiles--note-matches-tag-p (note-data query-and-groups)
  "Check if NOTE-DATA matches QUERY-AND-GROUPS.
QUERY-AND-GROUPS is a list of AND-groups.  Each AND-group is a list of tags
that must all be present.  Groups are combined with OR logic.
For example, ((\"b218\" \"lx2026\") (\"misc\")) means:
notes with both b218 AND lx2026, OR notes with misc."
  (let ((note-tags (plist-get note-data :tags)))
    (seq-some (lambda (and-group)
                (seq-every-p (lambda (qtag)
                               (tiles--tag-present-p qtag note-tags))
                             and-group))
              query-and-groups)))

(defun tiles--note-matches-keyword-p (note-data query-keywords)
  "Check if NOTE-DATA matches any of QUERY-KEYWORDS.
Query keywords are normalized (hyphens replaced with spaces) before matching."
  (let ((note-keywords (plist-get note-data :keywords)))
    (seq-some (lambda (qkw)
                (let ((normalized (tiles--normalize-keyword qkw)))
                  (seq-some (lambda (nkw)
                              (string-match-p (regexp-quote normalized) nkw))
                            note-keywords)))
              query-keywords)))

(defun tiles--validate-note-format (content)
  "Validate that CONTENT follows the TILES note format.
Returns t if valid, or an error message string.
Checks that the last non-empty line is a tag line preceded by a blank line."
  (let* ((lines (split-string content "\n"))
         (i (1- (length lines))))
    ;; Skip trailing empty lines
    (while (and (>= i 0) (string-empty-p (string-trim (nth i lines))))
      (setq i (1- i)))
    (cond
     ((< i 0)
      "Note is empty")
     ((< i 2)
      "Note must have at least content, a blank line, and tags")
     ((not (string-match-p "^[a-zA-Z0-9/_-]+$" (string-trim (nth i lines))))
      "Last line must be tags (letters, numbers, hyphens, separated by /)")
     ((not (string-empty-p (string-trim (nth (1- i) lines))))
      "Tags must be preceded by a blank line")
     (t t))))

;;; Tag-line fontification

(defun tiles--tag-line-matcher (limit)
  "Font-lock matcher for the tag line (last non-empty line) in a tiles note.
Only matches when preceded by a blank line, per the TILES note format.
Searches up to LIMIT.  Returns non-nil and sets match data if found."
  (let ((tag-line-pos nil))
    (save-excursion
      (goto-char (point-max))
      (skip-chars-backward " \t\n")
      (beginning-of-line)
      (when (and (<= (point) limit)
                 (looking-at "^\\(.+\\)$")
                 (not (looking-at-p "^&&?[ \t]"))
                 (> (line-number-at-pos) 1)
                 (save-excursion
                   (forward-line -1)
                   (looking-at-p "^[ \t]*$")))
        (setq tag-line-pos (point))))
    (when (and tag-line-pos (>= tag-line-pos (point)))
      (goto-char tag-line-pos)
      (looking-at "^\\(.+\\)$"))))

(defun tiles--enable-tag-line-fontification ()
  "Enable red tag-line highlighting in the current buffer."
  (font-lock-add-keywords nil '((tiles--tag-line-matcher 1 'tiles-tags t)))
  (when font-lock-mode
    (font-lock-flush)))

;;; Capture Mode

(defvar tiles-capture-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'tiles-capture-save)
    (define-key map (kbd "C-c C-k") #'tiles-capture-cancel)
    map)
  "Keymap for `tiles-capture-mode'.")

(define-minor-mode tiles-capture-mode
  "Minor mode for capturing TILES notes."
  :lighter " Tiles-Capture"
  :keymap tiles-capture-mode-map
  (when tiles-capture-mode
    (tiles--enable-tag-line-fontification)
    (message "TILES: C-c C-c to save, C-c C-k to cancel")))

;;;###autoload
(defun tiles-new ()
  "Create a new TILES note."
  (interactive)
  (tiles--ensure-directory)
  (let ((buf (get-buffer-create "*TILES New Note*")))
    (switch-to-buffer buf)
    (erase-buffer)
    (org-mode)
    (tiles-capture-mode 1)
    (insert "\n\n")
    (goto-char (point-min))
    (when tiles-focus-default
      (tiles-focus-mode 1))
    (message "Write your note, prepend with && any meta paragraph (optional), place tag(s) on the last line (separated by /), then hit C-c")))

;;;###autoload
(defalias 'tiles-capture #'tiles-new
  "Alias for `tiles-new'.")

(defun tiles-capture-save ()
  "Save the current TILES capture buffer as a new note."
  (interactive)
  (when (bound-and-true-p tiles-focus-mode)
    (tiles-focus-mode -1))
  (let ((content (buffer-string)))
    (let ((valid (tiles--validate-note-format content)))
      (when (not (eq valid t))
        ;; Offer to add tags
        (let ((tags (read-string (format "%s. Add tags (separated by /): " valid))))
          (if (string-empty-p (string-trim tags))
              (user-error "Aborted: note needs tags")
            (goto-char (point-max))
            ;; Ensure blank line before tags
            (unless (looking-back "\n\n" nil)
              (unless (looking-back "\n" nil)
                (insert "\n"))
              (insert "\n"))
            (insert tags "\n")
            (setq content (buffer-string))
            (setq valid (tiles--validate-note-format content))
            (when (not (eq valid t))
              (user-error "Still invalid: %s" valid)))))
      (when (eq valid t)
        (tiles--ensure-directory)
        (let* ((filename (tiles--generate-filename))
               (filepath (expand-file-name filename tiles-directory)))
          ;; Ensure we don't overwrite
          (while (file-exists-p filepath)
            (sleep-for 0 100)
            (setq filename (tiles--generate-filename))
            (setq filepath (expand-file-name filename tiles-directory)))
          (write-region content nil filepath)
          (tiles-capture-mode -1)
          (kill-buffer)
          ;; Refresh dashboard if it exists
          (when (get-buffer tiles--notes-buffer-name)
            (tiles-show-notes))
          (message "Saved: %s" filename))))))

(defun tiles-capture-cancel ()
  "Cancel the current TILES capture."
  (interactive)
  (when (yes-or-no-p "Discard this note? ")
    (tiles-capture-mode -1)
    (kill-buffer)
    (message "Note discarded")))

(defvar-local tiles--focus-saved-margins nil
  "Saved window margins before `tiles-focus-mode' was activated.")
(defvar-local tiles--focus-overlay nil
  "Overlay for top padding in `tiles-focus-mode'.")

(define-minor-mode tiles-focus-mode
  "Minor mode for distraction-free note editing.
Centers the buffer content with approximately 80-character line width
and adds 2 empty lines at the top for visual breathing room."
  :lighter " Focus"
  (if tiles-focus-mode
      (let* ((body-width 80)
             (win (selected-window))
             (win-width (window-total-width win))
             (margin (max 0 (/ (- win-width body-width) 2))))
        (setq tiles--focus-saved-margins (window-margins win))
        (set-window-margins win margin margin)
        (set-window-fringes win 0 0)
        (setq-local word-wrap t)
        (setq-local truncate-lines nil)
        (visual-line-mode 1)
        ;; Add 2 empty lines at top via overlay (not in buffer content)
        (let ((ov (make-overlay (point-min) (point-min))))
          (overlay-put ov 'before-string "\n\n")
          (setq tiles--focus-overlay ov))
        (goto-char (point-min)))
    ;; Deactivate: restore margins and remove overlay
    (when tiles--focus-overlay
      (delete-overlay tiles--focus-overlay)
      (setq tiles--focus-overlay nil))
    (let ((win (selected-window)))
      (if tiles--focus-saved-margins
          (set-window-margins win (car tiles--focus-saved-margins)
                              (cdr tiles--focus-saved-margins))
        (set-window-margins win 0 0))
      (set-window-fringes win nil nil))
    (visual-line-mode -1)
    (kill-local-variable 'word-wrap)
    (kill-local-variable 'truncate-lines)))

;;;###autoload
(defun tiles-touch ()
  "Update the current note's timestamp to now and rename the file accordingly.
Must be visiting a TILES note file.  Asks for confirmation."
  (interactive)
  (let ((filepath (buffer-file-name)))
    (unless (and filepath
                 (string-match-p "T[0-9]\\{14\\}\\.org$"
                                 (file-name-nondirectory filepath)))
      (user-error "Not visiting a TILES note file"))
    (let* ((old-name (file-name-nondirectory filepath))
           (old-ts (tiles--filename-to-timestamp old-name))
           (new-name (tiles--generate-filename))
           (new-filepath (expand-file-name new-name (file-name-directory filepath)))
           (new-ts (tiles--filename-to-timestamp new-name)))
      (when (yes-or-no-p (format "Update timestamp from %s to %s? " old-ts new-ts))
        ;; Save any unsaved changes first
        (when (buffer-modified-p)
          (save-buffer))
        (rename-file filepath new-filepath)
        (remhash filepath tiles--cache)
        (set-visited-file-name new-filepath t t)
        ;; Update preview in dashboard if open
        (when (equal tiles--preview-file filepath)
          (setq tiles--preview-file new-filepath))
        (let ((notes-buf (get-buffer tiles--notes-buffer-name)))
          (when notes-buf
            (with-current-buffer notes-buf
              (tiles-show-notes))))
        (message "Renamed %s -> %s" old-name new-name)))))

(defun tiles--quick-save (content tags)
  "Save a note with CONTENT and TAGS (slash-separated string)."
  (tiles--ensure-directory)
  (let* ((filename (tiles--generate-filename))
         (filepath (expand-file-name filename tiles-directory))
         (note (concat content "\n\n" tags "\n")))
    (while (file-exists-p filepath)
      (sleep-for 0 100)
      (setq filename (tiles--generate-filename))
      (setq filepath (expand-file-name filename tiles-directory)))
    (write-region note nil filepath)
    (when (get-buffer tiles--notes-buffer-name)
      (tiles-show-notes))
    (message "Saved: %s" filename)))

;;;###autoload
(defun tiles-quick ()
  "Quick capture a note via the minibuffer.
Prompts for content, then tags."
  (interactive)
  (let ((content (read-string "Note: "))
        (tags (read-string "Tags (separated by /): ")))
    (when (string-empty-p (string-trim content))
      (user-error "Note content cannot be empty"))
    (when (string-empty-p (string-trim tags))
      (user-error "Tags cannot be empty"))
    (tiles--quick-save content tags)))

;;;###autoload
(defun tiles-yank ()
  "Quick capture a note with content from clipboard.
Pre-fills content from the kill ring, then prompts for tags."
  (interactive)
  (let* ((clip (or (current-kill 0 t) ""))
         (content (read-string "Note: " clip))
         (tags (read-string "Tags (separated by /): ")))
    (when (string-empty-p (string-trim content))
      (user-error "Note content cannot be empty"))
    (when (string-empty-p (string-trim tags))
      (user-error "Tags cannot be empty"))
    (tiles--quick-save content tags)))

;;; Search Functions

;;;###autoload
(defun tiles-tag-search (query)
  "Search TILES notes by tags.
QUERY uses / for AND and space for OR.
\"b218/lx2026\" matches notes with both tags.
\"b218 misc\" matches notes with either tag.
\"b218/lx2026 misc\" matches (b218 AND lx2026) OR misc."
  (interactive "sSearch tags: ")
  (let* ((query-tags (tiles--parse-tag-query query))
         (files (tiles--get-all-tile-files))
         (results nil))
    (dolist (file files)
      (let ((note-data (tiles--parse-note-file file)))
        (when (and note-data (tiles--note-matches-tag-p note-data query-tags))
          (push file results))))
    (setq results (nreverse results))
    (setq tiles--current-search-results results)
    (setq tiles--current-search-query query)
    (setq tiles--current-search-type 'tag)
    (if results
        (tiles--show-search-view results)
      (message "No matching notes found for tags: %s" query))))

;;;###autoload
(defun tiles-keyword-search (query)
  "Search TILES notes by bold keywords.
QUERY is a space-separated list of keywords (OR logic)."
  (interactive "sSearch keywords: ")
  (let* ((query-keywords (split-string query " " t))
         (files (tiles--get-all-tile-files))
         (results nil))
    (dolist (file files)
      (let ((note-data (tiles--parse-note-file file)))
        (when (and note-data (tiles--note-matches-keyword-p note-data query-keywords))
          (push file results))))
    (setq results (nreverse results))
    (setq tiles--current-search-results results)
    (setq tiles--current-search-query query)
    (setq tiles--current-search-type 'keyword)
    (if results
        (tiles--show-search-view results)
      (message "No matching notes found for keywords: %s" query))))

(defvar tiles-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'tiles-list-next)
    (define-key map (kbd "p") #'tiles-list-prev)
    (define-key map (kbd "RET") #'tiles-list-select)
    (define-key map (kbd "o") #'tiles-list-sort-occurrence)
    (define-key map (kbd "a") #'tiles-list-sort-alpha)
    (define-key map (kbd "d") #'tiles-list-toggle-direction)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `tiles-list-mode'.")

(define-derived-mode tiles-list-mode special-mode "Tiles-List"
  "Major mode for browsing TILES tags or keywords."
  (setq-local cursor-type 'box)
  (setq truncate-lines t)
  (face-remap-add-relative 'hl-line 'tiles-notes-hl-line)
  (hl-line-mode 1))

(defvar-local tiles--list-type nil "Type of list: `tag' or `keyword'.")
(defvar-local tiles--list-items nil "Hash table of items to counts.")
(defvar-local tiles--list-cross nil "Hash table of cross-reference items.")
(defvar-local tiles--list-sort 'alpha "Current sort: `alpha' or `occurrence'.")
(defvar-local tiles--list-descending nil "When non-nil, sort in descending order.")

(defun tiles--list-render ()
  "Re-render the current list buffer with current sort settings."
  (let* ((items tiles--list-items)
         (cross tiles--list-cross)
         (sort-type tiles--list-sort)
         (desc tiles--list-descending)
         (type tiles--list-type)
         (type-label (if (eq type 'tag) "tags" "keywords"))
         (keys (hash-table-keys items))
         (sorted (if (eq sort-type 'occurrence)
                     (sort keys (if desc
                                    (lambda (a b)
                                      (let ((ca (gethash a items))
                                            (cb (gethash b items)))
                                        (if (= ca cb) (string< a b) (> ca cb))))
                                  (lambda (a b)
                                    (let ((ca (gethash a items))
                                          (cb (gethash b items)))
                                      (if (= ca cb) (string< a b) (< ca cb))))))
                   (sort keys (if desc #'string> #'string<))))
         (sort-label (format "%s %s"
                             (if (eq sort-type 'alpha) "alphabetical" "by occurrence")
                             (if desc "descending" "ascending")))
         (inhibit-read-only t))
    (erase-buffer)
    (insert (propertize (format "%d unique %s  [%s]  (RET:search  o:sort occurrence  a:sort alpha  d:toggle direction  q:quit)\n\n"
                                (length keys) type-label sort-label)
                        'face 'font-lock-comment-face
                        'tiles-header t))
    (dolist (item sorted)
      (let ((count (gethash item items))
            (is-cross (gethash item cross)))
        (insert (format "[%d] " count)
                (if is-cross
                    (propertize item 'face 'bold)
                  item)
                "\n")))
    (goto-char (point-min))
    (forward-line 2)))

(defun tiles-list-next ()
  "Move to the next item in the list."
  (interactive)
  (forward-line 1)
  (when (eobp) (forward-line -1)))

(defun tiles-list-prev ()
  "Move to the previous item in the list."
  (interactive)
  (forward-line -1)
  (when (get-text-property (point) 'tiles-header)
    (forward-line 1)))

(defun tiles-list-select ()
  "Search the dashboard for the item on the current line."
  (interactive)
  (let* ((raw (string-trim (buffer-substring-no-properties
                            (line-beginning-position) (line-end-position))))
         (item (if (string-match "^\\[[0-9]+\\] \\(.*\\)" raw)
                   (match-string 1 raw)
                 raw))
         (type (buffer-local-value 'tiles--list-type (current-buffer))))
    (when (and item (not (string-empty-p item)))
      (quit-window)
      (setq tiles--notes-page 0)
      (setq tiles--notes-filter (cons type item))
      (tiles-show-notes))))

(defun tiles-list-sort-occurrence ()
  "Sort the list by occurrence count (high to low by default)."
  (interactive)
  (setq tiles--list-sort 'occurrence)
  (setq tiles--list-descending t)
  (tiles--list-render))

(defun tiles-list-sort-alpha ()
  "Sort the list alphabetically (a-z by default)."
  (interactive)
  (setq tiles--list-sort 'alpha)
  (setq tiles--list-descending nil)
  (tiles--list-render))

(defun tiles-list-toggle-direction ()
  "Toggle between ascending and descending sort."
  (interactive)
  (setq tiles--list-descending (not tiles--list-descending))
  (tiles--list-render))

(defun tiles--list-collect-data ()
  "Collect tag and keyword counts from all notes.
Returns (tags-hash . keywords-hash) where values are counts."
  (let ((all-tags (make-hash-table :test 'equal))
        (all-keywords (make-hash-table :test 'equal)))
    (dolist (file (tiles--get-all-tile-files))
      (let ((note-data (tiles--parse-note-file file)))
        (when note-data
          (dolist (tag (plist-get note-data :tags))
            (puthash tag (1+ (gethash tag all-tags 0)) all-tags))
          (dolist (kw (plist-get note-data :keywords))
            (puthash kw (1+ (gethash kw all-keywords 0)) all-keywords)))))
    (cons all-tags all-keywords)))

;;;###autoload
(defun tiles-list-tags ()
  "Display all unique tags across all notes in a navigable buffer.
Tags that also appear as keywords are shown in bold.
Press RET to filter the dashboard by the selected tag."
  (interactive)
  (let* ((data (tiles--list-collect-data))
         (buf (get-buffer-create "*Tiles Tags*")))
    (with-current-buffer buf
      (tiles-list-mode)
      (setq-local tiles--list-type 'tag)
      (setq-local tiles--list-items (car data))
      (setq-local tiles--list-cross (cdr data))
      (setq-local tiles--list-sort 'alpha)
      (setq-local tiles--list-descending nil)
      (tiles--list-render))
    (switch-to-buffer buf)))

;;;###autoload
(defun tiles-list-keywords ()
  "Display all unique keywords across all notes in a navigable buffer.
Keywords that also appear as tags are shown in bold.
Press RET to filter the dashboard by the selected keyword."
  (interactive)
  (let* ((data (tiles--list-collect-data))
         (buf (get-buffer-create "*Tiles Keywords*")))
    (with-current-buffer buf
      (tiles-list-mode)
      (setq-local tiles--list-type 'keyword)
      (setq-local tiles--list-items (cdr data))
      (setq-local tiles--list-cross (car data))
      (setq-local tiles--list-sort 'alpha)
      (setq-local tiles--list-descending nil)
      (tiles--list-render))
    (switch-to-buffer buf)))

;;; Notes Viewer

(defvar tiles--notes-buffer-name "*Tiles Notes*"
  "Name of the notes viewer buffer.")

(defvar tiles-notes-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'tiles-notes-next)
    (define-key map (kbd "p") #'tiles-notes-prev)
    (define-key map (kbd "<down>") #'tiles-notes-next)
    (define-key map (kbd "<up>") #'tiles-notes-prev)
    (define-key map (kbd "RET") #'tiles-notes-open)
    (define-key map (kbd "SPC") #'tiles-notes-preview)
    (define-key map (kbd "t") #'tiles-notes-filter-tag)
    (define-key map (kbd "k") #'tiles-notes-filter-keyword)
    (define-key map (kbd "c") #'tiles-notes-clear-filter)
    (define-key map (kbd "C") #'tiles-notes-clear-exclude)
    (define-key map (kbd "d") #'tiles-notes-change-date)
    (define-key map (kbd "D") #'tiles-notes-delete)
    (define-key map (kbd "TAB") #'tiles-notes-toggle-expand)
    (define-key map (kbd "<M-up>") #'tiles-notes-move-up)
    (define-key map (kbd "<M-down>") #'tiles-notes-move-down)
    (define-key map (kbd "f") #'tiles-notes-toggle-raw)
    (define-key map (kbd "F") #'tiles-notes-exclude-tags)
    (define-key map (kbd "T") #'tiles-list-tags)
    (define-key map (kbd "K") #'tiles-list-keywords)
    (define-key map (kbd "u") #'tiles-notes-touch)
    (define-key map (kbd "+") #'tiles-notes-load-more)
    (define-key map (kbd "0") #'tiles-notes-stitch)
    (define-key map (kbd "l") #'tiles-new)
    (define-key map (kbd "g") #'tiles-show-notes)
    (define-key map (kbd "q") #'tiles-notes-quit)
    map)
  "Keymap for `tiles-notes-view-mode'.")

(defface tiles-timestamp-today
  '((t :foreground "#228b22"))
  "Face for today's note timestamps."
  :group 'tiles)

(defface tiles-timestamp-recent
  '((t :foreground "#3a5a2a"))
  "Face for note timestamps less than 2 weeks old."
  :group 'tiles)

(defface tiles-timestamp-old
  '((t :foreground "#999999"))
  "Face for note timestamps older than 2 weeks."
  :group 'tiles)

(defface tiles-tags
  '((t :foreground "#a00000"))
  "Face for tags in the notes viewer."
  :group 'tiles)

(defface tiles-notes-hl-line
  '((t :background "#FFC700"))
  "Face for the selected line in the notes viewer."
  :group 'tiles)

(defface tiles-notes-expanded
  '((t :background "#FFF8DC"))
  "Face for expanded note lines in the notes viewer."
  :group 'tiles)

(defun tiles--hl-line-range ()
  "Return the range for hl-line: from line start to at most target-width chars."
  (let* ((bol (line-beginning-position))
         (eol (line-end-position))
         (end (min eol (+ bol tiles--line-target-width))))
    (cons bol end)))

(define-derived-mode tiles-notes-view-mode special-mode "Tiles-Notes"
  "Major mode for TILES chronological notes viewer."
  (setq-local line-move-visual nil)
  (setq-local cursor-type 'box)
  (setq truncate-lines t)
  (setq-local hl-line-range-function #'tiles--hl-line-range)
  (face-remap-add-relative 'hl-line 'tiles-notes-hl-line)
  (hl-line-mode 1)
  (add-hook 'after-save-hook #'tiles--after-save-hook))

(defun tiles--filename-to-timestamp (filename)
  "Extract a formatted timestamp from FILENAME.
FILENAME should match TYYYYMMDDHHMMSS.org."
  (when (string-match "T\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)" filename)
    (format "%s-%s-%s %s:%s"
            (match-string 1 filename)
            (match-string 2 filename)
            (match-string 3 filename)
            (match-string 4 filename)
            (match-string 5 filename))))

(defface tiles-keywords
  '((t :foreground "#006600"))
  "Face for keywords (bold words) in the notes viewer."
  :group 'tiles)

(defun tiles--strip-org-markup (text)
  "Strip all org-mode markup from TEXT, returning plain text."
  (let ((result text))
    (setq result (replace-regexp-in-string "\\[fn:[^]]*\\]" "" result))
    (setq result (replace-regexp-in-string "\\[\\[\\(?:[^][]+\\)\\]\\[\\([^][]+\\)\\]\\]" "\\1" result))
    (setq result (replace-regexp-in-string "\\[\\[[^][]+\\]\\]" "" result))
    (setq result (replace-regexp-in-string "\\*\\([^*\n]+\\)\\*" "\\1" result))
    (setq result (replace-regexp-in-string "/\\([^/\n]+\\)/" "\\1" result))
    result))

(defun tiles--render-org-preview (text)
  "Render TEXT for dashboard preview.
When `tiles-preview-raw' is non-nil, strips all org markup to plain text.
Otherwise renders with bold/italic faces, stripping footnotes and links."
  (if tiles-preview-raw
      (tiles--strip-org-markup text)
    ;; Strip inline footnotes [fn:...]
    (let ((result text))
      (setq result (replace-regexp-in-string "\\[fn:[^]]*\\]" "" result))
      ;; Replace org links [[url][desc]] with just desc, and [[url]] with empty
      (setq result (replace-regexp-in-string "\\[\\[\\(?:[^][]+\\)\\]\\[\\([^][]+\\)\\]\\]" "\\1" result))
      (setq result (replace-regexp-in-string "\\[\\[[^][]+\\]\\]" "" result))
      ;; Apply bold
      (let ((pos 0)
            (out ""))
        (while (string-match "\\*\\([^*\n]+\\)\\*" result pos)
          (setq out (concat out (substring result pos (match-beginning 0))
                            (propertize (match-string 1 result) 'face 'bold)))
          (setq pos (match-end 0)))
        (setq result (concat out (substring result pos))))
      ;; Apply italic
      (let ((pos 0)
            (out ""))
        (while (string-match "/\\([^/\n]+\\)/" result pos)
          (setq out (concat out (substring result pos (match-beginning 0))
                            (propertize (match-string 1 result) 'face 'italic)))
          (setq pos (match-end 0)))
        (setq result (concat out (substring result pos))))
      result)))

(defun tiles--note-oneline-preview (note-data)
  "Return a propertized single-line preview of NOTE-DATA's content, max 80 chars."
  (let* ((content (tiles--strip-private-paragraphs
                   (or (plist-get note-data :content) "")))
         (line (string-trim (or (car (split-string content "\n" t)) "")))
         (rendered (tiles--render-org-preview line))
         (truncated (if (> (length rendered) tiles-preview-length)
                        (concat (substring rendered 0 (1- tiles-preview-length)) "…")
                      rendered)))
    truncated))

(defun tiles--timestamp-face (filename)
  "Return the appropriate face for FILENAME based on its age."
  (when (string-match "T\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)" filename)
    (let* ((date-str (format "%s-%s-%s"
                             (match-string 1 filename)
                             (match-string 2 filename)
                             (match-string 3 filename)))
           (note-time (date-to-time (concat date-str " 00:00:00")))
           (age-days (/ (float-time (time-subtract (current-time) note-time)) 86400)))
      (cond
       ((< age-days 1) 'tiles-timestamp-today)
       ((< age-days 14) 'tiles-timestamp-recent)
       (t 'tiles-timestamp-old)))))

(defvar tiles--notes-filter nil
  "Current dashboard filter as (TYPE . QUERY) or nil.
TYPE is `tag' or `keyword'.")

(defvar tiles--notes-exclude nil
  "List of tags to exclude from dashboard display and search.
Set by `F' in the dashboard, cleared by `c'.")

(defvar-local tiles--line-target-width 135
  "Computed target width for note lines in the dashboard.
Equal to `tiles-preview-length' + `tiles-line-padding' + longest tag string length.")
(put 'tiles--line-target-width 'permanent-local t)

(defvar tiles--notes-page 0
  "Current page offset in the dashboard (0-indexed).")

(defun tiles-notes-filter-tag (query)
  "Filter the dashboard to show only notes matching QUERY tags."
  (interactive "sFilter by tag: ")
  (if (string-empty-p (string-trim query))
      (tiles-notes-clear-filter)
    (setq tiles--notes-page 0)
    (setq tiles--notes-filter (cons 'tag query))
    (tiles-show-notes)))

(defun tiles-notes-filter-keyword (query)
  "Filter the dashboard to show only notes matching QUERY keywords."
  (interactive "sFilter by keyword: ")
  (if (string-empty-p (string-trim query))
      (tiles-notes-clear-filter)
    (setq tiles--notes-page 0)
    (setq tiles--notes-filter (cons 'keyword query))
    (tiles-show-notes)))

(defun tiles-notes-exclude-tags (query)
  "Exclude notes with any of the specified tags from the dashboard.
QUERY is a space-separated list of tags to exclude."
  (interactive "sExclude tags (space-separated): ")
  (if (string-empty-p (string-trim query))
      (message "No tags specified")
    (setq tiles--notes-page 0)
    (setq tiles--notes-exclude (split-string query " " t))
    (tiles-show-notes)))

(defun tiles-notes-clear-filter ()
  "Clear the dashboard search filter (keeps exclusions)."
  (interactive)
  (setq tiles--notes-page 0)
  (setq tiles--notes-filter nil)
  (tiles-show-notes))

(defun tiles-notes-clear-exclude ()
  "Clear the tag exclusion filter."
  (interactive)
  (setq tiles--notes-page 0)
  (setq tiles--notes-exclude nil)
  (tiles-show-notes))

(defun tiles-notes-touch ()
  "Update the selected note's timestamp to now and rename the file."
  (interactive)
  (let ((filepath (tiles--notes-current-file)))
    (unless filepath
      (user-error "No note on this line"))
    (let* ((old-name (file-name-nondirectory filepath))
           (old-ts (tiles--filename-to-timestamp old-name))
           (new-name (tiles--generate-filename))
           (new-filepath (expand-file-name new-name (file-name-directory filepath)))
           (new-ts (tiles--filename-to-timestamp new-name)))
      (when (yes-or-no-p (format "Update timestamp from %s to %s? " old-ts new-ts))
        ;; Close preview if showing this file
        (when (equal tiles--preview-file filepath)
          (let* ((buf (get-file-buffer filepath))
                 (win (when buf (get-buffer-window buf))))
            (when win (delete-window win))
            (when buf (kill-buffer buf))
            (setq tiles--preview-file nil)))
        (rename-file filepath new-filepath)
        (remhash filepath tiles--cache)
        (tiles-show-notes)
        (message "Renamed %s -> %s" old-name new-name)))))

(defun tiles-notes-load-more ()
  "Load the next batch of notes in the dashboard."
  (interactive)
  (if (not tiles-dashboard-limit)
      (message "All notes already shown")
    (setq tiles--notes-page (1+ tiles--notes-page))
    (tiles-show-notes)))

(defun tiles-notes-toggle-raw ()
  "Toggle raw (plain text) preview in the dashboard."
  (interactive)
  (setq tiles-preview-raw (not tiles-preview-raw))
  (tiles-show-notes)
  (message "Preview formatting %s" (if tiles-preview-raw "off" "on")))

(defun tiles--apply-exclude (files)
  "Remove from FILES any notes that have excluded tags.
Uses `tiles--notes-exclude'."
  (if (not tiles--notes-exclude)
      files
    (seq-remove (lambda (file)
                  (let* ((note-data (tiles--parse-note-file file))
                         (tags (when note-data (plist-get note-data :tags))))
                    (seq-some (lambda (etag)
                                (member etag tags))
                              tiles--notes-exclude)))
                files)))

(defun tiles--apply-filter (files)
  "Filter FILES according to `tiles--notes-filter' and `tiles--notes-exclude'."
  (let ((result (tiles--apply-exclude files)))
    (if (not tiles--notes-filter)
        result
      (let* ((type (car tiles--notes-filter))
             (query (cdr tiles--notes-filter))
             (query-parts (if (eq type 'tag)
                              (tiles--parse-tag-query query)
                            (split-string query " " t)))
             (match-fn (if (eq type 'tag)
                           #'tiles--note-matches-tag-p
                         #'tiles--note-matches-keyword-p)))
        (seq-filter (lambda (file)
                      (let ((note-data (tiles--parse-note-file file)))
                        (and note-data (funcall match-fn note-data query-parts))))
                    result)))))

;;;###autoload
(defun tiles-show-notes ()
  "Display all notes chronologically, newest first.
Shows a dashboard with statistics and note listing."
  (interactive)
  (let* ((all-files (tiles--get-all-tile-files))
         (filtered (tiles--apply-filter all-files)))
    (if (not all-files)
        (message "No tiles notes found in %s" tiles-directory)
      (let* ((buf (get-buffer-create tiles--notes-buffer-name))
             (start-time (float-time))
             (num-all (length all-files))
             (num-filtered (length filtered))
             (page-end (if tiles-dashboard-limit
                           (* (1+ tiles--notes-page) tiles-dashboard-limit)
                         (length filtered)))
             (files (seq-take filtered page-end))
             (has-more (and tiles-dashboard-limit (> num-filtered page-end)))
             (max-tag-len 0))
        ;; Compute longest tag string length
        (dolist (file files)
          (let* ((note-data (tiles--parse-note-file file))
                 (tags (when note-data (plist-get note-data :tags)))
                 (tag-str (if tags (concat "  " (mapconcat #'identity tags "/")) "")))
            (setq max-tag-len (max max-tag-len (length tag-str)))))
        (delete-other-windows)
        (switch-to-buffer buf)
        (setq tiles--line-target-width (+ tiles-preview-length tiles-line-padding max-tag-len))
        (let ((inhibit-read-only t))
          (erase-buffer)
          (dolist (file files)
            (insert (tiles--render-note-line file) "\n"))
          (when has-more
            (insert (propertize (format "  ... %d more notes (+:load more)\n"
                                        (- num-filtered page-end))
                                'face 'font-lock-comment-face
                                'tiles-header t)))
          (let* ((load-time (- (float-time) start-time))
                 (title (format "  *T*agged *I*nstant *L*ightweight *E*macs *S*nippets (TILES), v%s | %d notes | loaded in %.3fs\n"
                                tiles-version num-all load-time))
                 (keys (concat "  [SPC] view, [RET] open, [TAB] expand, [f] format, [d] chg date, [u] touch, [0] stitch, [D] delete, [g] refresh, [+] more, [q] quit\n"
                               "  [t] filter tag, [k] filter keyword, [F] exclude tags, [T] list tags, [K] list keywords, [c] clr search, [C] clr excl, [l] new tile\n"))
                 (lunar-str (or (condition-case nil
                                    (tiles--next-lunar-event)
                                  (error nil))
                                ""))
                 (filter-info (if tiles--notes-filter
                                  (format "filter %s: %s (%d/%d)"
                                          (car tiles--notes-filter)
                                          (cdr tiles--notes-filter)
                                          num-filtered num-all)
                                ""))
                 (exclude-info (if tiles--notes-exclude
                                   (format "excluding: %s"
                                           (mapconcat #'identity tiles--notes-exclude " "))
                                 ""))
                 (page-info (if tiles-dashboard-limit
                                (format "showing %d/%d" (length files) num-filtered)
                              ""))
                 (status-parts (seq-filter (lambda (s) (not (string-empty-p s)))
                                           (list lunar-str filter-info exclude-info page-info)))
                 (status-line (format "  %s\n" (mapconcat #'identity status-parts " > ")))
                 (eq-char (if tiles-fancy-separators ?═ ?=))
                 (dash-char (if tiles-fancy-separators ?─ ?-))
                 (eq-line (concat "  " (make-string (- tiles--line-target-width 2) eq-char) "\n"))
                 (dash-line (concat "  " (make-string (- tiles--line-target-width 2) dash-char) "\n\n")))
            (goto-char (point-min))
            (insert (propertize title 'tiles-header t)
                    (propertize eq-line 'face 'font-lock-comment-face 'tiles-header t)
                    (propertize keys 'face 'font-lock-comment-face 'tiles-header t)
                    (propertize status-line 'face 'font-lock-comment-face 'tiles-header t)
                    (propertize dash-line 'face 'font-lock-comment-face 'tiles-header t))))
        (tiles-notes-view-mode)
        ;; Move past header to first note line
        (goto-char (point-min))
        (text-property-search-forward 'tiles-filepath)))))

(defun tiles--notes-on-note-line-p ()
  "Return t if point is on a note line (not an expanded detail line)."
  (and (get-text-property (line-beginning-position) 'tiles-filepath)
       (not (get-text-property (line-beginning-position) 'tiles-expanded))))

(defun tiles--notes-update-preview-if-open ()
  "Update the preview split if it is currently showing a note."
  (when tiles--preview-file
    (let ((filepath (tiles--notes-current-file)))
      (when (and filepath (not (equal filepath tiles--preview-file)))
        (let* ((old-buf (get-file-buffer tiles--preview-file))
               (existing-win (when old-buf (get-buffer-window old-buf))))
          (when existing-win
            (with-selected-window existing-win
              (find-file filepath))
            (setq tiles--preview-file filepath)))))))

(defun tiles--notes-has-expansion-p ()
  "Return t if any note is currently expanded."
  (save-excursion
    (goto-char (point-min))
    (not (null (text-property-search-forward 'tiles-expanded)))))

(defun tiles-notes-next ()
  "Move to next note."
  (interactive)
  (if (tiles--notes-has-expansion-p)
      (message "Collapse expanded note first (TAB)")
    (let ((start (point)))
      (forward-line 1)
      (while (and (not (eobp)) (not (tiles--notes-on-note-line-p)))
        (forward-line 1))
      (if (tiles--notes-on-note-line-p)
          (tiles--notes-update-preview-if-open)
        (goto-char start)))))

(defun tiles-notes-prev ()
  "Move to previous note."
  (interactive)
  (if (tiles--notes-has-expansion-p)
      (message "Collapse expanded note first (TAB)")
    (let ((start (point)))
      (forward-line -1)
      (while (and (not (bobp)) (not (tiles--notes-on-note-line-p)))
        (forward-line -1))
      (if (tiles--notes-on-note-line-p)
          (tiles--notes-update-preview-if-open)
        (goto-char start)))))

(defun tiles--notes-current-file ()
  "Get filepath of note on current line."
  (get-text-property (line-beginning-position) 'tiles-filepath))

(defun tiles-notes-open ()
  "Open the note on the current line."
  (interactive)
  (let ((filepath (tiles--notes-current-file)))
    (when filepath
      (find-file filepath)
      (tiles--enable-tag-line-fontification)
      (when tiles-focus-default
        (tiles-focus-mode 1)))))

(defvar tiles--preview-file nil
  "Filepath currently shown in the preview split.")

(defun tiles-notes-preview ()
  "Open the note on the current line in a split below for editing."
  (interactive)
  (let ((filepath (tiles--notes-current-file)))
    (when filepath
      (let ((existing-win (when tiles--preview-file
                            (let ((buf (get-file-buffer tiles--preview-file)))
                              (when buf (get-buffer-window buf))))))
        (if existing-win
            ;; Reuse the existing preview window
            (progn
              (with-selected-window existing-win
                (find-file filepath))
              (setq tiles--preview-file filepath))
          ;; Open a new split
          (split-window-below)
          (other-window 1)
          (find-file filepath)
          (setq tiles--preview-file filepath)
          (other-window 1))))))

(defun tiles--note-expanded-extra (note-data file)
  "Return two extra lines: keywords and stats (chars, words, filesize)."
  (let* ((content (or (plist-get note-data :content) ""))
         (filepath (plist-get note-data :filepath))
         (indent (make-string 20 ?\s))
         (keywords (plist-get note-data :keywords))
         (char-count (length content))
         (word-count (length (split-string content "[ \t\n]+" t)))
         (filesize (if (and filepath (file-exists-p filepath))
                       (let* ((size (file-attribute-size (file-attributes filepath)))
                              (human (file-size-human-readable size)))
                         (if (string-match-p "[A-Za-z]" human)
                             human
                           (concat human "B")))
                     "?"))
         (result ""))
    ;; Private paragraphs (&&) line
    (let ((private (tiles--extract-private-paragraphs content)))
      (when private
        (let* ((priv-str (mapconcat #'identity private " | "))
               (max-priv-len (- tiles--line-target-width 20))
               (priv-str (if (> (length priv-str) max-priv-len)
                             (concat (substring priv-str 0 (1- max-priv-len)) "…")
                           priv-str))
               (priv-line (concat indent priv-str))
               (padded-priv (if (< (length priv-line) tiles--line-target-width)
                                (concat priv-line (make-string (- tiles--line-target-width (length priv-line)) ?\s))
                              priv-line)))
          (setq result (concat result
                               (propertize padded-priv
                                           'face (list 'font-lock-doc-face 'tiles-notes-expanded)
                                           'tiles-filepath file
                                           'tiles-expanded t)
                               "\n")))))
    ;; Keywords line (truncated to align with preview above)
    (let* ((kw-str (if keywords
                       (concat "[" (mapconcat #'identity keywords ", ") "]")
                     "[no keywords]"))
           (max-kw-len (- tiles--line-target-width 20))
           (kw-str (if (> (length kw-str) max-kw-len)
                       (concat (substring kw-str 0 (1- max-kw-len)) "…")
                     kw-str))
           (kw-line (concat indent kw-str))
           (padded-kw (if (< (length kw-line) tiles--line-target-width)
                          (concat kw-line (make-string (- tiles--line-target-width (length kw-line)) ?\s))
                        kw-line)))
      (setq result (concat result
                           (propertize padded-kw
                                       'face (list 'tiles-tags 'tiles-notes-expanded)
                                       'tiles-filepath file
                                       'tiles-expanded t)
                           "\n")))
    ;; Stats line
    (let* ((stats (format "%d chars, %d words, %s" char-count word-count filesize))
           (stats-line (concat indent stats))
           (padded-stats (if (< (length stats-line) tiles--line-target-width)
                             (concat stats-line (make-string (- tiles--line-target-width (length stats-line)) ?\s))
                           stats-line)))
      (setq result (concat result
                           (propertize padded-stats
                                       'face (list 'font-lock-comment-face 'tiles-notes-expanded)
                                       'tiles-filepath file
                                       'tiles-expanded t)
                           "\n")))
    result))

(defun tiles-notes-toggle-expand ()
  "Toggle expanded view for the note on the current line."
  (interactive)
  (let ((filepath (tiles--notes-current-file)))
    (unless filepath
      (user-error "No note on this line"))
    (let ((inhibit-read-only t))
      (save-excursion
        (forward-line 1)
        (if (get-text-property (point) 'tiles-expanded)
            ;; Collapse: remove expanded lines
            (let ((start (point)))
              (while (and (not (eobp))
                          (get-text-property (point) 'tiles-expanded))
                (forward-line 1))
              (delete-region start (point)))
          ;; Expand: insert continuation + keywords below current line
          (let* ((note-data (tiles--parse-note-file filepath))
                 (extra (when note-data
                          (tiles--note-expanded-extra note-data filepath))))
            (when (and extra (not (string-empty-p extra)))
              (insert extra))))))))

(defun tiles--notes-displayed-files ()
  "Return the list of filepaths in current display order from the dashboard buffer."
  (let ((files nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((fp (get-text-property (point) 'tiles-filepath)))
          (when (and fp
                     (not (get-text-property (point) 'tiles-expanded))
                     (not (get-text-property (point) 'tiles-header)))
            (push fp files)))
        (forward-line 1)))
    (nreverse files)))

(defun tiles-notes-move-up ()
  "Move the selected note up one position in the dashboard."
  (interactive)
  (when (tiles--notes-has-expansion-p)
    (user-error "Collapse expanded note first (TAB)"))
  (let ((filepath (tiles--notes-current-file)))
    (unless filepath
      (user-error "No note on this line"))
    (let ((inhibit-read-only t)
          (cur-start (line-beginning-position))
          (cur-end (1+ (line-end-position))))
      ;; Find previous note line
      (save-excursion
        (forward-line -1)
        (while (and (not (bobp)) (not (tiles--notes-on-note-line-p)))
          (forward-line -1))
        (when (tiles--notes-on-note-line-p)
          (let* ((prev-start (line-beginning-position))
                 (cur-line (buffer-substring cur-start cur-end)))
            (delete-region cur-start cur-end)
            (goto-char prev-start)
            (insert cur-line))))
      ;; Re-position cursor on the moved line
      (goto-char (point-min))
      (while (and (not (eobp))
                  (not (equal (get-text-property (point) 'tiles-filepath) filepath)))
        (forward-line 1)))))

(defun tiles-notes-move-down ()
  "Move the selected note down one position in the dashboard."
  (interactive)
  (when (tiles--notes-has-expansion-p)
    (user-error "Collapse expanded note first (TAB)"))
  (let ((filepath (tiles--notes-current-file)))
    (unless filepath
      (user-error "No note on this line"))
    (let ((inhibit-read-only t)
          (cur-start (line-beginning-position))
          (cur-end (1+ (line-end-position))))
      ;; Find next note line
      (save-excursion
        (goto-char cur-end)
        (while (and (not (eobp)) (not (tiles--notes-on-note-line-p)))
          (forward-line 1))
        (when (tiles--notes-on-note-line-p)
          (let* ((next-end (1+ (line-end-position)))
                 (cur-line (buffer-substring cur-start cur-end)))
            (goto-char next-end)
            (insert cur-line)
            (delete-region cur-start cur-end))))
      ;; Re-position cursor on the moved line
      (goto-char (point-min))
      (while (and (not (eobp))
                  (not (equal (get-text-property (point) 'tiles-filepath) filepath)))
        (forward-line 1)))))

(defun tiles-notes-stitch ()
  "Stitch the currently displayed notes into a flowing view, respecting display order."
  (interactive)
  (let ((files (tiles--notes-displayed-files)))
    (if (not files)
        (message "No notes to stitch")
      (if (or tiles--notes-filter
              (yes-or-no-p "You should perform a search before stitching notes. Are you sure you want to stitch ALL the notes? "))
          (progn
            (tiles-notes-quit)
            (setq tiles--current-search-results files)
            (tiles--show-stitched-view files))
        (message nil)))))

(defun tiles-notes-change-date ()
  "Change the date/time of the note on the current line.
Prompts for a new date in YYYY-MM-DD HH:MM or YYYY-MM-DD HH:MM:SS format."
  (interactive)
  (let ((filepath (tiles--notes-current-file)))
    (unless filepath
      (user-error "No note on this line"))
    (let* ((fname (file-name-nondirectory filepath))
           (current-ts (tiles--filename-to-timestamp fname))
           (new-ts (read-string "New date (YYYY-MM-DD HH:MM[:SS]): " current-ts))
           (parts (split-string new-ts "[-: ]" t)))
      (when (= (length parts) 5)
        (setq parts (append parts '("00"))))
      (unless (= (length parts) 6)
        (user-error "Invalid date format, use YYYY-MM-DD HH:MM or YYYY-MM-DD HH:MM:SS"))
      (let* ((new-fname (format "T%s%s%s%s%s%s.org"
                                (nth 0 parts) (nth 1 parts) (nth 2 parts)
                                (nth 3 parts) (nth 4 parts) (nth 5 parts)))
             (new-filepath (expand-file-name new-fname (file-name-directory filepath))))
        (when (and (file-exists-p new-filepath)
                   (not (equal filepath new-filepath)))
          (user-error "A note with that timestamp already exists"))
        (unless (equal filepath new-filepath)
          ;; If this file is open in the preview, close it first
          (when (equal tiles--preview-file filepath)
            (let* ((buf (get-file-buffer filepath))
                   (win (when buf (get-buffer-window buf))))
              (when win (delete-window win))
              (when buf (kill-buffer buf))
              (setq tiles--preview-file nil)))
          (rename-file filepath new-filepath)
          (remhash filepath tiles--cache)
          ;; Refresh the dashboard
          (tiles-show-notes))))))

(defun tiles-notes-delete ()
  "Delete the note on the current line after confirmation."
  (interactive)
  (let ((filepath (tiles--notes-current-file)))
    (unless filepath
      (user-error "No note on this line"))
    (let* ((fname (file-name-nondirectory filepath))
           (note-data (tiles--parse-note-file filepath))
           (preview (if note-data
                        (truncate-string-to-width
                         (string-trim
                          (or (car (split-string
                                    (tiles--strip-private-paragraphs
                                     (plist-get note-data :content)) "\n" t))
                              ""))
                         60)
                      "")))
      (when (yes-or-no-p (format "Delete note %s (%s)? " fname preview))
        ;; Close preview if showing this file
        (when (equal tiles--preview-file filepath)
          (let* ((buf (get-file-buffer filepath))
                 (win (when buf (get-buffer-window buf))))
            (when win (delete-window win))
            (when buf (kill-buffer buf))
            (setq tiles--preview-file nil)))
        (delete-file filepath)
        (remhash filepath tiles--cache)
        (tiles-show-notes)))))

(defun tiles--render-note-line (file)
  "Return a propertized string for FILE's line in the notes viewer."
  (let* ((fname (file-name-nondirectory file))
         (timestamp (tiles--filename-to-timestamp fname))
         (ts-face (tiles--timestamp-face fname))
         (note-data (tiles--parse-note-file file))
         (preview (if note-data (tiles--note-oneline-preview note-data) ""))
         (tags (when note-data (plist-get note-data :tags)))
         (tag-str (if tags (concat "  " (mapconcat #'identity tags "/")) ""))
         (preview-padded (if (< (length preview) tiles-preview-length)
                             (concat preview (make-string (- tiles-preview-length (length preview)) ?\s))
                           preview))
         (has-private (and (not tiles-preview-raw)
                           note-data
                           (let ((content (plist-get note-data :content)))
                             (seq-some #'tiles--private-paragraph-p
                                       (split-string (or content "") "\n\n+" t)))))
         (separator (if has-private
                        (concat " " (propertize "&" 'face 'tiles-tags))
                      "  ")))
    (let* ((line (concat (propertize "  " 'tiles-filepath file)
                         (propertize timestamp 'face ts-face 'tiles-filepath file)
                         (propertize separator 'tiles-filepath file)
                         (propertize preview-padded 'tiles-filepath file)
                         (propertize tag-str 'face 'tiles-tags 'tiles-filepath file)))
           (cur-len (length line)))
      (if (< cur-len tiles--line-target-width)
          (concat line (propertize (make-string (- tiles--line-target-width cur-len) ?\s)
                                   'tiles-filepath file))
        line))))

(defun tiles--refresh-note-line (filepath)
  "Refresh the line for FILEPATH in the notes viewer buffer."
  ;; Invalidate cache so we re-read from disk
  (remhash filepath tiles--cache)
  (let ((notes-buf (get-buffer tiles--notes-buffer-name)))
    (when notes-buf
      (with-current-buffer notes-buf
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (when (equal (get-text-property (point) 'tiles-filepath) filepath)
                (let ((line-start (line-beginning-position))
                      (line-end (line-end-position)))
                  (delete-region line-start line-end)
                  (insert (tiles--render-note-line filepath)))
                (goto-char (line-end-position)))
              (forward-line 1))))))))

(defun tiles--after-save-hook ()
  "Hook to refresh the notes viewer after saving a tile file."
  (let ((filepath (buffer-file-name)))
    (when (and filepath
               (string-prefix-p (expand-file-name tiles-directory)
                                (expand-file-name filepath)))
      (tiles--refresh-note-line filepath))))

(defun tiles-notes-quit ()
  "Quit the notes viewer."
  (interactive)
  (remove-hook 'after-save-hook #'tiles--after-save-hook)
  (setq tiles--notes-filter nil)
  (setq tiles--notes-page 0)
  (let ((notes-buf (get-buffer tiles--notes-buffer-name)))
    (when notes-buf (kill-buffer notes-buf)))
  ;; Close the preview window but keep the file buffer (in case of unsaved edits)
  (when tiles--preview-file
    (let* ((buf (get-file-buffer tiles--preview-file))
           (win (when buf (get-buffer-window buf))))
      (when win (delete-window win)))
    (setq tiles--preview-file nil))
  (delete-other-windows))

;;; Search View Mode (Two-Panel)

(defvar tiles-search-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'tiles-search-next)
    (define-key map (kbd "p") #'tiles-search-prev)
    (define-key map (kbd "<down>") #'tiles-search-next)
    (define-key map (kbd "<up>") #'tiles-search-prev)
    (define-key map (kbd "RET") #'tiles-search-open)
    (define-key map (kbd "SPC") #'tiles-toggle-view)
    (define-key map (kbd "r") #'tiles-refine-search)
    (define-key map (kbd "t") #'tiles-switch-to-tag-search)
    (define-key map (kbd "k") #'tiles-switch-to-keyword-search)
    (define-key map (kbd "q") #'tiles-quit)
    map)
  "Keymap for `tiles-search-view-mode'.")

(define-derived-mode tiles-search-view-mode special-mode "Tiles-Search"
  "Major mode for TILES search results."
  (setq-local line-move-visual nil)
  (setq-local cursor-type 'box)
  (hl-line-mode 1))

(defun tiles--show-search-view (results)
  "Display RESULTS in two-panel search view."
  (delete-other-windows)
  ;; Create search results buffer (upper panel)
  (let ((search-buf (get-buffer-create tiles--search-buffer-name)))
    (switch-to-buffer search-buf)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (dolist (file results)
        (insert (propertize (file-name-nondirectory file)
                            'tiles-filepath file)
                "\n"))
      (goto-char (point-min)))
    (tiles-search-view-mode)
    ;; Split for preview (lower panel)
    (split-window-below)
    (other-window 1)
    (switch-to-buffer (get-buffer-create tiles--preview-buffer-name))
    (org-mode)
    (setq buffer-read-only t)
    (other-window 1)
    ;; Update preview for first result
    (tiles--update-preview)))

(defun tiles--current-result-file ()
  "Get the filepath of the currently selected result."
  (get-text-property (line-beginning-position) 'tiles-filepath))

(defun tiles--update-preview ()
  "Update the preview buffer with the current selection."
  (let ((filepath (tiles--current-result-file)))
    (when filepath
      (let ((note-data (tiles--parse-note-file filepath))
            (preview-buf (get-buffer tiles--preview-buffer-name)))
        (when (and note-data preview-buf)
          (with-current-buffer preview-buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert (tiles--strip-private-paragraphs
                       (plist-get note-data :content)))
              (goto-char (point-min)))))))))

(defun tiles-search-next ()
  "Move to next search result."
  (interactive)
  (forward-line 1)
  (when (eobp)
    (forward-line -1))
  (tiles--update-preview))

(defun tiles-search-prev ()
  "Move to previous search result."
  (interactive)
  (forward-line -1)
  (tiles--update-preview))

(defun tiles-search-open ()
  "Open the selected note for editing."
  (interactive)
  (let ((filepath (tiles--current-result-file)))
    (when filepath
      (tiles-quit)
      (find-file filepath)
      (tiles--enable-tag-line-fontification))))

(defun tiles-toggle-view ()
  "Toggle between search view and stitched view."
  (interactive)
  (if tiles--current-search-results
      (tiles--show-stitched-view tiles--current-search-results)
    (message "No search results to display")))

(defun tiles-refine-search ()
  "Start a new search."
  (interactive)
  (let ((type tiles--current-search-type))
    (tiles-quit)
    (if (eq type 'keyword)
        (call-interactively #'tiles-keyword-search)
      (call-interactively #'tiles-tag-search))))

(defun tiles-switch-to-tag-search ()
  "Switch to tag search."
  (interactive)
  (tiles-quit)
  (call-interactively #'tiles-tag-search))

(defun tiles-switch-to-keyword-search ()
  "Switch to keyword search."
  (interactive)
  (tiles-quit)
  (call-interactively #'tiles-keyword-search))

(defun tiles-quit ()
  "Quit TILES view and clean up buffers."
  (interactive)
  (let ((search-buf (get-buffer tiles--search-buffer-name))
        (preview-buf (get-buffer tiles--preview-buffer-name))
        (stitched-buf (get-buffer tiles--stitched-buffer-name)))
    (when search-buf (kill-buffer search-buf))
    (when preview-buf (kill-buffer preview-buf))
    (when stitched-buf (kill-buffer stitched-buf)))
  (delete-other-windows))

;;; Stitched View Mode

(defvar tiles-stitched-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'tiles-stitched-next)
    (define-key map (kbd "p") #'tiles-stitched-prev)
    (define-key map (kbd "RET") #'tiles-stitched-open)
    (define-key map (kbd "e") #'tiles-stitched-open)
    (define-key map (kbd "SPC") #'tiles-stitched-toggle-view)
    (define-key map (kbd "r") #'tiles-refine-search)
    (define-key map (kbd "q") #'tiles-quit)
    map)
  "Keymap for `tiles-stitched-view-mode'.")

(define-derived-mode tiles-stitched-view-mode org-mode "Tiles-Stitched"
  "Major mode for TILES stitched view."
  (setq buffer-read-only t))

(defvar-local tiles--stitched-boundaries nil
  "List of (start-pos . filepath) for each note in stitched view.")

(defun tiles--show-stitched-view (results)
  "Display RESULTS in stitched view."
  (delete-other-windows)
  (let ((buf (get-buffer-create tiles--stitched-buffer-name))
        (boundaries nil))
    (switch-to-buffer buf)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (dolist (file results)
        (let ((note-data (tiles--parse-note-file file)))
          (when note-data
            (push (cons (point) file) boundaries)
            (insert (tiles--strip-private-paragraphs
                     (plist-get note-data :content)))
            (insert "\n\n"))))
      (goto-char (point-min)))
    (tiles-stitched-view-mode)
    (setq tiles--stitched-boundaries (nreverse boundaries))
    ;; Kill other tiles buffers
    (let ((search-buf (get-buffer tiles--search-buffer-name))
          (preview-buf (get-buffer tiles--preview-buffer-name)))
      (when search-buf (kill-buffer search-buf))
      (when preview-buf (kill-buffer preview-buf)))))

(defun tiles--stitched-current-file ()
  "Get the filepath of the note at point in stitched view."
  (let ((pos (point))
        (result nil))
    (dolist (boundary tiles--stitched-boundaries)
      (when (<= (car boundary) pos)
        (setq result (cdr boundary))))
    result))

(defun tiles-stitched-next ()
  "Jump to next note boundary in stitched view."
  (interactive)
  (let ((pos (point))
        (next-pos nil))
    (dolist (boundary tiles--stitched-boundaries)
      (when (and (> (car boundary) pos) (not next-pos))
        (setq next-pos (car boundary))))
    (if next-pos
        (goto-char next-pos)
      (message "No more notes"))))

(defun tiles-stitched-prev ()
  "Jump to previous note boundary in stitched view."
  (interactive)
  (let ((pos (point))
        (prev-pos nil))
    (dolist (boundary tiles--stitched-boundaries)
      (when (< (car boundary) pos)
        (setq prev-pos (car boundary))))
    (if prev-pos
        (goto-char prev-pos)
      (message "At first note"))))

(defun tiles-stitched-open ()
  "Open the source file of the note at point."
  (interactive)
  (let ((filepath (tiles--stitched-current-file)))
    (if filepath
        (progn
          (tiles-quit)
          (find-file filepath)
          (tiles--enable-tag-line-fontification)
          (when tiles-focus-default
            (tiles-focus-mode 1)))
      (message "No note at point"))))

(defun tiles-stitched-toggle-view ()
  "Toggle back to search view from stitched view."
  (interactive)
  (if tiles--current-search-results
      (tiles--show-search-view tiles--current-search-results)
    (message "No search results to display")))

;;; Dynamic Blocks

(defun tiles--dblock-get-files (params)
  "Get filtered and sorted tile files based on PARAMS plist.
Supports :tags, :keywords, :sort (\"newest\" or \"oldest\"), and :limit."
  (let* ((tags (plist-get params :tags))
         (keywords (plist-get params :keywords))
         (sort-order (or (plist-get params :sort) "newest"))
         (limit (plist-get params :limit))
         (files (tiles--get-all-tile-files))
         (filtered
          (seq-filter
           (lambda (file)
             (let ((note-data (tiles--parse-note-file file)))
               (and note-data
                    (or (not tags)
                        (tiles--note-matches-tag-p
                         note-data (tiles--parse-tag-query tags)))
                    (or (not keywords)
                        (tiles--note-matches-keyword-p
                         note-data (split-string keywords " " t))))))
           files)))
    (when (equal sort-order "oldest")
      (setq filtered (nreverse filtered)))
    (if (and limit (> limit 0))
        (seq-take filtered limit)
      filtered)))

(defun org-dblock-write:tiles-notes (params)
  "Dynamic block: insert a list of matching TILES notes as org links.
PARAMS supports :tags, :keywords, :sort (\"newest\"/\"oldest\"), :limit."
  (let ((files (tiles--dblock-get-files params)))
    (if (not files)
        (insert "  N/A\n")
      (dolist (file files)
        (let* ((fname (file-name-nondirectory file))
               (timestamp (tiles--filename-to-timestamp fname))
               (note-data (tiles--parse-note-file file))
               (preview (if note-data
                            (let* ((content (tiles--strip-private-paragraphs
                                            (or (plist-get note-data :content) "")))
                                   (line (string-trim
                                          (or (car (split-string content "\n" t)) ""))))
                              (if (> (length line) 80)
                                  (concat (substring line 0 77) "...")
                                line))
                          ""))
               (tags (when note-data (plist-get note-data :tags)))
               (tag-str (if tags
                            (concat "  " (mapconcat #'identity tags "/"))
                          "")))
          (insert (format "- [[file:%s][%s]] %s%s\n" file timestamp preview tag-str)))))))

(defun org-dblock-write:tiles-files (params)
  "Dynamic block: embed the contents of matching TILES notes.
PARAMS supports :tags, :keywords, :sort (\"newest\"/\"oldest\"), :limit,
and :separator (string between notes, default blank line)."
  (let ((files (tiles--dblock-get-files params))
        (separator (or (plist-get params :separator) "\n")))
    (if (not files)
        (insert "N/A\n")
      (let ((first t))
        (dolist (file files)
          (let ((note-data (tiles--parse-note-file file)))
            (when note-data
              (if first
                  (setq first nil)
                (insert separator))
              (insert (tiles--strip-private-paragraphs
                       (plist-get note-data :content)) "\n"))))))))

;; Register dblocks for C-c C-x x insertion menu
(with-eval-after-load 'org
  (when (boundp 'org-dynamic-block-alist)
    (add-to-list 'org-dynamic-block-alist
                 '("tiles-notes" . org-dblock-write:tiles-notes))
    (add-to-list 'org-dynamic-block-alist
                 '("tiles-files" . org-dblock-write:tiles-files))))

;;; Keybindings

;;;###autoload
(defvar tiles-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "m") #'tiles-show-notes)
    (define-key map (kbd "n") #'tiles-new)
    (define-key map (kbd "q") #'tiles-quick)
    (define-key map (kbd "y") #'tiles-yank)
    (define-key map (kbd "t") #'tiles-tag-search)
    (define-key map (kbd "k") #'tiles-keyword-search)
    map)
  "Keymap for TILES commands, bound under `C-c m'.")

;;;###autoload
(global-set-key (kbd "C-c m") tiles-command-map)

(provide 'tiles)
;;; tiles.el ends here
