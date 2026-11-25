;;; tiles.el --- Tagged Instant Lightweight Emacs Snippets -*- lexical-binding: t; -*-

;; Author: Claudiu Tănăselia <https://www.tanaselia.ro>
;; Version: 0.5.2
;; Package-Requires: ((emacs "29.1"))
;; Keywords: files, text, convenience
;; URL: https://github.com/ctanas/tiles

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; TILES (Tagged Instant Lightweight Emacs Snippets) is a note-taking
;; system where each note is a title-less, single paragraph stored
;; in its own file.  Notes are organized through tags and bold keywords
;; rather than hierarchies or titles.  A dashboard offers a bird's-eye
;; view of notes and allows note manipulation: create, edit, delete,
;; stitch view mode, and more.
;;
;; Note file format (TYYYYMMDDHHMMSS.org):
;;
;;   This is the paragraph.  *Bold words* are searchable keywords.
;;   /Italic/ and [[links]] are supported.  Inline footnotes[fn:: like
;;   this] are stripped from previews.  Org Mode format is recommended.
;;
;;   && This is a private paragraph.  It is hidden from previews,
;;   stitched views, and dynamic blocks.  Only visible when expanding
;;   a note with TAB in the dashboard or editing the file directly.
;;
;;   tag1/tag2/tag3
;;
;; The last non-empty line is always the tag line; everything above it
;; (separated by a blank line) is content.  Multi-paragraph notes work,
;; but they are discouraged and not considered to be in the spirit of
;; TILES.  Paragraphs starting with && are private (hidden from all
;; views except TAB expansion and direct editing).
;;
;; Features:
;; - Atomic notes: one paragraph per file, no titles required
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
;; - Keyword search with AND/OR/NOT (/ = AND, space = OR, ! = NOT)
;; - Two-panel search view with live preview
;; - Stitched view: search results as flowing text
;; - Quick capture via minibuffer (tiles-quick, tiles-yank)
;; - Touch command to bump a note's timestamp (tiles-touch)
;; - Private paragraphs: && prefix hides content from all views
;;   except TAB expansion and direct file editing
;; - Focus mode: distraction-free editing with centered ~80-char
;;   body, (tiles-focus-mode, enabled by default)
;; - Tag/keyword listing with occurrence counts, sorting, and keyword rename
;; - Tag-line fontification: tag line shown in red when editing notes
;; - Unicode box-drawing dashboard separators (tiles-fancy-separators)
;; - In-memory cache with mtime invalidation for fast repeated access
;; - Org dynamic blocks for embedding note lists/contents in org files
;; - Optional archiving of the web pages notes link to, as WARC files
;;   next to the notes, with a built-in viewer for them.  Off by
;;   default; turn it on with (setq tiles-archive-urls t).
;;
;; Global keybindings (under C-c m):
;;   C-c m m   - Dashboard (tiles-show-notes)
;;   C-c m n   - New note (tiles-new)
;;   C-c m q   - Quick capture via minibuffer (tiles-quick)
;;   C-c m y   - Quick capture from clipboard (tiles-yank)
;;   C-c m t   - Tag search (tiles-tag-search); insert tag at point in capture buffers
;;   C-c m k   - Keyword search (tiles-keyword-search)
;;
;; Dashboard keybindings:
;;   n/p       - Navigate notes
;;   SPC       - Open editable preview split (updates on navigation)
;;   RET       - Open note file
;;   M-RET     - Open note file read-only
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
;;   /         - AND (all terms in group must match)
;;   space     - OR (any group matches)
;;   !prefix   - NOT (term must NOT match)
;;   Example: "emacs Lisp" = Emacs OR Lisp
;;   Example: "emacs/Lisp" = Emacs AND Lisp
;;   Example: "emacs/!Lisp" = Emacs but not Lisp
;;
;; Other commands (M-x):
;;   tiles-touch       - Update a note's timestamp to now (renames file)
;;   tiles-focus-mode  - Toggle distraction-free editing (~80-char centered)
;;   tiles-list-tags   - Browse all unique tags with occurrence counts
;;   tiles-list-keywords - Browse all unique keywords with occurrence counts
;;   tiles-list-rename - Rename keyword under cursor across all notes (R in keyword list)
;;   tiles-clear-cache - Force reload of all note data from disk
;;   tiles-show-notes  - Open the dashboard (also C-c m m)
;;   tiles-new         - Create a new note (also C-c m n)
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
;;   0.5.2 - Keyword search now supports AND (/) and NOT (!) alongside the
;;           existing space-as-OR.  Tag rename: R in the tag list renames a
;;           tag across all notes (parallel to keyword rename).  New
;;           dashboard key w copies the selected note (private && stripped)
;;           to the kill ring.  HTML anchor in tiles-files dblocks is now
;;           opt-in via tiles-dblock-html-anchor, with per-tag suppression
;;           via tiles-dblock-no-anchor-tags.  Polish: g refresh preserves
;;           cursor position; tiles--strip-org-markup also handles
;;           underline/strikethrough/code/verbatim; touch logic deduplicated.
;;   0.5.1 - Similar notes: press m in a read-only tile to list the top
;;           tiles-similar-count notes most similar to it (TF-IDF over
;;           shared content tokens, bold *keywords* boosted). q closes
;;           the read-only buffer.
;;   0.5   - Persistent on-disk cache (tiles-cache-file), reverse tag and
;;           keyword indices for O(matching) search and dashboard filter,
;;           file-notify watch (tiles-watch-files) to catch external edits.
;;   0.4   - First MELPA release;
;;   0.3.5 - Tag mode control: tiles-tag-mode can be `unrestricted' (default),
;;           `inhibit' (no tags, tag search/filter disabled), a list of allowed
;;           tag strings (completion-based, first element is default), or
;;           (required-one-of TAG...) where any tags are accepted but at least
;;           one from the list must be present.
;;           Fix: deleting the last note now refreshes the dashboard to an empty
;;           state instead of leaving the deleted note visible.
;;   0.3.4 - Keyword rename: R in keyword list renames a keyword across all notes.
;;   0.3.3 - Unicode box-drawing dashboard separators, tag-line fontification,
;;           focus mode from stitched view.
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
(require 'face-remap)
(require 'url-parse)
(require 'mailcap)
;; For the web archives (see "Web archives (WARC)" below).
(require 'cl-lib)
(require 'subr-x)
(require 'browse-url)
(require 'dom)
(require 'tabulated-list)
(require 'url-expand)
(require 'url-util)

(declare-function file-notify-add-watch "filenotify" (file flags callback))
(declare-function file-notify-rm-watch "filenotify" (descriptor))

(defconst tiles-version "0.5.2"
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
  "When non-nil, strip all `org-mode' formatting from dashboard previews.
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

(defcustom tiles-year-subfolders nil
  "Subfolder layout for notes inside `tiles-directory'.
When nil, new notes are saved directly in `tiles-directory'.
When t, they go in a per-year subfolder: a note created in 2026 is
saved to TILES-DIR/2026/.
When a string, it is a `format-time-string' template applied to the
note's timestamp.  For example \"%Y/%m\" saves a note created in
September 2026 to TILES-DIR/2026/09/.
When non-nil, changing a note's date (`tiles-touch', `u' and `d' in the
dashboard) also moves it to the subfolder matching its new date.
Existing notes in any location are always found (loading is always
recursive)."
  :type '(choice (const :tag "No subfolders" nil)
                 (const :tag "Per year (YYYY/)" t)
                 (string :tag "Format string, e.g. \"%Y/%m\""))
  :group 'tiles)

(defcustom tiles-cache-file
  (locate-user-emacs-file "tiles-cache.el")
  "File used to persist the parsed-note cache between Emacs sessions.
Set to nil to disable persistence; the cache will then be rebuilt on
demand each session.  Entries are revalidated against file mtime on
access, so a stale cache file is always safe."
  :type '(choice file (const :tag "Disabled" nil))
  :group 'tiles)

(defcustom tiles-watch-files t
  "When non-nil, use `file-notify' to watch `tiles-directory' for changes.
Lets TILES detect external edits (e.g. from another Emacs, mobile sync,
or git pull) and invalidate its caches automatically."
  :type 'boolean
  :group 'tiles)

(defcustom tiles-tag-mode 'unrestricted
  "Controls how tags are handled in TILES.

Possible values:
- `unrestricted' (default): tags work normally; any tag string is accepted.
- `inhibit': tags are disabled.  Capture does not prompt for tags, notes
  are saved with a placeholder tag internally, and tag-based search and
  filtering in the dashboard are disabled.
- A list of strings: only those tags are accepted.  Prompts use completion
  restricted to the list.  The first element is applied as the default when
  the user provides no input.
- (required-one-of TAG...): any tags are accepted, but at least one from
  the list must be present.  Prompts suggest the required tags via
  completion (free input is still allowed).  Example:
  (required-one-of \"work\" \"personal\" \"journal\")
  A TAG may also be (regexp RE), which every tag RE matches as a whole
  satisfies (case-sensitively); prompts show RE and suggest the tags
  in use that it matches.  Example, any year tag such as y2026:
  (required-one-of \"work\" (regexp \"y20[0-9][0-9]\"))"
  :type '(choice (const :tag "Unrestricted (default)" unrestricted)
                 (const :tag "Inhibit tags" inhibit)
                 (repeat :tag "Allowed tags list (only these accepted)" string)
                 (sexp :tag "At least one required (free input otherwise): (required-one-of \"tag1\" ...)"))
  :group 'tiles)

(defcustom tiles-dblock-html-anchor nil
  "When non-nil, `tiles-files' dblocks emit an HTML anchor before each note.
The anchor is an `@@html:<a id=\"...\">@@' Org export snippet that lets
exported HTML link to individual notes by timestamp.  Notes whose tags
include any member of `tiles-dblock-no-anchor-tags' skip the anchor."
  :type 'boolean
  :group 'tiles)

(defcustom tiles-dblock-no-anchor-tags nil
  "List of tag strings that suppress the HTML anchor in `tiles-files' dblocks.
Only meaningful when `tiles-dblock-html-anchor' is non-nil.  A note that
has any of these tags will not get an HTML anchor."
  :type '(repeat string)
  :group 'tiles)

(defcustom tiles-archive-urls nil
  "When non-nil, archive the web pages that notes link to.
Off by default; turn it on with (setq tiles-archive-urls t).

Each time a note is saved, the http and https links in it that are not
archived yet are fetched in the background and saved, one WARC file
per link.  The files go in the warc/ folder of
`tiles-directory', in the same subfolders as the notes, and are named
after the note with letters added: the links of T20260924101500.org
are saved in T20260924101500a.warc.gz, T20260924101500b.warc.gz, and so
on.  Pages that fail to download are tried again the next time the
note is saved.  Links to `tiles-archive-exclude-hosts' are skipped.
A note visited in a buffer lists its archives below its tag line;
RET on one shows the archived page.

While it is on, Emacs also opens the WARC files it visits with
`tiles-warc-visit', and RET in Dired opens them with `tiles-warc-open'.
While it is nil, nothing of this runs: no page is fetched, no list is
shown, and Emacs opens WARC files as any other file."
  :type 'boolean
  :group 'tiles)

(defcustom tiles-archive-exclude-hosts nil
  "Host names whose pages `tiles-archive-urls' does not archive.
A host name also covers its subdomains: \"example.com\" excludes
links to www.example.com as well."
  :type '(repeat string)
  :group 'tiles)

(defcustom tiles-archive-exclude-file-types nil
  "File types that `tiles-archive-urls' does not archive.
They are given as file name extensions, such as \"pdf\".  A link whose
URL ends in one of them is not downloaded at all; any other link is
not saved if the server says it is a file of one of these types, and
it is not downloaded again for the rest of the Emacs session."
  :type '(repeat string)
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

(defvar tiles--tag-index (make-hash-table :test 'equal)
  "Reverse index mapping each tag string to a list of filepaths.")

(defvar tiles--keyword-index (make-hash-table :test 'equal)
  "Reverse index mapping each normalized keyword to a list of filepaths.")

(defvar tiles--file-index-record (make-hash-table :test 'equal)
  "Snapshot of the (TAGS . KEYWORDS) indexed for each filepath.
Used to remove a file cleanly from `tiles--tag-index' and
`tiles--keyword-index' when it is re-parsed or evicted.")

(defvar tiles--content-token-cache (make-hash-table :test 'equal)
  "Cache of (MTIME . TOKEN-LIST) per filepath for content-token similarity.")

(defvar tiles--cache-loaded nil
  "Non-nil after the persistent cache has been loaded this session.")

(defvar tiles--index-complete nil
  "Non-nil when the reverse index is known to cover every on-disk note.")

(defvar tiles--file-watches nil
  "Alist of (DIR . DESCRIPTOR) for active `file-notify' watches.")

(defvar tiles--archive-queue nil
  "Links waiting to be archived, oldest first.
Each element is a cons (NOTE . URL), NOTE being the note's filepath.")

(defvar tiles--archive-current nil
  "The (NOTE . URL) link being archived, or nil.")

(defvar tiles--archive-results (make-hash-table :test 'equal)
  "Results so far of the notes whose links are being archived.
Maps a note's filepath to (ARCHIVED . NOT-ARCHIVED), the number of its
links archived and a list of (URL . REASON) for the others, until its
summary is shown.")

(defvar tiles--archive-skipped (make-hash-table :test 'equal)
  "Links found to be of a type in `tiles-archive-exclude-file-types'.
Maps each URL to its media type, so it is not downloaded again during
the session.")

(defvar tiles--archive-idle-delay 1
  "Seconds Emacs must be idle before the download of a link starts.
Looking up the host name of a link can pause Emacs briefly, so it waits
for a pause in typing.")

(defvar tiles-debug nil
  "When non-nil, display performance diagnostics after each dashboard load.
Enable at runtime with (setq tiles-debug t).  Off by default.")

(defvar tiles--debug-cache-hits 0
  "Cache hit count during the last dashboard load.  Internal debug counter.")

(defvar tiles--debug-cache-misses 0
  "Cache miss count (disk reads) during the last dashboard load.
Internal debug counter.")

(defvar tiles--preview-buffer-name "*Tile Preview*"
  "Name of the preview buffer.")

(defvar tiles--search-buffer-name "*Tiles Search Results*"
  "Name of the search results buffer.")

(defvar tiles--stitched-buffer-name "*Tiles Stitched View*"
  "Name of the stitched view buffer.")

(defvar tiles--notes-buffer-name "*Tiles Notes*"
  "Name of the notes viewer buffer.")

(defvar tiles--notes-filter nil
  "Current dashboard filter as (TYPE . QUERY) or nil.
TYPE is `tag' or `keyword'.")

(defvar tiles--notes-exclude nil
  "List of tags to exclude from dashboard display and search.
Set by F in the dashboard, cleared by c.")

(defvar-local tiles--line-target-width 135
  "Computed target width for note lines in the dashboard.
Equal to `tiles-preview-length' + `tiles-line-padding' +
longest tag string length.")
(put 'tiles--line-target-width 'permanent-local t)

(defvar tiles--notes-page 0
  "Current page offset in the dashboard (0-indexed).")

(defvar tiles--preview-file nil
  "Filepath currently shown in the preview split.")

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
                   (today-days (time-to-days now))
                   (phase-days (time-to-days phase-time))
                   (diff-days (- phase-days today-days)))
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
          (format "%s MOON TOMORROW!" short))
         (t
          (format "%d days until %s: %s" best-days best-name best-date)))))))

(defun tiles--ensure-directory ()
  "Ensure `tiles-directory' exists."
  (unless (file-exists-p tiles-directory)
    (make-directory tiles-directory t)))

(defun tiles--filename-to-time (filename)
  "Return the local time encoded in FILENAME (TYYYYMMDDHHMMSS.org), or nil."
  (when (string-match "T\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)" filename)
    (encode-time (append (mapcar (lambda (i) (string-to-number (match-string i filename)))
                                 '(6 5 4 3 2 1))
                         '(nil -1 nil)))))

(defun tiles--note-filepath (filename &optional current)
  "Return the full path where the note named FILENAME belongs.
When `tiles-year-subfolders' is non-nil, that is the subfolder of
`tiles-directory' matching the timestamp in FILENAME.  Otherwise it is
the directory of CURRENT (the note's existing path) or, for a new
note, `tiles-directory'."
  (let ((time (and tiles-year-subfolders (tiles--filename-to-time filename))))
    (expand-file-name
     filename
     (cond (time (expand-file-name
                  (format-time-string (if (stringp tiles-year-subfolders)
                                          tiles-year-subfolders
                                        "%Y")
                                      time)
                  tiles-directory))
           (current (file-name-directory current))
           (t (expand-file-name tiles-directory))))))

(defun tiles--new-note-filepath ()
  "Return an unused path for a new note, creating its directory if needed."
  (let ((filepath (tiles--note-filepath (tiles--generate-filename))))
    (while (file-exists-p filepath)
      (sleep-for 0.1)
      (setq filepath (tiles--note-filepath (tiles--generate-filename))))
    (make-directory (file-name-directory filepath) t)
    filepath))

(defun tiles--rename-note (filepath new-filepath)
  "Rename note FILEPATH to NEW-FILEPATH and evict the old path from the cache.
Creates the directory of NEW-FILEPATH if needed.  The web archives of
the note are renamed with it."
  (make-directory (file-name-directory new-filepath) t)
  (rename-file filepath new-filepath)
  (tiles--evict-file filepath)
  (tiles--archive-rename filepath new-filepath))

(defun tiles--generate-timestamp ()
  "Generate a timestamp string for file naming."
  (format-time-string "T%Y%m%d%H%M%S"))

(defun tiles--generate-filename ()
  "Generate a new filename with timestamp."
  (concat (tiles--generate-timestamp) ".org"))

(defun tiles--get-all-tile-files ()
  "Get all tile files from `tiles-directory' and subdirectories.
Returned list is sorted newest first."
  (when (file-exists-p tiles-directory)
    (let ((files (directory-files-recursively
                  tiles-directory "^T[0-9]\\{14\\}\\.org$" nil
                  ;; Do not even list the web archives: they hold no notes.
                  (lambda (dir) (not (tiles--archive-path-p dir))))))
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
          (progn
            (when tiles-debug
              (setq tiles--debug-cache-hits (1+ tiles--debug-cache-hits)))
            (cdr cached))
        (progn
          (when tiles-debug
            (setq tiles--debug-cache-misses (1+ tiles--debug-cache-misses)))
          (let ((result (tiles--parse-note-file-uncached filepath)))
            (puthash filepath (cons mtime result) tiles--cache)
            (tiles--index-record filepath result)
            result))))))

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

(defun tiles--clear-memory-cache ()
  "Empty the in-memory note cache and reverse indices."
  (clrhash tiles--cache)
  (clrhash tiles--tag-index)
  (clrhash tiles--keyword-index)
  (clrhash tiles--file-index-record)
  (clrhash tiles--content-token-cache)
  (setq tiles--index-complete nil))

(defun tiles-clear-cache ()
  "Clear the tiles note cache and reverse index.
Removes the persistent cache file as well."
  (interactive)
  (tiles--clear-memory-cache)
  (setq tiles--cache-loaded nil)
  (when (and tiles-cache-file (file-exists-p tiles-cache-file))
    (delete-file tiles-cache-file))
  (message "Tiles cache cleared"))

;;;###autoload
(defun tiles-rescan ()
  "Rescan `tiles-directory' from scratch.
Re-reads every note, rebuilds the tag and keyword indices, and
re-installs the file watches so folders created since the last scan
are watched too.  Use it after moving or editing many notes outside
Emacs.  Refreshes the dashboard when called from it."
  (interactive)
  (tiles--clear-memory-cache)
  ;; Skip the persistent cache: it may still list notes at their old paths.
  (setq tiles--cache-loaded t)
  (tiles--ensure-index)
  (when (string= (buffer-name) tiles--notes-buffer-name)
    (tiles-show-notes))
  (message "Tiles: rescanned %d notes" (hash-table-count tiles--cache)))

;;; Reverse index, persistent cache, and file-notify

(defun tiles--index-record (filepath note-data)
  "Add or refresh FILEPATH's entries in the reverse indices using NOTE-DATA."
  (tiles--index-forget filepath)
  (let ((tags (plist-get note-data :tags))
        (keywords (plist-get note-data :keywords)))
    (dolist (tag tags)
      (let ((files (gethash tag tiles--tag-index)))
        (unless (member filepath files)
          (puthash tag (cons filepath files) tiles--tag-index))))
    (dolist (kw keywords)
      (let ((files (gethash kw tiles--keyword-index)))
        (unless (member filepath files)
          (puthash kw (cons filepath files) tiles--keyword-index))))
    (puthash filepath (cons tags keywords) tiles--file-index-record)))

(defun tiles--index-forget (filepath)
  "Remove FILEPATH from the tag and keyword indices."
  (let ((rec (gethash filepath tiles--file-index-record)))
    (when rec
      (dolist (tag (car rec))
        (let ((files (delete filepath (gethash tag tiles--tag-index))))
          (if files
              (puthash tag files tiles--tag-index)
            (remhash tag tiles--tag-index))))
      (dolist (kw (cdr rec))
        (let ((files (delete filepath (gethash kw tiles--keyword-index))))
          (if files
              (puthash kw files tiles--keyword-index)
            (remhash kw tiles--keyword-index))))
      (remhash filepath tiles--file-index-record))))

(defun tiles--evict-file (filepath)
  "Remove FILEPATH from cache and reverse indices entirely."
  (tiles--index-forget filepath)
  (remhash filepath tiles--cache))

(defun tiles--load-disk-cache-once ()
  "Load the persistent cache from `tiles-cache-file' on the first call.
Subsequent calls are no-ops.  Entries are revalidated by mtime when first
accessed, so a stale file is always safe."
  (unless tiles--cache-loaded
    (setq tiles--cache-loaded t)
    (when (and tiles-cache-file (file-exists-p tiles-cache-file))
      (condition-case err
          (let* ((data (with-temp-buffer
                         (insert-file-contents tiles-cache-file)
                         (read (current-buffer))))
                 (version (plist-get data :version))
                 (entries (plist-get data :entries)))
            (when (eq version 1)
              (dolist (entry entries)
                (let* ((file (car entry))
                       (mtime-and-parsed (cdr entry)))
                  (when (and (stringp file) (file-exists-p file))
                    (puthash file mtime-and-parsed tiles--cache)
                    (tiles--index-record file (cdr mtime-and-parsed)))))))
        (error
         (message "Tiles: failed to load cache from %s: %s"
                  tiles-cache-file (error-message-string err)))))))

(defun tiles--save-disk-cache ()
  "Persist `tiles--cache' to `tiles-cache-file' if enabled."
  (when (and tiles-cache-file
             (> (hash-table-count tiles--cache) 0))
    (condition-case err
        (let (entries)
          (maphash (lambda (file v) (push (cons file v) entries))
                   tiles--cache)
          (let ((dir (file-name-directory tiles-cache-file)))
            (when dir (make-directory dir t)))
          (with-temp-file tiles-cache-file
            (let ((print-level nil) (print-length nil))
              (prin1 (list :version 1 :entries entries) (current-buffer)))))
      (error
       (message "Tiles: failed to save cache to %s: %s"
                tiles-cache-file (error-message-string err))))))

(defun tiles--ensure-index ()
  "Ensure the reverse index covers all current on-disk notes.
Loads the persistent cache, parses any uncached files, evicts entries
whose files have disappeared, and installs `file-notify' watches."
  (tiles--load-disk-cache-once)
  (unless tiles--index-complete
    (let ((files (tiles--get-all-tile-files))
          (current (make-hash-table :test 'equal)))
      (dolist (file files)
        (puthash file t current)
        (tiles--parse-note-file file))
      (let (stale)
        (maphash (lambda (file _)
                   (unless (gethash file current)
                     (push file stale)))
                 tiles--cache)
        (dolist (file stale)
          (tiles--evict-file file)))
      (setq tiles--index-complete t)
      (when tiles-watch-files
        (tiles--start-file-watch)))))

(defun tiles--files-with-tag-substring (qtag)
  "Return files whose tags contain QTAG as a substring."
  (let (out)
    (maphash (lambda (tag files)
               (when (string-match-p (regexp-quote qtag) tag)
                 (setq out (append files out))))
             tiles--tag-index)
    (delete-dups out)))

(defun tiles--files-with-keyword-substring (qkw)
  "Return files whose keywords contain QKW (normalized) as a substring."
  (let ((normalized (tiles--normalize-keyword qkw))
        out)
    (maphash (lambda (kw files)
               (when (string-match-p (regexp-quote normalized) kw)
                 (setq out (append files out))))
             tiles--keyword-index)
    (delete-dups out)))

(defun tiles--files-matching-tag-query (query-and-groups)
  "Return files matching tag QUERY-AND-GROUPS via the reverse index.
AND within each group, OR across groups."
  (tiles--ensure-index)
  (let (result)
    (dolist (and-group query-and-groups)
      (when and-group
        (let ((files (tiles--files-with-tag-substring (car and-group))))
          (dolist (qtag (cdr and-group))
            (setq files (seq-intersection
                         files
                         (tiles--files-with-tag-substring qtag)
                         #'equal)))
          (setq result (append files result)))))
    (delete-dups result)))

(defun tiles--files-matching-keyword-and-group (and-group)
  "Return files matching every term in AND-GROUP.
AND-GROUP is a list of (NEGATE . TERM) pairs as produced by
`tiles--parse-keyword-query'.  Positive terms intersect; negative terms
are subtracted.  An all-negative group starts from the universe of all
indexed files."
  (let ((positive (seq-filter (lambda (t1) (not (car t1))) and-group))
        (negative (seq-filter #'car and-group))
        files)
    (cond
     (positive
      (setq files (tiles--files-with-keyword-substring (cdr (car positive))))
      (dolist (term (cdr positive))
        (setq files (seq-intersection
                     files
                     (tiles--files-with-keyword-substring (cdr term))
                     #'equal))))
     (negative
      (setq files (hash-table-keys tiles--file-index-record))))
    (dolist (term negative)
      (let ((excluded (tiles--files-with-keyword-substring (cdr term))))
        (setq files (seq-difference files excluded #'equal))))
    files))

(defun tiles--files-matching-keyword-query (query-and-groups)
  "Return files matching keyword QUERY-AND-GROUPS via the reverse index.
QUERY-AND-GROUPS is the output of `tiles--parse-keyword-query'.
AND within each group, OR across groups; `!'-prefixed terms negate."
  (tiles--ensure-index)
  (let (out)
    (dolist (and-group query-and-groups)
      (when and-group
        (setq out (append (tiles--files-matching-keyword-and-group and-group)
                          out))))
    (delete-dups out)))

(defun tiles--sort-newest-first (files)
  "Return FILES sorted by filename descending (newest first)."
  (sort (copy-sequence files)
        (lambda (a b)
          (string> (file-name-nondirectory a)
                   (file-name-nondirectory b)))))

(defun tiles--watched-directories ()
  "Return the list of directories that should be watched for changes.
That is `tiles-directory', every directory containing tile files, and
every directory in between (e.g. YYYY/ for notes in YYYY/MM/), so a
new subfolder created inside one of them is noticed."
  (let ((root (directory-file-name (expand-file-name tiles-directory)))
        dirs)
    (when (file-exists-p root)
      (push root dirs)
      (dolist (file (tiles--get-all-tile-files))
        (let ((dir (directory-file-name
                    (file-name-directory (expand-file-name file)))))
          (while (and (not (member dir dirs))
                      (string-prefix-p root dir))
            (push dir dirs)
            (setq dir (directory-file-name (file-name-directory dir)))))))
    dirs))

(defun tiles--start-file-watch ()
  "Install `file-notify' watches for all current note directories.
Replaces any watches installed by a previous call."
  (when (require 'filenotify nil t)
    (tiles--stop-file-watch)
    (dolist (dir (tiles--watched-directories))
      (condition-case nil
          (let ((desc (file-notify-add-watch
                       dir '(change attribute-change)
                       #'tiles--handle-file-event)))
            (push (cons dir desc) tiles--file-watches))
        (file-notify-error nil)))))

(defun tiles--stop-file-watch ()
  "Remove all active `file-notify' watches."
  (dolist (entry tiles--file-watches)
    (ignore-errors (file-notify-rm-watch (cdr entry))))
  (setq tiles--file-watches nil))

(defun tiles--tile-file-p (path)
  "Return non-nil if PATH names a TILES note file."
  (and (stringp path)
       (string-match-p "T[0-9]\\{14\\}\\.org\\'" path)))

(defun tiles--handle-file-event (event)
  "React to a `file-notify' EVENT touching a tile file."
  (let ((action (nth 1 event))
        (file (nth 2 event))
        (file2 (nth 3 event)))
    (cond
     ((eq action 'deleted)
      (when (tiles--tile-file-p file)
        (tiles--evict-file file)))
     ((eq action 'renamed)
      (when (tiles--tile-file-p file)
        (tiles--evict-file file))
      (when (tiles--tile-file-p file2)
        (remhash file2 tiles--cache)
        (when (file-exists-p file2)
          (tiles--parse-note-file file2)
          (tiles--archive-note-later file2))))
     ((memq action '(changed attribute-changed))
      (when (tiles--tile-file-p file)
        (remhash file tiles--cache)
        (when (eq action 'changed)
          (tiles--archive-note-later file))))
     ((eq action 'created)
      (cond
       ((tiles--tile-file-p file)
        (when (file-exists-p file)
          (tiles--parse-note-file file)
          (tiles--archive-note-later file)))
       ;; A new folder may hold notes, but the web archives never do.
       ((and file (file-directory-p file) (not (tiles--archive-path-p file)))
        (setq tiles--index-complete nil)
        ;; It is not watched yet, so look at the notes it came with.
        (when tiles-archive-urls
          (dolist (note (directory-files-recursively file "^T[0-9]\\{14\\}\\.org$"))
            (tiles--archive-note-later note)))))))))

(add-hook 'kill-emacs-hook #'tiles--save-disk-cache)

(defun tiles--private-paragraph-p (paragraph)
  "Return non-nil if PARAGRAPH start with &&."
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
Bold words in `org-mode' are surrounded by *asterisks*.
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

(defun tiles--parse-keyword-query (query)
  "Parse QUERY into AND-groups for keyword matching.
`/' means AND (all terms in the group must match), space means OR (any
group matches).  A `!' prefix on a term negates it (the keyword must
NOT be present in the note).  Hyphens inside a term are preserved here;
normalization happens at match time.

Each AND-group is returned as a list of (NEGATE . TERM) cons cells.
For example, \"foo/!bar baz\" parses to
\((nil . \"foo\") (t . \"bar\")) ((nil . \"baz\"))."
  (mapcar (lambda (part)
            (mapcar (lambda (term)
                      (if (string-prefix-p "!" term)
                          (cons t (substring term 1))
                        (cons nil term)))
                    (split-string part "/" t)))
          (split-string query " " t)))

(defun tiles--keyword-present-p (qkw note-keywords)
  "Return non-nil if QKW (after normalization) matches any of NOTE-KEYWORDS."
  (let ((normalized (tiles--normalize-keyword qkw)))
    (seq-some (lambda (nkw)
                (string-match-p (regexp-quote normalized) nkw))
              note-keywords)))

(defun tiles--note-matches-keyword-p (note-data query-and-groups)
  "Check if NOTE-DATA matches QUERY-AND-GROUPS.
QUERY-AND-GROUPS is the output of `tiles--parse-keyword-query': a list
of AND-groups, each a list of (NEGATE . TERM) pairs.  All terms in a
group must match (positive terms present, negative terms absent);
groups are OR'd together."
  (let ((note-keywords (plist-get note-data :keywords)))
    (seq-some
     (lambda (and-group)
       (seq-every-p
        (lambda (term)
          (let ((has (tiles--keyword-present-p (cdr term) note-keywords)))
            (if (car term) (not has) has)))
        and-group))
     query-and-groups)))

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

;;; Link archiving

;; Off unless `tiles-archive-urls' is set.  Pages are fetched and saved
;; by the WARC code near the end of this file (prefix `tiles-warc-'),
;; which also sets up how Emacs opens WARC files while archiving is on
;; (`tiles--warc-setup', called by `tiles--archive-urls-watcher').

(defvar tiles-warc-capture-compress)

(defun tiles--archive-enabled-p ()
  "Return non-nil if the web pages linked from notes are to be archived."
  tiles-archive-urls)

(defun tiles--archive-directory ()
  "Return the directory holding the web archives of linked pages."
  (expand-file-name "warc" tiles-directory))

(defun tiles--archive-path-p (path)
  "Return non-nil if PATH is `tiles--archive-directory' or inside it."
  (string-prefix-p (file-name-as-directory (tiles--archive-directory))
                   (file-name-as-directory (expand-file-name path))))

(defun tiles--note-archive-directory (note)
  "Return the directory for the web archives of NOTE, or nil.
It mirrors, inside `tiles--archive-directory', the folder where NOTE
belongs (see `tiles--note-filepath'): warc/2026/09/ for a note of
September 2026 when `tiles-year-subfolders' is \"%Y/%m\", even while
the note is elsewhere, as when sync moves it.  Return nil if NOTE is
not in `tiles-directory'."
  (let* ((root (expand-file-name tiles-directory))
         (note (expand-file-name note))
         (relative (file-relative-name (file-name-directory note) root)))
    (unless (or (string-prefix-p "../" relative)
                (file-name-absolute-p relative))
      (let ((home (file-name-directory
                   (tiles--note-filepath (file-name-nondirectory note) note))))
        (file-name-as-directory
         (expand-file-name (file-relative-name home root) (tiles--archive-directory)))))))

(defun tiles--archive-suffix (n)
  "Return the letters added to the name of a note for its Nth web archive.
N counts from 0: the letters are a to z, then aa, ab, and so on."
  (let ((suffix ""))
    (setq n (1+ n))
    (while (> n 0)
      (setq n (1- n)
            suffix (concat (string (+ ?a (% n 26))) suffix)
            n (/ n 26)))
    suffix))

(defun tiles--note-archives (note)
  "Return the web archive files of NOTE."
  (let ((dir (tiles--note-archive-directory note)))
    (when (and dir (file-directory-p dir))
      (directory-files dir t (concat "\\`" (regexp-quote (file-name-base note))
                                     "[a-z]+\\.warc\\(?:\\.gz\\)?\\'")))))

(defun tiles--new-archive-file (note)
  "Return the file for a new web archive of NOTE, or nil.
It is named after NOTE, with the first unused suffix letters."
  (when-let* ((dir (tiles--note-archive-directory note)))
    (let ((base (expand-file-name (file-name-base note) dir))
          (n 0))
      (while (let ((suffix (tiles--archive-suffix n)))
               (or (file-exists-p (concat base suffix ".warc"))
                   (file-exists-p (concat base suffix ".warc.gz"))))
        (setq n (1+ n)))
      (concat base (tiles--archive-suffix n)
              (if tiles-warc-capture-compress ".warc.gz" ".warc")))))

(defun tiles--note-urls (content)
  "Return the http and https URLs linked from CONTENT, without duplicates.
Both Org bracket links and plain URLs count.  They are returned in the
order they appear in CONTENT."
  (with-temp-buffer
    (insert content)
    (let (found)
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[\\(https?://[^][]+\\)\\]" nil t)
        (push (cons (match-beginning 0) (match-string-no-properties 1)) found)
        ;; Hide the link from the search for plain URLs below.
        (replace-match (make-string (- (match-end 0) (match-beginning 0)) ?\s) t t))
      (goto-char (point-min))
      (while (re-search-forward "https?://[^][ \t\n<>\"]+" nil t)
        (let ((url (match-string-no-properties 0)))
          ;; Leave out the punctuation that follows a URL in a sentence.
          (while (or (string-match-p "[.,;:!?'\"]\\'" url)
                     (and (string-suffix-p ")" url)
                          (not (string-match-p "(" url))))
            (setq url (substring url 0 -1)))
          (push (cons (match-beginning 0) url) found)))
      (delete-dups (mapcar #'cdr (sort found (lambda (a b) (< (car a) (car b)))))))))

(defun tiles--archive-excluded-p (url)
  "Return non-nil if the page at URL is not to be archived.
That is if URL is on a host in `tiles-archive-exclude-hosts', or ends
in an extension in `tiles-archive-exclude-file-types'."
  (let* ((parsed (url-generic-parse-url url))
         (host (downcase (or (url-host parsed) "")))
         (path (car (url-path-and-query parsed)))
         (extension (and path (file-name-extension path))))
    (or (seq-some (lambda (excluded)
                    (let ((excluded (downcase excluded)))
                      (or (string= host excluded)
                          (string-suffix-p (concat "." excluded) host))))
                  tiles-archive-exclude-hosts)
        (and extension
             (member-ignore-case extension tiles-archive-exclude-file-types)
             t))))

(defun tiles--archive-excluded-type-p (type)
  "Return non-nil if the media TYPE is in `tiles-archive-exclude-file-types'."
  (seq-some (lambda (extension)
              (equal type (mailcap-extension-to-mime (concat "." extension))))
            tiles-archive-exclude-file-types))

(defun tiles--note-pending-urls (note)
  "Return the URLs linked from NOTE that still need to be archived.
They are the links to archive, in the form `tiles-warc-canonical-url'
gives, minus those archived already or queued for NOTE."
  (when (and (tiles--tile-file-p note)
             (file-exists-p note)
             (tiles--note-archive-directory note))
    (let* ((note (expand-file-name note))
           (urls (seq-remove
                  #'tiles--archive-excluded-p
                  (delete-dups
                   (delq nil (mapcar (lambda (url)
                                       (ignore-errors (tiles-warc-canonical-url url)))
                                     (tiles--note-urls
                                      (with-temp-buffer
                                        (insert-file-contents note)
                                        (buffer-string)))))))))
      (when urls
        (let ((known (append (mapcan (lambda (file)
                                       (ignore-errors (tiles-warc-archived-urls file)))
                                     (tiles--note-archives note))
                             (mapcar #'cdr (seq-filter (lambda (job)
                                                         (equal (car job) note))
                                                       (cons tiles--archive-current
                                                             tiles--archive-queue))))))
          (seq-remove (lambda (url)
                        (or (member url known)
                            (gethash url tiles--archive-skipped)))
                      urls))))))

(defun tiles--archive-note (note)
  "Archive the web pages linked from NOTE that are not archived yet.
They are queued and fetched in the background, one at a time.  Does
nothing unless `tiles-archive-urls' is non-nil."
  (when (tiles--archive-enabled-p)
    (when-let* ((note (expand-file-name note))
                (new (tiles--note-pending-urls note)))
      (setq tiles--archive-queue
            (append tiles--archive-queue
                    (mapcar (lambda (url) (cons note url)) new)))
      (tiles--archive-next))))

(defun tiles--archive-next ()
  "Archive the next queued link, unless one is being archived.
Its download starts once Emacs has been idle for
`tiles--archive-idle-delay' seconds."
  (unless tiles--archive-current
    (when-let* ((job (pop tiles--archive-queue)))
      (setq tiles--archive-current job)
      (let ((idle (current-idle-time)))
        (if (or (<= tiles--archive-idle-delay 0)
                (and idle (>= (float-time idle) tiles--archive-idle-delay)))
            (run-at-time 0 nil #'tiles--archive-start job)
          (run-with-idle-timer tiles--archive-idle-delay nil
                               #'tiles--archive-start job))))))

(defun tiles--archive-start (job)
  "Start archiving JOB, a (NOTE . URL) cons, then the next queued link."
  (condition-case err
      (tiles-warc-capture-in-background
       (cdr job)
       ;; Chosen once the page is fetched: the note may have been
       ;; renamed meanwhile (which updates JOB), or deleted.
       (lambda (status type)
         (when (<= 200 status 299)
           (if (tiles--archive-excluded-type-p type)
               (ignore (puthash (cdr job) type tiles--archive-skipped))
             (and (file-exists-p (car job))
                  (tiles--new-archive-file (car job))))))
       (lambda (file status problem)
         (let ((skipped (gethash (cdr job) tiles--archive-skipped)))
           ;; The link is done whatever goes wrong while reporting it:
           ;; otherwise no other link would be archived this session.
           (unwind-protect
               (condition-case err
                   (cond
                    (file
                     (tiles--archive-log "Tiles: archived %s in %s"
                                         (cdr job) (file-name-nondirectory file))
                     (when-let* ((buffer (get-file-buffer (car job))))
                       (with-current-buffer buffer
                         (tiles--archive-list-show))))
                    (problem
                     (tiles--archive-log "Tiles: could not archive %s: %s" (cdr job) problem))
                    (skipped
                     (tiles--archive-log "Tiles: did not archive %s: it is %s" (cdr job) skipped))
                    ((not (<= 200 status 299))
                     (tiles--archive-log "Tiles: did not archive %s: the server answered %d"
                                         (cdr job) status)))
                 (error
                  (tiles--archive-log "Tiles: error after archiving %s: %s"
                                      (cdr job) (error-message-string err))))
             (tiles--archive-done job (unless file
                                        (tiles--archive-reason status problem skipped)))))))
    (error
     (tiles--archive-log "Tiles: could not archive %s: %s"
                         (cdr job) (error-message-string err))
     (tiles--archive-done job "error"))))

(defun tiles--archive-log (format-string &rest args)
  "Log a message about one link, formatted from FORMAT-STRING and ARGS.
It goes to the *Messages* buffer without being shown: the echo area
shows one summary per note instead (see `tiles--archive-summary').  In
batch mode, where there is no echo area, it is printed."
  (let ((inhibit-message (not noninteractive)))
    (apply #'message format-string args)))

(defun tiles--archive-reason (status problem skipped)
  "Return why a link was not archived, in a word or two, for the summary.
STATUS, PROBLEM and SKIPPED are the status code, error message and
excluded media type of the attempt, when there are."
  (let ((case-fold-search t))
    (cond
     (skipped (upcase (car (last (split-string skipped "/")))))
     (problem
      (cond ((string-prefix-p "Cannot connect" problem) "cannot connect")
            ((string-prefix-p "No data" problem) "timed out")
            ((string-prefix-p "No HTTP response" problem) "no response")
            ((string-match-p "redirect" problem) "redirects")
            (t "error")))
     ((and status (not (<= 200 status 299))) (number-to-string status))
     (t "note deleted"))))

(defun tiles--archive-done (job reason)
  "Count JOB as archived, or as not archived for REASON, and go on.
Once the last queued link of the note is done, show the summary, then
start archiving the next link."
  (let* ((note (car job))
         (result (or (gethash note tiles--archive-results) (list 0))))
    (if reason
        (setcdr result (append (cdr result) (list (cons (cdr job) reason))))
      (setcar result (1+ (car result))))
    (puthash note result tiles--archive-results)
    (setq tiles--archive-current nil)
    (unless (assoc note tiles--archive-queue)
      (remhash note tiles--archive-results)
      (tiles--archive-summary note result))
    (tiles--archive-next)))

(defun tiles--archive-summary (note result)
  "Show how many links of NOTE were archived.
RESULT is (ARCHIVED . NOT-ARCHIVED): the number of links archived, and
a list of (URL . REASON) for the others."
  (let* ((archived (car result))
         (missed (cdr result))
         (total (+ archived (length missed))))
    (message "Tiles: note %s: %d of %d link%s archived%s"
             (or (tiles--filename-to-timestamp (file-name-nondirectory note))
                 (file-name-base note))
             archived total (if (= total 1) "" "s")
             (if missed
                 (concat "; not archived: "
                         (mapconcat (lambda (miss)
                                      (format "%s (%s)"
                                              (or (url-host (url-generic-parse-url (car miss)))
                                                  (car miss))
                                              (cdr miss)))
                                    missed ", "))
               ""))))

(defun tiles--archive-rename (note new-note)
  "Rename the web archives of NOTE after NEW-NOTE, the note's new name."
  (let ((dir (tiles--note-archive-directory new-note))
        (base (file-name-base note)))
    (when dir
      (dolist (file (tiles--note-archives note))
        (make-directory dir t)
        (rename-file file (expand-file-name
                           (concat (file-name-base new-note)
                                   (substring (file-name-nondirectory file)
                                              (length base)))
                           dir)))))
  ;; Pages still being fetched are saved under the new name.
  (let ((note (expand-file-name note)))
    (dolist (job (cons tiles--archive-current tiles--archive-queue))
      (when (equal (car job) note)
        (setcar job (expand-file-name new-note))))
    (when-let* ((result (gethash note tiles--archive-results)))
      (remhash note tiles--archive-results)
      (puthash (expand-file-name new-note) result tiles--archive-results))))

(defun tiles--archive-after-save ()
  "Archive the new links of the note saved in the current buffer."
  (when (and tiles-archive-urls
             buffer-file-name
             (tiles--tile-file-p buffer-file-name))
    (tiles--archive-note buffer-file-name)))

(defvar tiles--archive-external-delay 60
  "Seconds to wait before archiving the links of a note changed on disk.
The change may come from another device that archives the same links:
waiting lets its archives arrive first, so that pages are not saved
twice.  It also lets a file being written by sync be complete.")

(defvar tiles--archive-scan-timers (make-hash-table :test 'equal)
  "Timers of the notes changed on disk that wait to have their links archived.")

(defun tiles--archive-note-later (note)
  "Archive the new links of NOTE, changed on disk, after a while.
See `tiles--archive-external-delay'.  Each new change of NOTE starts
the wait again."
  (when tiles-archive-urls
    (let ((note (expand-file-name note)))
      (when-let* ((timer (gethash note tiles--archive-scan-timers)))
        (cancel-timer timer))
      (puthash note
               (run-with-timer tiles--archive-external-delay nil
                               (lambda ()
                                 (remhash note tiles--archive-scan-timers)
                                 (tiles--archive-note note)))
               tiles--archive-scan-timers))))

(add-hook 'after-save-hook #'tiles--archive-after-save)

;;;; The list of web archives below a note

;; A note visited in a buffer shows the pages archived from its links
;; below its tag line.  The list is read-only text in the buffer, so
;; point can move onto it, but it is never written to the note's file:
;; `tiles--archive-list-annotate' leaves it out of every save and
;; auto-save.

(defvar tiles--similar-region-start)

(defface tiles-archive-list
  '((t :inherit shadow))
  "Face for the list of web archives shown below a note."
  :group 'tiles)

(defvar-local tiles--archive-list nil
  "Markers (START . END) of the list of web archives below the note, or nil.")
;; The list stays in the buffer when its major mode is set again, so
;; this must stay too, or the list would be saved with the note.
(put 'tiles--archive-list 'permanent-local t)

(defvar tiles-archive-list-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'tiles-open-archive)
    (define-key map [mouse-2] #'tiles-open-archive)
    (define-key map [follow-link] 'mouse-face)
    map)
  "Keymap of the list of web archives below a note.")

(defun tiles--archive-list-text (note)
  "Return the lines that list the web archives of NOTE, or nil if none.
Each line is the address of an archived page."
  (when-let* ((files (tiles--note-archives note)))
    (mapconcat
     (lambda (file)
       (let ((url (car (ignore-errors (tiles-warc-archived-urls file)))))
         (concat
          (propertize (if url
                          ;; Without http://, which Org would make into
                          ;; links of its own.
                          (replace-regexp-in-string "https?://" "" url)
                        (file-name-nondirectory file))
                      'tiles-archive file
                      'help-echo (format "RET or mouse-1: open the archived page of %s"
                                         (or url (file-name-nondirectory file))))
          "\n")))
     files "")))

(defun tiles--archive-list-decorate (start end)
  "Give the list of web archives from START to END its face and keys.
They are overlays: Org removes faces and keymaps from the text itself
when it fontifies it."
  (let ((overlay (make-overlay start end nil t nil)))
    (overlay-put overlay 'face 'tiles-archive-list)
    (overlay-put overlay 'tiles-archive-list t)
    (overlay-put overlay 'evaporate t))
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (when (get-text-property (point) 'tiles-archive)
        (let ((line (make-overlay (point) (min end (line-beginning-position 2))))
              (address (make-overlay (point) (line-end-position))))
          (overlay-put line 'keymap tiles-archive-list-map)
          (overlay-put line 'mouse-face 'highlight)
          (overlay-put line 'tiles-archive-list t)
          (overlay-put line 'evaporate t)
          ;; Shows the address cut to the window, see `tiles--archive-list-fit'.
          (overlay-put address 'tiles-archive-address t)
          (overlay-put address 'tiles-archive-list t)
          (overlay-put address 'evaporate t)))
      (forward-line 1))))

(defun tiles--archive-list-fit ()
  "Keep each address listed below the note on one line of its window.
An address longer than the window is shown cut, ending in an ellipsis;
the text itself stays whole, for `tiles-open-archive' and its tooltip.
Run again when the window changes size (`window-configuration-change-hook')."
  (when tiles--archive-list
    (save-restriction
      ;; The note may be narrowed to a part before the list.
      (widen)
      (let* ((window (get-buffer-window (current-buffer) t))
             (width (and window (window-max-chars-per-line window))))
        (dolist (overlay (overlays-in (car tiles--archive-list) (cdr tiles--archive-list)))
          (when (overlay-get overlay 'tiles-archive-address)
            (let ((address (buffer-substring-no-properties
                            (overlay-start overlay) (overlay-end overlay))))
              (overlay-put overlay 'display
                           (and width (> (string-width address) width)
                                (truncate-string-to-width address width nil nil "…"))))))))))

(defmacro tiles--without-visiting (&rest body)
  "Run BODY, which changes the list of web archives, as a silent change.
It leaves no undo entry, keeps the modified flag, and does not count as
editing the file: that would lock it, or ask what to do if the file
changed on disk.  The change hooks still run, so Org keeps track."
  (declare (indent 0) (debug t))
  `(let ((inhibit-read-only t)
         (buffer-undo-list t)
         (modified (buffer-modified-p)))
     (let ((buffer-file-name nil)
           (buffer-file-truename nil))
       ,@body)
     (restore-buffer-modified-p modified)))

(defun tiles--archive-list-remove ()
  "Remove the list of web archives from the current buffer."
  (when tiles--archive-list
    ;; Widened, since the note may be narrowed to a part before the list.
    (save-restriction
      (widen)
      ;; The list's overlays, and the one hiding blank lines before it.
      (remove-overlays (point-min) (point-max) 'tiles-archive-list t)
      (tiles--without-visiting
        (delete-region (car tiles--archive-list) (cdr tiles--archive-list))))
    (set-marker (car tiles--archive-list) nil)
    (set-marker (cdr tiles--archive-list) nil)
    (setq tiles--archive-list nil)))

(defun tiles--archive-list-show ()
  "Show the web archives of the note in the current buffer below it.
Replace the list shown already, if any.  Shows nothing if
`tiles-archive-urls' is nil, the buffer does not visit a note in
`tiles-directory', or the note has no archives."
  (if (not (and tiles-archive-urls
                buffer-file-name
                (tiles--tile-file-p buffer-file-name)
                (tiles--note-archive-directory buffer-file-name)))
      ;; A list shown before archiving was turned off goes away.
      (tiles--archive-list-remove)
    (save-excursion
      (tiles--archive-list-remove)
      ;; The list goes right below the note: similar notes shown in a
      ;; read-only buffer are hidden first (m shows them again).
      (when tiles--similar-region-start
        (tiles--similar-clear))
      (when-let* ((text (tiles--archive-list-text buffer-file-name)))
        (tiles--without-visiting
          (save-restriction
            (widen)
            ;; Exactly one empty line between the tag line and the list,
            ;; however many the note ends with: the list brings the line
            ;; breaks the note lacks, and blank lines it has beyond the
            ;; tag line's own line break are hidden while the list shows.
            (goto-char (point-max))
            (skip-chars-backward " \t\n")
            (let ((line-end (search-forward "\n" nil t)))
              (when (and line-end (< line-end (point-max)))
                (let ((blank (make-overlay line-end (point-max) nil t nil)))
                  (overlay-put blank 'invisible 'tiles-archive-list)
                  (overlay-put blank 'tiles-archive-list t)
                  (overlay-put blank 'evaporate t))
                (add-to-invisibility-spec 'tiles-archive-list))
              (goto-char (point-max))
              (let ((start (point)))
                (insert (propertize (concat (if line-end "\n" "\n\n") text)
                                    'read-only t 'tiles-archive-list t))
                (tiles--archive-list-decorate start (point))
                ;; Text typed at the end of the note goes before the list.
                (setq tiles--archive-list (cons (copy-marker start t) (point-marker)))))))
        (tiles--archive-list-fit)
        (add-hook 'window-configuration-change-hook #'tiles--archive-list-fit nil t)
        (when font-lock-mode
          (font-lock-flush))))))

(defun tiles--archive-list-annotate (start end)
  "Leave the list of web archives out of what is written to the file.
For `write-region-annotate-functions': when the text from START to END
includes the list, make a copy of it without the list current, which is
then written instead."
  (when (and tiles--archive-list (not (stringp start)))
    (save-restriction
      (widen)
      (let* ((start (or start (point-min)))
             (end (or end (point-max)))
             (list-start (marker-position (car tiles--archive-list)))
             (list-end (marker-position (cdr tiles--archive-list))))
        (when (and (< start list-end) (< list-start end))
          (let ((text (concat (if (< start list-start)
                                  (buffer-substring-no-properties start list-start)
                                "")
                              (if (< list-end end)
                                  (buffer-substring-no-properties list-end end)
                                "")))
                (coding buffer-file-coding-system))
            (set-buffer (generate-new-buffer " *tiles-note*" t))
            (insert text)
            (setq buffer-file-coding-system coding)
            ;; `write-region' calls this in the copy once it is written.
            (setq-local write-region-post-annotation-function #'kill-buffer))))))
  nil)

;; Global rather than buffer-local, so that no change of major mode
;; can remove it while the list is shown.
(add-hook 'write-region-annotate-functions #'tiles--archive-list-annotate)

(defun tiles--archive-list-maybe-show ()
  "Show the web archives of the note visited in the current buffer.
For `find-file-hook' and `after-revert-hook'."
  (when (and buffer-file-name (tiles--tile-file-p buffer-file-name))
    (tiles--archive-list-show)))

(defun tiles--archive-list-remove-for-export (_backend)
  "Leave the list of web archives out of an Org export.
For `org-export-before-processing-functions', which run in the copy of
the buffer that is exported: web archives never go to a website or a
newsletter."
  (let ((inhibit-read-only t)
        (start (point-min)))
    (while (setq start (text-property-any start (point-max) 'tiles-archive-list t))
      (delete-region start (or (next-single-property-change start 'tiles-archive-list)
                               (point-max))))))

(add-hook 'org-export-before-processing-functions #'tiles--archive-list-remove-for-export)

(add-hook 'find-file-hook #'tiles--archive-list-maybe-show)
;; Reverting compares the buffer with the file, so the list is taken
;; out first and shown again afterwards.
(add-hook 'before-revert-hook #'tiles--archive-list-remove)
(add-hook 'after-revert-hook #'tiles--archive-list-maybe-show)

(defun tiles-open-archive (&optional event)
  "Open the web archive listed on the current line below a note.
The archived page is shown by `tiles-warc-open' in a read-only
buffer; press q there to come back.  EVENT is the mouse event, when
there is one."
  (interactive (list last-nonmenu-event))
  (when (mouse-event-p event)
    (posn-set-point (event-start event)))
  (let ((file (get-text-property (point) 'tiles-archive)))
    (unless file
      (user-error "No web archive on this line"))
    (tiles-warc-open file)))

;;; Tag-line fontification

(defvar tiles--similar-region-start)

(defun tiles--tag-line-matcher (limit)
  "Font-lock matcher for the tag line (last non-empty line) in a tiles note.
Only matches when preceded by a blank line, per the TILES note format.
Searches up to LIMIT.  Returns non-nil and sets match data if found."
  (let ((tag-line-pos nil)
        ;; The web archives, then similar notes, may follow the note.
        (search-end (or (and tiles--archive-list
                             (marker-position (car tiles--archive-list)))
                        (and (markerp tiles--similar-region-start)
                             (marker-position tiles--similar-region-start))
                        (point-max))))
    (save-excursion
      (goto-char search-end)
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

;;; Tag mode helpers

(defun tiles--tags-inhibited-p ()
  "Return non-nil when tag usage is suppressed by `tiles-tag-mode'."
  (eq tiles-tag-mode 'inhibit))

(defun tiles--tags-restricted-p ()
  "Return non-nil when tags are restricted to the `tiles-tag-mode' list.
True only for the plain list-of-strings form, not `required-one-of'."
  (and (listp tiles-tag-mode)
       (or (null tiles-tag-mode) (stringp (car tiles-tag-mode)))))

(defun tiles--tags-required-one-of-p ()
  "Return non-nil when `tiles-tag-mode' is the (required-one-of ...) form."
  (and (consp tiles-tag-mode) (eq (car tiles-tag-mode) 'required-one-of)))

(defun tiles--tags-required-list ()
  "Return the entries at least one of which a note's tags must satisfy.
Each is a tag string or (regexp RE), see `tiles-tag-mode'.
Only meaningful when `tiles--tags-required-one-of-p' is non-nil."
  (cdr tiles-tag-mode))

(defun tiles--tag-required-p (tag)
  "Return non-nil when TAG satisfies an entry of `tiles--tags-required-list'.
A string entry is TAG itself; (regexp RE) matches when RE matches the
whole of TAG.  Tags are case-sensitive, so is the match."
  (let ((case-fold-search nil))
    (seq-some (lambda (entry)
                (if (stringp entry)
                    (string= entry tag)
                  (and (eq (car-safe entry) 'regexp)
                       (string-match-p (concat "\\`\\(?:" (cadr entry) "\\)\\'")
                                       tag))))
              (tiles--tags-required-list))))

(defun tiles--tags-required-description ()
  "Return the required tags of `tiles-tag-mode' as text for a prompt.
A (regexp RE) entry shows as RE."
  (mapconcat (lambda (entry) (if (stringp entry) entry (cadr entry)))
             (tiles--tags-required-list) ", "))

(defun tiles--tags-required-candidates ()
  "Return the required tags of `tiles-tag-mode' as completion candidates.
The tag strings of the list, then the tags in use that a (regexp RE)
entry matches: a pattern is never offered as a tag."
  (let* ((entries (tiles--tags-required-list))
         (strings (seq-filter #'stringp entries)))
    (if (seq-every-p #'stringp entries)
        strings
      (tiles--ensure-index)
      (append strings
              (sort (seq-filter (lambda (tag)
                                  (and (not (member tag strings))
                                       (tiles--tag-required-p tag)))
                                (hash-table-keys tiles--tag-index))
                    #'string<)))))

(defun tiles--prompt-for-tags (&optional prompt)
  "Prompt for tags according to `tiles-tag-mode'.
Returns a slash-separated tag string, or nil when tags are inhibited.
PROMPT overrides the default prompt string."
  (cond
   ((tiles--tags-inhibited-p) nil)
   ((tiles--tags-restricted-p)
    (let* ((allowed tiles-tag-mode)
           (default (car allowed))
           (pr (or prompt (format "Tag (default: %s): " default)))
           (input (completing-read pr allowed nil t nil nil default)))
      (if (string-empty-p (string-trim input)) default input)))
   ((tiles--tags-required-one-of-p)
    (let ((pr (or prompt (format "Tags (at least one of: %s): "
                                 (tiles--tags-required-description)))))
      (read-string pr)))
   (t (read-string (or prompt "Tags (separated by /): ")))))

;;; Capture Mode

(defun tiles-insert-tag ()
  "Insert a tag at point using minibuffer completion.
In `inhibit' mode, displays a message instead.
In restricted mode, completes from the allowed list with `require-match'.
In `required-one-of' mode, suggests the required tags (free input allowed).
In unrestricted mode, suggests all tags found in existing notes (free input)."
  (interactive)
  (if (tiles--tags-inhibited-p)
      (message "Tags are inhibited (tiles-tag-mode is 'inhibit).  \
Set tiles-tag-mode to 'unrestricted or a tag list to enable them.")
    (let* ((candidates
            (cond
             ((tiles--tags-restricted-p) tiles-tag-mode)
             ((tiles--tags-required-one-of-p) (tiles--tags-required-candidates))
             (t (hash-table-keys (car (tiles--list-collect-data))))))
           (require-match (and (tiles--tags-restricted-p) t))
           (prompt
            (cond
             ((tiles--tags-required-one-of-p)
              (format "Tag (required: %s): " (tiles--tags-required-description)))
             (t "Tag: ")))
           (tag (completing-read prompt candidates nil require-match)))
      (unless (string-empty-p (string-trim tag))
        (insert tag)))))

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
    (message "TILES: C-c C-c to save, C-c C-k to cancel, C-c m t to insert tag")))

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
    (if (tiles--tags-inhibited-p)
        (message "TILES: Write your note, then C-c C-c to save (C-c C-k to cancel)")
      (message "Write your note, prepend with && any meta paragraph (optional), place tag(s) on the last line (separated by /), then hit C-c"))))

;;;###autoload
(defalias 'tiles-capture #'tiles-new
  "Alias for `tiles-new'.")

(defun tiles-capture-save ()
  "Save the current TILES capture buffer as a new note."
  (interactive)
  (let ((content (buffer-string)))
    ;; Step 1: resolve tags per tiles-tag-mode
    (cond
     ((tiles--tags-inhibited-p)
      ;; Auto-insert placeholder tag when the format is not yet valid
      (unless (eq (tiles--validate-note-format content) t)
        (goto-char (point-max))
        (unless (looking-back "\n\n" nil)
          (unless (looking-back "\n" nil) (insert "\n"))
          (insert "\n"))
        (insert "-\n")
        (setq content (buffer-string))))
     (t
      ;; Prompt for tags if missing
      (let ((valid (tiles--validate-note-format content)))
        (unless (eq valid t)
          (let ((tags (tiles--prompt-for-tags (format "%s. Add tags: " valid))))
            (when (or (null tags) (string-empty-p (string-trim tags)))
              (user-error "Aborted: note needs tags"))
            (goto-char (point-max))
            (unless (looking-back "\n\n" nil)
              (unless (looking-back "\n" nil) (insert "\n"))
              (insert "\n"))
            (insert tags "\n")
            (setq content (buffer-string)))))))
    ;; Step 2: final format validation
    (let ((valid (tiles--validate-note-format content)))
      (unless (eq valid t)
        (user-error "%s" valid))
      ;; Step 3: validate tags per mode
      (when (or (tiles--tags-restricted-p) (tiles--tags-required-one-of-p))
        (let* ((lines (split-string content "\n"))
               (i (1- (length lines))))
          (while (and (>= i 0) (string-empty-p (string-trim (nth i lines))))
            (setq i (1- i)))
          (when (>= i 0)
            (let* ((tag-line (string-trim (nth i lines)))
                   (tags (split-string tag-line "/" t "[ \t]+")))
              (cond
               ((tiles--tags-restricted-p)
                (let ((invalid (seq-remove (lambda (tag) (member tag tiles-tag-mode)) tags)))
                  (when invalid
                    (user-error "Tag(s) not in allowed list: %s.  Allowed: %s"
                                (mapconcat #'identity invalid ", ")
                                (mapconcat #'identity tiles-tag-mode ", ")))))
               ((tiles--tags-required-one-of-p)
                (unless (seq-some #'tiles--tag-required-p tags)
                  (user-error "At least one of these tags is required: %s"
                              (tiles--tags-required-description)))))))))
      ;; Step 4: save
      (when (bound-and-true-p tiles-focus-mode)
        (tiles-focus-mode -1))
      (tiles--ensure-directory)
      (let ((filepath (tiles--new-note-filepath)))
        (write-region content nil filepath)
        (tiles--archive-note filepath)
        (tiles-capture-mode -1)
        (kill-buffer)
        ;; Refresh dashboard if it exists
        (when (get-buffer tiles--notes-buffer-name)
          (tiles-show-notes))
        (message "Saved: %s" (file-name-nondirectory filepath))))))

(defun tiles-capture-cancel ()
  "Cancel the current TILES capture."
  (interactive)
  (when (yes-or-no-p "Discard this note? ")
    (tiles-capture-mode -1)
    (kill-buffer)
    (message "Note discarded")))

(defvar-local tiles--focus-saved-margins nil
  "Saved window margins before `tiles-focus-mode' was activated.")
(defvar-local tiles--focus-saved-header nil
  "Saved `header-line-format' before `tiles-focus-mode' was activated.")
(defvar-local tiles--focus-header-remap nil
  "Face remap cookie for `header-line' in `tiles-focus-mode'.")
(defvar-local tiles--focus-margin-remap nil
  "Face remap cookie for `margin' in `tiles-focus-mode'.")

(defun tiles--frame-background-color ()
  "Return the effective background color of the selected frame.
Prefers the frame parameter over the `default' face's :background
attribute, since the latter can be the unresolved placeholder
\"unspecified-bg\" when the color comes from frame parameters or X
resources rather than face theming."
  (or (frame-parameter nil 'background-color)
      (face-attribute 'default :background)))

(defun tiles--focus-reapply ()
  "Re-apply focus mode margins to all windows currently displaying this buffer.
Used as a buffer-local `window-configuration-change-hook' to restore margins
after switching away and back to a focus-mode buffer."
  (when (bound-and-true-p tiles-focus-mode)
    (dolist (win (get-buffer-window-list (current-buffer) nil t))
      (let* ((body-width 80)
             (win-width (window-total-width win))
             (margin (max 0 (/ (- win-width body-width) 2))))
        (set-window-margins win margin margin)
        (set-window-fringes win 0 0)))))

(define-minor-mode tiles-focus-mode
  "Minor mode for distraction-free note editing.
Centers the buffer content with approximately 80-character line width
and adds visual breathing room at the top via the header line."
  :lighter " Focus"
  (if tiles-focus-mode
      (let* ((body-width 80)
             (win (selected-window))
             (win-width (window-total-width win))
             (margin (max 0 (/ (- win-width body-width) 2))))
        (setq tiles--focus-saved-margins (window-margins win))
        (set-window-margins win margin margin)
        (set-window-fringes win 0 0)
        ;; The `margin' face controls the empty margin areas used to center
        ;; the text; if a theme gives it a background other than `default'
        ;; (e.g. matching `fringe'), the margins show up as gray bars
        ;; flanking the centered text, so remap it to blend in.
        (setq tiles--focus-margin-remap
              (face-remap-add-relative 'margin
                :background (tiles--frame-background-color)))
        (setq-local word-wrap t)
        (setq-local truncate-lines nil)
        (visual-line-mode 1)
        ;; Add visual breathing room via header-line: it lives above the buffer
        ;; content area so cursor positioning in the buffer is unaffected.
        (setq tiles--focus-saved-header header-line-format)
        (setq tiles--focus-header-remap
              (face-remap-add-relative 'header-line
                :background (tiles--frame-background-color)
                :foreground (face-attribute 'default :foreground)
                :box nil :underline nil :overline nil))
        (setq-local header-line-format
                    (propertize " " 'display '(space :height 2.0)))
        ;; Re-apply margins whenever this buffer appears in a new window
        (add-hook 'window-configuration-change-hook #'tiles--focus-reapply nil t))
    ;; Deactivate: restore margins and header line
    (remove-hook 'window-configuration-change-hook #'tiles--focus-reapply t)
    (when tiles--focus-header-remap
      (face-remap-remove-relative tiles--focus-header-remap)
      (setq tiles--focus-header-remap nil))
    (when tiles--focus-margin-remap
      (face-remap-remove-relative tiles--focus-margin-remap)
      (setq tiles--focus-margin-remap nil))
    (setq-local header-line-format tiles--focus-saved-header)
    (setq tiles--focus-saved-header nil)
    (let ((win (selected-window)))
      (if tiles--focus-saved-margins
          (set-window-margins win (car tiles--focus-saved-margins)
                              (cdr tiles--focus-saved-margins))
        (set-window-margins win 0 0))
      (set-window-fringes win nil nil))
    (visual-line-mode -1)
    (kill-local-variable 'word-wrap)
    (kill-local-variable 'truncate-lines)))

(defun tiles--touch-rename (filepath)
  "Prompt to rename FILEPATH to a fresh current-time timestamp.
On confirmation, performs the rename and evicts the old path from the
cache.  Returns the new filepath, or nil if the user declined."
  (let* ((old-ts (tiles--filename-to-timestamp (file-name-nondirectory filepath)))
         (new-name (tiles--generate-filename))
         (new-filepath (tiles--note-filepath new-name filepath))
         (new-ts (tiles--filename-to-timestamp new-name)))
    (when (yes-or-no-p (format "Update timestamp from %s to %s? " old-ts new-ts))
      (tiles--rename-note filepath new-filepath)
      new-filepath)))

;;;###autoload
(defun tiles-touch ()
  "Update the current note's timestamp to now and rename the file accordingly.
Must be visiting a TILES note file.  Asks for confirmation."
  (interactive)
  (let ((filepath (buffer-file-name)))
    (unless (and filepath (tiles--tile-file-p filepath))
      (user-error "Not visiting a TILES note file"))
    (when (buffer-modified-p)
      (save-buffer))
    (let* ((old-name (file-name-nondirectory filepath))
           (new-filepath (tiles--touch-rename filepath)))
      (when new-filepath
        (set-visited-file-name new-filepath t t)
        ;; Its web archives were renamed with it.
        (tiles--archive-list-show)
        (when (equal tiles--preview-file filepath)
          (setq tiles--preview-file new-filepath))
        (let ((notes-buf (get-buffer tiles--notes-buffer-name)))
          (when notes-buf
            (with-current-buffer notes-buf
              (tiles-show-notes))))
        (message "Renamed %s -> %s"
                 old-name (file-name-nondirectory new-filepath))))))

(defun tiles--quick-save (content tags)
  "Save a note with CONTENT and TAGS (slash-separated string)."
  (tiles--ensure-directory)
  (let ((filepath (tiles--new-note-filepath)))
    (write-region (concat content "\n\n" tags "\n") nil filepath)
    (tiles--archive-note filepath)
    (when (get-buffer tiles--notes-buffer-name)
      (tiles-show-notes))
    (message "Saved: %s" (file-name-nondirectory filepath))))

;;;###autoload
(defun tiles-quick ()
  "Quick capture a note via the minibuffer.
Prompts for content, then tags per `tiles-tag-mode'."
  (interactive)
  (let* ((content (read-string "Note: "))
         (tags (tiles--prompt-for-tags)))
    (when (string-empty-p (string-trim content))
      (user-error "Note content cannot be empty"))
    (when (and (not (tiles--tags-inhibited-p))
               (or (null tags) (string-empty-p (string-trim tags))))
      (user-error "Tags cannot be empty"))
    (tiles--quick-save content (or tags "-"))))

;;;###autoload
(defun tiles-yank ()
  "Quick capture a note with content from clipboard.
Pre-fills content from the kill ring, then prompts for tags."
  (interactive)
  (let* ((clip (or (current-kill 0 t) ""))
         (content (read-string "Note: " clip))
         (tags (tiles--prompt-for-tags)))
    (when (string-empty-p (string-trim content))
      (user-error "Note content cannot be empty"))
    (when (and (not (tiles--tags-inhibited-p))
               (or (null tags) (string-empty-p (string-trim tags))))
      (user-error "Tags cannot be empty"))
    (tiles--quick-save content (or tags "-"))))

;;; Search Functions

;;;###autoload
(defun tiles-tag-search (query)
  "Search TILES notes by tags.
QUERY uses / for AND and space for OR.
\"b218/lx2026\" matches notes with both tags.
\"b218 misc\" matches notes with either tag.
\"b218/lx2026 misc\" matches (b218 AND lx2026) OR misc."
  (interactive (list (cond
                       ((tiles--tags-inhibited-p)
                        (user-error "Tag search is disabled (tiles-tag-mode is inhibit)"))
                       ((tiles--tags-restricted-p)
                        (completing-read "Search tag: " tiles-tag-mode nil t))
                       ((tiles--tags-required-one-of-p)
                        (completing-read
                         (format "Search tags (required: %s): "
                                 (tiles--tags-required-description))
                         (tiles--tags-required-candidates) nil nil))
                       (t (read-string "Search tags: ")))))
  (let* ((query-tags (tiles--parse-tag-query query))
         (results (tiles--sort-newest-first
                   (tiles--files-matching-tag-query query-tags))))
    (setq tiles--current-search-results results)
    (setq tiles--current-search-query query)
    (setq tiles--current-search-type 'tag)
    (if results
        (tiles--show-search-view results)
      (message "No matching notes found for tags: %s" query))))

;;;###autoload
(defun tiles-keyword-search (query)
  "Search TILES notes by bold keywords.
QUERY uses space for OR, `/' for AND, and a `!' prefix for NOT.
\"foo bar\" matches notes containing either keyword.
\"foo/bar\" matches notes containing both.
\"foo/!bar\" matches notes containing foo but not bar.
\"foo/bar baz\" = (foo AND bar) OR baz."
  (interactive "sSearch keywords (space=OR, /=AND, !=NOT): ")
  (let* ((query-and-groups (tiles--parse-keyword-query query))
         (results (tiles--sort-newest-first
                   (tiles--files-matching-keyword-query query-and-groups))))
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
    (define-key map (kbd "R") #'tiles-list-rename)
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
    (insert (propertize (format "%d unique %s  [%s]  (RET:search  R:rename  o:sort occurrence  a:sort alpha  d:toggle direction  q:quit)\n\n"
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

(defun tiles--rename-keyword-in-file (file old-keyword new-keyword)
  "Replace bold OLD-KEYWORD with NEW-KEYWORD in FILE.
Matching is done after normalizing hyphens to spaces, so `*Falcon-9*'
matches the keyword \"Falcon 9\".  Returns non-nil if the file changed."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((modified nil))
      (goto-char (point-min))
      (while (re-search-forward "\\*\\([^*\n]+\\)\\*" nil t)
        (when (string= (tiles--normalize-keyword (match-string 1))
                       old-keyword)
          (replace-match (format "*%s*" new-keyword) t t)
          (setq modified t)))
      (when modified
        (write-region (point-min) (point-max) file nil 'silent)
        t))))

(defun tiles--rename-tag-in-file (file old-tag new-tag)
  "Replace OLD-TAG with NEW-TAG on the tag line of FILE.
The tag line is the last non-empty line of the file.  Returns non-nil
if the file changed."
  (with-temp-buffer
    (insert-file-contents file)
    (let* ((lines (split-string (buffer-string) "\n"))
           (i (1- (length lines))))
      (while (and (>= i 0) (string-empty-p (string-trim (nth i lines))))
        (setq i (1- i)))
      (when (>= i 0)
        (let* ((tag-line (string-trim (nth i lines)))
               (tags (split-string tag-line "/" t "[ \t]+"))
               (renamed (mapcar (lambda (tag)
                                  (if (string= tag old-tag) new-tag tag))
                                tags)))
          (when (not (equal tags renamed))
            (setf (nth i lines) (mapconcat #'identity renamed "/"))
            (erase-buffer)
            (insert (mapconcat #'identity lines "\n"))
            (write-region (point-min) (point-max) file nil 'silent)
            t))))))

(defun tiles-list-rename ()
  "Rename the tag or keyword on the current line across all note files.
For keywords, replaces every `*old*' bold occurrence with `*new*'.
For tags, replaces the tag in the tag line of every file that has it.
Prompts for the new name and reports how many files were modified."
  (interactive)
  (let* ((type tiles--list-type)
         (label (if (eq type 'tag) "tag" "keyword"))
         (raw (string-trim (buffer-substring-no-properties
                            (line-beginning-position) (line-end-position))))
         (item (if (string-match "^\\[[0-9]+\\] \\(.*\\)" raw)
                   (match-string 1 raw)
                 raw)))
    (when (or (not item) (string-empty-p item))
      (user-error "No %s on this line" label))
    (when (and (eq type 'tag) (tiles--tags-inhibited-p))
      (user-error "Tag rename is disabled (tiles-tag-mode is inhibit)"))
    (let ((new-name (string-trim
                     (read-string (format "Rename %s \"%s\" to: " label item)
                                  item))))
      (when (string-empty-p new-name)
        (user-error "New %s name cannot be empty" label))
      (when (if (eq type 'tag)
                (string= new-name item)
              (string= (tiles--normalize-keyword new-name)
                       (tiles--normalize-keyword item)))
        (user-error "New name is the same as the old name"))
      (when (eq type 'tag)
        (cond
         ((tiles--tags-restricted-p)
          (unless (member new-name tiles-tag-mode)
            (user-error "New tag not in allowed list: %s"
                        (mapconcat #'identity tiles-tag-mode ", "))))
         ((tiles--tags-required-one-of-p)
          (when (and (tiles--tag-required-p item)
                     (not (tiles--tag-required-p new-name)))
            (unless (yes-or-no-p
                     (format "\"%s\" is a required tag and \"%s\" is not.  Continue? "
                             item new-name))
              (user-error "Aborted"))))))
      (tiles--ensure-index)
      (let* ((index (if (eq type 'tag) tiles--tag-index tiles--keyword-index))
             (files (copy-sequence (gethash item index)))
             (rename-fn (if (eq type 'tag)
                            #'tiles--rename-tag-in-file
                          #'tiles--rename-keyword-in-file))
             (count 0))
        (dolist (file files)
          (when (funcall rename-fn file item new-name)
            (remhash file tiles--cache)
            (tiles--parse-note-file file)
            (setq count (1+ count))))
        (message "Renamed %s \"%s\" to \"%s\" in %d file%s"
                 label item new-name count (if (= count 1) "" "s"))
        (let ((data (tiles--list-collect-data)))
          (setq-local tiles--list-items
                      (if (eq type 'tag) (car data) (cdr data)))
          (setq-local tiles--list-cross
                      (if (eq type 'tag) (cdr data) (car data)))
          (tiles--list-render))))))

(defun tiles--list-collect-data ()
  "Collect tag and keyword counts from all notes.
Returns (tags-hash . keywords-hash) where values are counts."
  (tiles--ensure-index)
  (let ((all-tags (make-hash-table :test 'equal))
        (all-keywords (make-hash-table :test 'equal)))
    (maphash (lambda (tag files)
               (puthash tag (length files) all-tags))
             tiles--tag-index)
    (maphash (lambda (kw files)
               (puthash kw (length files) all-keywords))
             tiles--keyword-index)
    (cons all-tags all-keywords)))

;;;###autoload
(defun tiles-list-tags ()
  "Display all unique tags across all notes in a navigable buffer.
Tags that also appear as keywords are shown in bold.
Press RET to filter the dashboard by the selected tag."
  (interactive)
  (when (tiles--tags-inhibited-p)
    (user-error "Tag listing is disabled (tiles-tag-mode is inhibit)"))
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

(defvar tiles-notes-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'tiles-notes-next)
    (define-key map (kbd "p") #'tiles-notes-prev)
    (define-key map (kbd "<down>") #'tiles-notes-next)
    (define-key map (kbd "<up>") #'tiles-notes-prev)
    (define-key map (kbd "RET") #'tiles-notes-open)
    (define-key map (kbd "<M-return>") #'tiles-notes-open-read-only)
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
    (define-key map (kbd "w") #'tiles-notes-copy)
    (define-key map (kbd "W") #'tiles-notes-copy-stripped)
    (define-key map (kbd "+") #'tiles-notes-load-more)
    (define-key map (kbd "0") #'tiles-notes-stitch)
    (define-key map (kbd "l") #'tiles-new)
    (define-key map (kbd "g") #'tiles-show-notes)
    (define-key map (kbd "G") #'tiles-rescan)
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
  "Strip all `org-mode' markup from TEXT, returning plain text.
Handles inline footnotes, links (with or without description), and the
emphasis markups: bold (*x*), italic (/x/), underline (_x_),
strikethrough (+x+), code (~x~), and verbatim (=x=)."
  (let ((result text))
    (setq result (replace-regexp-in-string "\\[fn:[^]]*\\]" "" result))
    (setq result (replace-regexp-in-string "\\[\\[\\(?:[^][]+\\)\\]\\[\\([^][]+\\)\\]\\]" "\\1" result))
    (setq result (replace-regexp-in-string "\\[\\[[^][]+\\]\\]" "" result))
    (setq result (replace-regexp-in-string "\\*\\([^*\n]+\\)\\*" "\\1" result))
    (setq result (replace-regexp-in-string "/\\([^/\n]+\\)/" "\\1" result))
    (setq result (replace-regexp-in-string "_\\([^_\n]+\\)_" "\\1" result))
    (setq result (replace-regexp-in-string "\\+\\([^+\n]+\\)\\+" "\\1" result))
    (setq result (replace-regexp-in-string "~\\([^~\n]+\\)~" "\\1" result))
    (setq result (replace-regexp-in-string "=\\([^=\n]+\\)=" "\\1" result))
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
         (rendered (tiles--render-org-preview line)))
    ;; By width on screen: CJK characters, for one, take two columns.
    (if (> (string-width rendered) tiles-preview-length)
        (truncate-string-to-width rendered tiles-preview-length nil nil "…")
      rendered)))

(defun tiles--align-to (column &optional file)
  "Return a space that stretches to COLUMN of the window when displayed.
It lines things up exactly, whatever the width of the characters before
it.  FILE, if non-nil, is its `tiles-filepath' property."
  (propertize " " 'display `(space :align-to ,column) 'tiles-filepath file))

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

(defun tiles-notes-filter-tag (query)
  "Filter the dashboard to show only notes matching QUERY tags."
  (interactive (list (cond
                       ((tiles--tags-inhibited-p)
                        (user-error "Tag filtering is disabled (tiles-tag-mode is inhibit)"))
                       ((tiles--tags-restricted-p)
                        (completing-read "Filter by tag: " tiles-tag-mode nil t))
                       ((tiles--tags-required-one-of-p)
                        (completing-read
                         (format "Filter by tag (required: %s): "
                                 (tiles--tags-required-description))
                         (tiles--tags-required-candidates) nil nil))
                       (t (read-string "Filter by tag: ")))))
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
  (interactive (list (cond
                       ((tiles--tags-inhibited-p)
                        (user-error "Tag exclusion is disabled (tiles-tag-mode is inhibit)"))
                       ((tiles--tags-restricted-p)
                        (completing-read "Exclude tag: " tiles-tag-mode nil t))
                       ((tiles--tags-required-one-of-p)
                        (completing-read
                         (format "Exclude tag (required: %s): "
                                 (tiles--tags-required-description))
                         (tiles--tags-required-candidates) nil nil))
                       (t (read-string "Exclude tags (space-separated): ")))))
  (if (string-empty-p (string-trim query))
      (message "No tags specified")
    (setq tiles--notes-page 0)
    (setq tiles--notes-exclude (split-string query " " t))
    (tiles-show-notes)))

(defun tiles-notes-clear-filter ()
  "Clear the dashboard search filter (keep exclusions)."
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
           (new-filepath (tiles--touch-rename filepath)))
      (when new-filepath
        ;; Close preview if it was showing the renamed file
        (when (equal tiles--preview-file filepath)
          (let* ((buf (get-file-buffer filepath))
                 (win (when buf (get-buffer-window buf))))
            (when win (delete-window win))
            (when buf (kill-buffer buf))
            (setq tiles--preview-file nil)))
        (tiles-show-notes)
        (message "Renamed %s -> %s"
                 old-name (file-name-nondirectory new-filepath))))))

(defun tiles--notes-copy-to-kill (strip-markup)
  "Copy the note at point's content to the kill ring.
Private (`&&') paragraphs are always excluded.  When STRIP-MARKUP is
non-nil, also strip all org formatting (bold, italic, underline,
strikethrough, code, verbatim, links, inline footnotes)."
  (let ((filepath (tiles--notes-current-file)))
    (unless filepath
      (user-error "No note on this line"))
    (let* ((note-data (tiles--parse-note-file filepath))
           (raw (when note-data
                  (tiles--strip-private-paragraphs
                   (or (plist-get note-data :content) ""))))
           (content (when raw
                      (string-trim
                       (if strip-markup
                           (tiles--strip-org-markup raw)
                         raw)))))
      (if (and content (not (string-empty-p content)))
          (progn
            (kill-new content)
            (message "Copied %d character%s (%s) to the kill ring"
                     (length content)
                     (if (= (length content) 1) "" "s")
                     (if strip-markup "plain text" "verbatim")))
        (message "Note is empty")))))

(defun tiles-notes-copy ()
  "Copy the note on the current line to the kill ring verbatim.
Private (`&&') paragraphs are stripped, matching what is shown in
previews and stitched views, but `org-mode' markup (bold, italic,
links, etc.) is preserved.  Use `tiles-notes-copy-stripped' (bound to
`W' in the dashboard) for a plain-text copy with markup removed."
  (interactive)
  (tiles--notes-copy-to-kill nil))

(defun tiles-notes-copy-stripped ()
  "Copy the note on the current line to the kill ring as plain text.
Strips both private (`&&') paragraphs and all `org-mode' formatting
\(bold, italic, underline, strikethrough, code, verbatim, links,
inline footnotes).  Bound to `W' in the dashboard; use `w' to copy
verbatim with markup preserved."
  (interactive)
  (tiles--notes-copy-to-kill t))

(defun tiles-notes-load-more ()
  "Load the next batch of notes in the dashboard."
  (interactive)
  (if (not tiles-dashboard-limit)
      (message "All notes already shown")
    (let ((current-file (tiles--notes-current-file)))
      (setq tiles--notes-page (1+ tiles--notes-page))
      (tiles-show-notes)
      (when current-file
        (goto-char (point-min))
        (while (and (not (eobp))
                    (not (equal (get-text-property (point) 'tiles-filepath) current-file)))
          (forward-line 1))))))

(defun tiles-notes-toggle-raw ()
  "Toggle raw (plain text) preview in the dashboard."
  (interactive)
  (setq tiles-preview-raw (not tiles-preview-raw))
  (tiles-show-notes)
  (message "Preview formatting %s" (if tiles-preview-raw "off" "on")))

(defun tiles--apply-exclude (files)
  "Remove from FILES any notes that have excluded tags.
Uses `tiles--notes-exclude' and the reverse index."
  (if (not tiles--notes-exclude)
      files
    (tiles--ensure-index)
    (let ((excluded (make-hash-table :test 'equal)))
      (dolist (etag tiles--notes-exclude)
        (dolist (f (gethash etag tiles--tag-index))
          (puthash f t excluded)))
      (seq-remove (lambda (f) (gethash f excluded)) files))))

(defun tiles--apply-filter (files)
  "Filter FILES according to `tiles--notes-filter' and `tiles--notes-exclude'."
  (let ((result (tiles--apply-exclude files)))
    (if (not tiles--notes-filter)
        result
      (let* ((type (car tiles--notes-filter))
             (query (cdr tiles--notes-filter))
             (matching (if (eq type 'tag)
                           (tiles--files-matching-tag-query
                            (tiles--parse-tag-query query))
                         (tiles--files-matching-keyword-query
                          (tiles--parse-keyword-query query))))
             (match-set (make-hash-table :test 'equal)))
        (dolist (f matching) (puthash f t match-set))
        (seq-filter (lambda (f) (gethash f match-set)) result)))))

;;;###autoload
(defun tiles-show-notes ()
  "Display all notes chronologically, newest first.
Shows a dashboard with statistics and note listing."
  (interactive)
  (when tiles-debug
    (setq tiles--debug-cache-hits 0
          tiles--debug-cache-misses 0))
  (let* ((tiles--debug-t0 (when tiles-debug (float-time)))
         ;; Remember which note is selected when refreshing in place,
         ;; so we can restore the cursor on that line after re-render.
         (prev-selection (when (string= (buffer-name) tiles--notes-buffer-name)
                           (get-text-property (line-beginning-position)
                                              'tiles-filepath)))
         (all-files (tiles--get-all-tile-files))
         (tiles--debug-t1 (when tiles-debug (float-time)))
         (filtered (tiles--apply-filter all-files))
         (tiles--debug-t2 (when tiles-debug (float-time))))
    (if (and (not all-files)
             (not (string= (buffer-name) tiles--notes-buffer-name)))
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
            (setq max-tag-len (max max-tag-len (string-width tag-str)))))
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
                 (keys (concat "  [SPC] view, [RET] open, [M-RET] read-only, [TAB] expand, [f] format, [d] chg date, [u] touch, [w] copy, [0] stitch, [D] delete, [g] refresh\n"
                               (if (tiles--tags-inhibited-p)
                                   "  [k] filter keyword, [K] list keywords, [c] clr search, [l] new tile, [q] quit\n"
                                 "  [t] filter tag, [k] filter keyword, [F] exclude tags, [T] list tags, [K] list keywords, [c] clr search, [C] clr excl, [l] new tile, [q] quit\n")))
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
                    (propertize dash-line 'face 'font-lock-comment-face 'tiles-header t))
            (when tiles-debug
              (message "[TILES debug] %d files total, %d displayed | scan %.1fms | filter %.1fms | render %.1fms | total %.1fms | cache: %d hits, %d misses"
                       num-all (length files)
                       (* 1000.0 (- tiles--debug-t1 tiles--debug-t0))
                       (* 1000.0 (- tiles--debug-t2 tiles--debug-t1))
                       (* 1000.0 load-time)
                       (* 1000.0 (- (float-time) tiles--debug-t0))
                       tiles--debug-cache-hits
                       tiles--debug-cache-misses))))
        (tiles-notes-view-mode)
        ;; Restore cursor to the previously selected note if it is still
        ;; displayed; otherwise jump to the first note line.
        (goto-char (point-min))
        (unless (and prev-selection
                     (let (found)
                       (while (and (not found) (not (eobp)))
                         (if (and (equal (get-text-property (point) 'tiles-filepath)
                                         prev-selection)
                                  (not (get-text-property (point) 'tiles-header)))
                             (setq found t)
                           (forward-line 1)))
                       found))
          (goto-char (point-min))
          (text-property-search-forward 'tiles-filepath))))))

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
    (and (text-property-search-forward 'tiles-expanded) t)))

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
      (goto-char (point-min))
      (tiles--enable-tag-line-fontification)
      (when tiles-focus-default
        (tiles-focus-mode 1)))))

(defun tiles-notes-open-read-only ()
  "Open the note on the current line in a read-only buffer.
Activates `tiles-readonly-mode' so `m' shows similar notes."
  (interactive)
  (let ((filepath (tiles--notes-current-file)))
    (when filepath
      (find-file-read-only filepath)
      (goto-char (point-min))
      (tiles--enable-tag-line-fontification)
      (tiles-readonly-mode 1)
      (when tiles-focus-default
        (tiles-focus-mode 1)))))

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
  "Return up to three extra lines for NOTE-DATA.
The lines are: private (`&&') paragraphs when present, keywords, and stats
\(char/word counts and on-disk size).  FILE is the path to the note file,
used to compute filesize."
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
               (priv-str (truncate-string-to-width priv-str max-priv-len nil nil "…"))
               (padded-priv (concat indent priv-str
                                    (tiles--align-to tiles--line-target-width))))
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
           (kw-str (truncate-string-to-width kw-str max-kw-len nil nil "…"))
           (padded-kw (concat indent kw-str (tiles--align-to tiles--line-target-width))))
      (setq result (concat result
                           (propertize padded-kw
                                       'face (list 'tiles-tags 'tiles-notes-expanded)
                                       'tiles-filepath file
                                       'tiles-expanded t)
                           "\n")))
    ;; Stats line
    (let* ((stats (format "%d chars, %d words, %s" char-count word-count filesize))
           (padded-stats (concat indent stats (tiles--align-to tiles--line-target-width))))
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
  "Stitch the currently displayed notes into a flowing view.
Respects manual display order set by M-up/M-down."
  (interactive)
  (let ((files (tiles--notes-displayed-files)))
    (if (not files)
        (message "No notes to stitch")
      (if (or tiles--notes-filter
              (yes-or-no-p "You should perform a search before stitching notes.  Are you sure you want to stitch ALL the notes? "))
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
             (new-filepath (tiles--note-filepath new-fname filepath)))
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
          (tiles--rename-note filepath new-filepath)
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
        (tiles--evict-file filepath)
        (tiles-show-notes)))))

(defun tiles--render-note-line (file)
  "Return a propertized string for FILE's line in the notes viewer."
  (let* ((fname (file-name-nondirectory file))
         (timestamp (tiles--filename-to-timestamp fname))
         (ts-face (tiles--timestamp-face fname))
         (note-data (tiles--parse-note-file file))
         (preview (if note-data (tiles--note-oneline-preview note-data) ""))
         (tags (when note-data (plist-get note-data :tags)))
         (tag-str (if (and tags (not (tiles--tags-inhibited-p)))
                      (concat "  " (mapconcat #'identity tags "/"))
                    ""))
         (has-private (and (not tiles-preview-raw)
                           note-data
                           (let ((content (plist-get note-data :content)))
                             (seq-some #'tiles--private-paragraph-p
                                       (split-string (or content "") "\n\n+" t)))))
         (separator (if has-private
                        (concat " " (propertize "&" 'face 'tiles-tags))
                      "  ")))
    ;; The tags and the end of the line are aligned on the display, not
    ;; by counting characters, which wide ones (CJK) would throw off.
    (concat (propertize "  " 'tiles-filepath file)
            (propertize timestamp 'face ts-face 'tiles-filepath file)
            (propertize separator 'tiles-filepath file)
            (propertize preview 'tiles-filepath file)
            (tiles--align-to (+ 2 (string-width timestamp) 2 tiles-preview-length) file)
            (propertize tag-str 'face 'tiles-tags 'tiles-filepath file)
            (tiles--align-to tiles--line-target-width file))))

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
                (end-of-line))
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

;;; Similar Notes

(defcustom tiles-similar-count 5
  "Number of similar notes shown by `tiles-show-similar'."
  :type 'integer
  :group 'tiles)

(defcustom tiles-similar-keyword-weight 3.0
  "Score multiplier for shared tokens that are also `*bold*' in the target."
  :type 'number
  :group 'tiles)

(defcustom tiles-similar-min-token-length 3
  "Minimum length of a content token considered for similarity scoring."
  :type 'integer
  :group 'tiles)

(defvar-local tiles--similar-region-start nil
  "Start marker of the inserted similar-notes block, or nil if hidden.")

(defvar-local tiles--similar-region-end nil
  "End marker of the inserted similar-notes block, or nil if hidden.")

(defun tiles--tokenize-content (content)
  "Return a deduplicated list of lowercase tokens from CONTENT.
Strips org markup, splits on non-alphanumerics, and drops tokens
shorter than `tiles-similar-min-token-length'."
  (let ((text (downcase (tiles--strip-org-markup (or content ""))))
        tokens)
    (dolist (w (split-string text "[^[:alnum:]]+" t))
      (when (>= (length w) tiles-similar-min-token-length)
        (push w tokens)))
    (delete-dups tokens)))

(defun tiles--file-content-tokens (filepath)
  "Return content tokens for FILEPATH, cached by mtime."
  (let* ((mtime (file-attribute-modification-time
                 (file-attributes filepath)))
         (cached (gethash filepath tiles--content-token-cache)))
    (if (and cached (equal (car cached) mtime))
        (cdr cached)
      (let* ((data (tiles--parse-note-file filepath))
             (content (tiles--strip-private-paragraphs
                       (or (plist-get data :content) "")))
             (tokens (tiles--tokenize-content content)))
        (puthash filepath (cons mtime tokens) tiles--content-token-cache)
        tokens))))

(defun tiles--similar-notes (filepath)
  "Return up to `tiles-similar-count' files most similar to FILEPATH.
Scoring is TF-IDF over content tokens; tokens that are bold keywords
in the target get a `tiles-similar-keyword-weight' boost.
Each element is (SCORE . OTHER-FILEPATH), highest score first."
  (tiles--ensure-index)
  (let* ((target-tokens (tiles--file-content-tokens filepath))
         (target-kw-set (make-hash-table :test 'equal))
         (df (make-hash-table :test 'equal))
         (n (max 1 (hash-table-count tiles--file-index-record)))
         scored)
    (dolist (kw (plist-get (tiles--parse-note-file filepath) :keywords))
      (dolist (w (split-string (downcase kw) "[^[:alnum:]]+" t))
        (when (>= (length w) tiles-similar-min-token-length)
          (puthash w t target-kw-set))))
    (maphash
     (lambda (f _)
       (dolist (tok (tiles--file-content-tokens f))
         (when (member tok target-tokens)
           (puthash tok (1+ (gethash tok df 0)) df))))
     tiles--file-index-record)
    (maphash
     (lambda (f _)
       (unless (equal f filepath)
         (let ((score 0.0))
           (dolist (tok (tiles--file-content-tokens f))
             (when (member tok target-tokens)
               (let* ((d (gethash tok df 0))
                      (idf (log (/ (+ n 1.0) (+ d 1.0))))
                      (boost (if (gethash tok target-kw-set)
                                 tiles-similar-keyword-weight
                               1.0)))
                 (setq score (+ score (* boost idf))))))
           (when (> score 0)
             (push (cons score f) scored)))))
     tiles--file-index-record)
    (setq scored (sort scored (lambda (a b) (> (car a) (car b)))))
    (seq-take scored tiles-similar-count)))

(defun tiles--similar-clear ()
  "Remove the inserted similar-notes block from the current buffer."
  (when (and tiles--similar-region-start tiles--similar-region-end)
    (let ((inhibit-read-only t)
          (was-modified (buffer-modified-p)))
      (delete-region tiles--similar-region-start tiles--similar-region-end)
      (set-marker tiles--similar-region-start nil)
      (set-marker tiles--similar-region-end nil)
      (setq tiles--similar-region-start nil
            tiles--similar-region-end nil)
      (set-buffer-modified-p was-modified)
      (when font-lock-mode (font-lock-flush)))))

(defun tiles--similar-insert (filepath)
  "Append the top similar notes for FILEPATH to the current buffer.
Returns the number of notes inserted, or nil when none were found."
  (let ((matches (tiles--similar-notes filepath)))
    (when matches
      (let ((was-modified (buffer-modified-p))
            (inhibit-read-only t))
        (save-excursion
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (insert "\n")
          (setq tiles--similar-region-start (point-marker))
          (set-marker-insertion-type tiles--similar-region-start nil)
          (dolist (m matches)
            (let* ((f (cdr m))
                   (data (tiles--parse-note-file f))
                   (content (string-trim
                             (or (tiles--strip-private-paragraphs
                                  (plist-get data :content))
                                 ""))))
              (insert content "\n\n")))
          (setq tiles--similar-region-end (point-marker))
          (set-marker-insertion-type tiles--similar-region-end t))
        (set-buffer-modified-p was-modified)
        (when font-lock-mode (font-lock-flush))
        (length matches)))))

(defun tiles-show-similar ()
  "Toggle a block of notes similar to the current tile at end of buffer.
Scores notes by TF-IDF over shared content tokens; tokens that are
`*bold*' in the current note get a `tiles-similar-keyword-weight'
boost.  Press \\[tiles-show-similar] again to remove the block."
  (interactive)
  (let ((filepath (buffer-file-name)))
    (unless (and filepath (tiles--tile-file-p filepath))
      (user-error "Not visiting a tile file"))
    (if (and tiles--similar-region-start tiles--similar-region-end)
        (progn (tiles--similar-clear)
               (message "Similar notes hidden."))
      (let ((n (tiles--similar-insert filepath)))
        (if n
            (message "Showing %d similar %s."
                     n (if (= 1 n) "note" "notes"))
          (message "No similar notes found."))))))

(defun tiles-readonly-quit ()
  "Bury the current read-only tile buffer and delete its window."
  (interactive)
  (quit-window t))

(defvar tiles-readonly-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "m") #'tiles-show-similar)
    (define-key map (kbd "q") #'tiles-readonly-quit)
    map)
  "Keymap for `tiles-readonly-mode'.")

(define-minor-mode tiles-readonly-mode
  "Minor mode for read-only TILES note buffers.
Press \\<tiles-readonly-mode-map>\\[tiles-show-similar] to list notes similar to the current tile."
  :lighter " Tile-RO"
  :keymap tiles-readonly-mode-map)

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
    ;; Boundaries are sorted ascending; stop at the first one >= pos.
    (catch 'done
      (dolist (boundary tiles--stitched-boundaries)
        (if (< (car boundary) pos)
            (setq prev-pos (car boundary))
          (throw 'done nil))))
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
          (goto-char (point-min))
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
                         note-data (tiles--parse-keyword-query keywords))))))
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

(defun tiles--launch-id-from-content (content)
  "Return the COSPAR launch ID if CONTENT starts with *YYYY-ID*, else nil.
Matches patterns like *2026-146* or *2026-F01* at the start of the string."
  (when (string-match "\\`\\*\\([0-9]\\{4\\}-[A-Za-z0-9]+\\)\\*" content)
    (match-string 1 content)))

(defun org-dblock-write:tiles-files (params)
  "Dynamic block: embed the contents of matching TILES notes.
PARAMS supports :tags, :keywords, :sort (\"newest\"/\"oldest\"), :limit,
:separator (string between notes, default blank line), and :link-from-btag.
When `tiles-dblock-html-anchor' is non-nil, each note is preceded by an
`@@html:<a id=\"...\">@@' Org export snippet, unless the note has a tag
listed in `tiles-dblock-no-anchor-tags'.
When :link-from-btag is non-nil, the anchor href is resolved as follows:
- If the note content starts with *YYYY-ID* (a COSPAR launch ID), the href
  is \"l/YYYY-ID.html\" pointing to the individual launch page.
- Otherwise the href is built from the note's bulletin tag (a tag matching
  \"^b[0-9]+$\", e.g. \"b227\"), producing \"bul/227.html#ANCHOR\".
- Falls back to the self-link \"#ANCHOR\" when no btag is found."
  (let ((files (tiles--dblock-get-files params))
        (separator (or (plist-get params :separator) "\n"))
        (link-from-btag (plist-get params :link-from-btag)))
    (if (not files)
        (insert "N/A\n")
      (let ((first t))
        (dolist (file files)
          (let ((note-data (tiles--parse-note-file file)))
            (when note-data
              (if first
                  (setq first nil)
                (insert separator))
              (let ((anchor (file-name-sans-extension
                             (file-name-nondirectory file)))
                    (tags (plist-get note-data :tags))
                    ;; What is published: `&&' paragraphs left out.
                    (content (tiles--strip-private-paragraphs
                              (plist-get note-data :content))))
                (when (and tiles-dblock-html-anchor
                           (not (seq-some (lambda (tag)
                                            (member tag tiles-dblock-no-anchor-tags))
                                          tags)))
                  (let* ((href (if link-from-btag
                                   (let ((launch-id (tiles--launch-id-from-content content)))
                                     (if launch-id
                                         (format "l/%s.html" launch-id)
                                       (let* ((btag (seq-find
                                                      (lambda (tag)
                                                        (string-match-p "^b[0-9]+$" tag))
                                                      tags))
                                              (bnum (when btag (substring btag 1))))
                                         (if bnum
                                             (format "bul/%s.html#%s" bnum anchor)
                                           (format "#%s" anchor)))))
                                 (format "#%s" anchor))))
                    (insert (format "@@html:<a id=\"%s\" href=\"%s\" class=\"paragraph-link\">&rarr;</a>@@\n"
                                    anchor href))))
                (insert content)
                (insert "\n")))))))))

;; Register dblocks for C-c C-x x insertion menu
(when (boundp 'org-dynamic-block-alist)
  (add-to-list 'org-dynamic-block-alist
               '("tiles-notes" . org-dblock-write:tiles-notes))
  (add-to-list 'org-dynamic-block-alist
               '("tiles-files" . org-dblock-write:tiles-files)))

;;; Web archives (WARC)

;; Reading, showing and writing WARC (Web ARChive) files, used by link
;; archiving (`tiles-archive-urls') and the archive list below a note.
;; This was warcel, a separate package, until it was folded into TILES:
;; `tiles-warc-' is its prefix, and the User-Agent and warcinfo records
;; it writes still say warcel, as tilesnet's do.
;;
;; `tiles-warc-open' lists the records of an archive and shows archived
;; pages either as Org documents or rendered by EWW.  Scripts, styles,
;; images, video, audio, frames and form controls are removed before a
;; page is shown, so viewing never runs code or fetches anything from
;; the network.  Links to pages that are in the archive open the
;; archived copy; other links are handled according to
;; `tiles-warc-follow-live-links'.  `tiles-warc-capture' saves a web
;; page in a new archive, and `tiles-warc-capture-in-background' does
;; it without waiting, as link archiving does.
;;
;; Everything is done by Emacs itself: plain and gzip-compressed WARC
;; files are read with the built-in zlib support, HTML is parsed with
;; the built-in libxml2 support, and archives are compressed by a small
;; DEFLATE compressor below.  EWW is only loaded for the EWW view.

(declare-function dired-find-file "dired" ())
(declare-function dired-get-file-for-visit "dired" ())
(declare-function dired-get-filename "dired" (&optional localp no-error-if-not-filep))
(declare-function org-link-make-string "ol" (link &optional description))
(declare-function org-open-at-point "org" (&optional arg))
(declare-function org-table-align "org-table" ())
(declare-function org-table-map-tables "org-table" (f &optional quietly))
;; EWW is loaded only for the EWW view (`tiles--warc-require-eww').
(declare-function eww-mode "eww" ())
(declare-function eww-save-history "eww" ())
(declare-function eww--before-browse "eww" ())
(declare-function eww-display-document "eww" (document &optional point buffer))
(declare-function eww-update-header-line-format "eww" ())
(declare-function eww-browse-with-external-browser "eww" (&optional url))
(declare-function shr-insert-document "shr" (dom))
(declare-function shr-parse-base "shr" (url))

(defconst tiles--warc-version "0.1.0"
  "Version of the WARC code, written in the archives it makes.
It is warcel's version: archives say they were made by warcel.")

(defvar desktop-save-buffer)
(defvar dired-mode-map)
(defvar eww-data)
(defvar eww-history-position)
(defvar jka-compr-really-do-compress)
(defvar org-startup-folded)
(defvar org-startup-with-inline-images)
(defvar org-startup-with-latex-preview)
(defvar shr-base)
(defvar shr-inhibit-images)
(defvar shr-target-id)

;;;; Customization

(defgroup tiles-warc nil
  "Web archives of TILES: viewing and capturing WARC files."
  :group 'tiles
  :prefix "tiles-warc-")

(defcustom tiles-warc-default-view 'org
  "How \\<tiles-warc-mode-map>\\[tiles-warc-open-record] shows an archived HTML page."
  :type '(choice (const :tag "Org document" org)
                 (const :tag "EWW" eww))
  :group 'tiles-warc)

(defcustom tiles-warc-readable t
  "Non-nil means the Org view shows only the title and main content of pages.
The menus, headers, footers, sidebars, sharing buttons, lists of
related pages and comments around an article are then left out.
\\<tiles-warc-view-mode-map>\\[tiles-warc-toggle-readable] switches a page between that and the whole page."
  :type 'boolean
  :group 'tiles-warc)

(defcustom tiles-warc-record-types '("response" "resource" "revisit" "conversion")
  "WARC record types listed by default.
\\<tiles-warc-mode-map>\\[tiles-warc-toggle-all-records] toggles between these and all records."
  :type '(repeat string)
  :group 'tiles-warc)

(defcustom tiles-warc-follow-live-links 'ask
  "What to do when following a link to a page that is not in the archive.
When `ask', ask before opening the live page with `browse-url'.
When t, open it without asking.  When nil, only report it."
  :type '(choice (const :tag "Ask first" ask)
                 (const :tag "Open with `browse-url'" t)
                 (const :tag "Do not open" nil))
  :group 'tiles-warc)

(defcustom tiles-warc-capture-directory nil
  "Directory where \\[tiles-warc-capture] saves new archives.
When nil, save them in the `default-directory' of the current buffer."
  :type '(choice (const :tag "Current directory" nil)
                 directory)
  :group 'tiles-warc)

(defcustom tiles-warc-capture-compress t
  "Non-nil means \\[tiles-warc-capture] saves new archives compressed.
They are then .warc.gz files, with each record compressed on its own
as other tools do.  Otherwise they are .warc files.  It applies to
link archiving (`tiles-archive-urls') too."
  :type 'boolean
  :group 'tiles-warc)

(defcustom tiles-warc-capture-timeout 30
  "Seconds to wait for a server that stopped sending data."
  :type 'natnum
  :group 'tiles-warc)

(defcustom tiles-warc-capture-user-agent
  (format "Mozilla/5.0 (compatible; warcel/%s; GNU Emacs %s)"
          tiles--warc-version emacs-version)
  "User-Agent header sent when capturing pages."
  :type 'string
  :group 'tiles-warc)

;;;; Reading bytes

(defconst tiles--warc-chunk-size (* 4 1024 1024)
  "Number of bytes read from a WARC file at a time.")

(defconst tiles--warc-head-size 65536
  "Number of bytes examined to parse the head of a record.")

(defconst tiles--warc-gzip-peek-size 16384
  "Number of compressed bytes inflated to read the head of a gzip member.")

(cl-defstruct (tiles--warc-reader (:constructor tiles--warc-reader-create)
                              (:copier nil))
  "Random access to bytes of a file, or of a whole buffer.
BUFFER holds the bytes from offset BEG up to offset END."
  file size buffer (beg 0) (end 0))

(defvar tiles--warc-scratch nil
  "Unibyte buffer used to inflate the heads of gzip members.")

(defvar tiles--warc-progress nil
  "Progress reporter of the WARC file being read.")

(defun tiles--warc-unibyte-buffer (name)
  "Return a new unibyte buffer named after NAME."
  (let ((buffer (generate-new-buffer name t)))
    (with-current-buffer buffer
      (set-buffer-multibyte nil)
      (setq buffer-undo-list t))
    buffer))

(defun tiles--warc-file-reader (file)
  "Return a reader over the bytes of FILE."
  (tiles--warc-reader-create :file file
                         :size (file-attribute-size (file-attributes file))
                         :buffer (tiles--warc-unibyte-buffer " *tiles-warc-reader*")))

(defun tiles--warc-buffer-reader (buffer)
  "Return a reader over the contents of the unibyte BUFFER."
  (let ((size (buffer-size buffer)))
    (tiles--warc-reader-create :buffer buffer :size size :end size)))

(defun tiles--warc-reader-at (reader offset length)
  "Make READER hold the LENGTH bytes starting at OFFSET.
Fewer bytes are held near the end of the data.  Return the position
of OFFSET in the reader's buffer."
  (let ((want (min (tiles--warc-reader-size reader) (+ offset length))))
    (unless (and (<= (tiles--warc-reader-beg reader) offset)
                 (<= want (tiles--warc-reader-end reader)))
      (let ((end (min (tiles--warc-reader-size reader)
                      (+ offset (max length tiles--warc-chunk-size)))))
        (with-current-buffer (tiles--warc-reader-buffer reader)
          (erase-buffer)
          (insert-file-contents-literally (tiles--warc-reader-file reader)
                                          nil offset end))
        (setf (tiles--warc-reader-beg reader) offset
              (tiles--warc-reader-end reader) end)))
    (- offset (tiles--warc-reader-beg reader) -1)))

(defun tiles--warc-reader-u32 (reader offset)
  "Return the little-endian 32-bit number at OFFSET in READER, or nil."
  (when (and (>= offset 0) (<= (+ offset 4) (tiles--warc-reader-size reader)))
    (let ((pos (tiles--warc-reader-at reader offset 4)))
      (with-current-buffer (tiles--warc-reader-buffer reader)
        (logior (char-after pos)
                (ash (char-after (+ pos 1)) 8)
                (ash (char-after (+ pos 2)) 16)
                (ash (char-after (+ pos 3)) 24))))))

(defun tiles--warc-progress (offset)
  "Report that reading the WARC file reached OFFSET."
  (when tiles--warc-progress
    (progress-reporter-update tiles--warc-progress offset)))

;;;; Gzip members

;; A compressed WARC file is a series of gzip members, usually one per
;; record.  `zlib-decompress-region' stops at the end of the first
;; member without telling where it ended, so members are found by
;; looking for gzip headers that are followed by valid gzip data.

(defun tiles--warc-gzip-candidate (reader from)
  "Return the offset of the first plausible gzip header at or after FROM.
READER holds the compressed data."
  (let ((size (tiles--warc-reader-size reader))
        (magic (unibyte-string #x1f #x8b #x08))
        (case-fold-search nil)
        found)
    (while (and (not found) (<= (+ from 10) size))
      (let* ((pos (tiles--warc-reader-at reader from 10))
             (base (- from pos))
             (end (tiles--warc-reader-end reader)))
        (with-current-buffer (tiles--warc-reader-buffer reader)
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

(defun tiles--warc-warc-start (data)
  "Return where the WARC record at the start of DATA begins, or nil.
Newlines before the record are skipped."
  (let ((case-fold-search nil))
    (and (string-match "\\`[\r\n]*WARC/" data)
         (- (match-end 0) 5))))

(defun tiles--warc-gunzip-peek (reader offset)
  "Inflate the start of the gzip member at OFFSET in READER.
Return (DATA . STATUS).  STATUS is t if the whole member was
inflated, 0 if the member goes on after DATA, and nil if OFFSET
does not start a gzip member."
  (let* ((pos (tiles--warc-reader-at reader offset tiles--warc-gzip-peek-size))
         (source (tiles--warc-reader-buffer reader))
         (end (min (+ pos tiles--warc-gzip-peek-size)
                   (with-current-buffer source (point-max))))
         (full (= (- end pos) tiles--warc-gzip-peek-size)))
    (unless (buffer-live-p tiles--warc-scratch)
      (setq tiles--warc-scratch (tiles--warc-unibyte-buffer " *tiles-warc-scratch*")))
    (with-current-buffer tiles--warc-scratch
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
               ((and (eql status 0) (or full (tiles--warc-warc-start data))) 0)))))))

(defun tiles--warc-inflate-member (file beg end buffer)
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

(defun tiles--warc-gzip-member-end (reader offset expected)
  "Find where the gzip member starting at OFFSET in READER ends.
EXPECTED is the inflated size of the member if it holds just the
record at its start, or nil.  Return a list (END NEXT ALIGNED DATA).
END is the offset where the member ends and NEXT the inflated start
of the member there, nil at the end of the file.  ALIGNED is non-nil
if the inflated size of the member is EXPECTED.  DATA is nil or a
buffer holding the inflated member, for the caller to kill."
  (let ((size (tiles--warc-reader-size reader))
        (expected (and expected (logand expected #xffffffff)))
        (from (+ offset 20))
        result)
    (while (not result)
      (let ((candidate (tiles--warc-gzip-candidate reader from)))
        (if (not candidate)
            (setq result (list size nil
                               (and expected
                                    (eql expected
                                         (tiles--warc-reader-u32 reader (- size 4))))
                               nil))
          (let ((peek (tiles--warc-gunzip-peek reader candidate))
                ;; A gzip member ends with the size of its inflated data.
                (isize (tiles--warc-reader-u32 reader (- candidate 4))))
            (cond
             ((not (cdr peek)))
             ((and expected (eql isize expected))
              (setq result (list candidate (car peek) t nil)))
             (t
              ;; Compressed files stored in the archive look like
              ;; members too: only inflating up to the candidate tells
              ;; whether the member really ends there.
              (let ((data (tiles--warc-unibyte-buffer " *tiles-warc-member*")))
                (if (and (tiles--warc-inflate-member (tiles--warc-reader-file reader)
                                                 offset candidate data)
                         (eql isize (logand (buffer-size data) #xffffffff)))
                    (setq result (list candidate (car peek) nil data))
                  (kill-buffer data)))))
            (setq from (1+ candidate))))))
    result))

(defun tiles--warc-append-member (buffer file beg end data)
  "Append the inflated gzip member of FILE between BEG and END to BUFFER.
DATA is nil or a buffer already holding the inflated member; it is killed."
  (if (not data)
      (tiles--warc-inflate-member file beg end buffer)
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert-buffer-substring data))
    (kill-buffer data)))

;;;; Records

(cl-defstruct (tiles-warc-record (:constructor tiles-warc-record--create)
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

(cl-defstruct (tiles-warc-archive (:constructor tiles-warc-archive--create)
                              (:copier nil))
  "The records of a WARC file and indexes over them."
  file records by-uri by-id by-digest)

(defun tiles--warc-field (fields name)
  "Return the value of the header field NAME in the alist FIELDS."
  (cdr (assoc-string name fields t)))

(defun tiles--warc-parse-fields (lines)
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

(defun tiles--warc-http-block-p (headers)
  "Return non-nil if a record with WARC HEADERS holds an HTTP response."
  (and (member (downcase (or (tiles--warc-field headers "WARC-Type") ""))
               '("response" "revisit"))
       (string-prefix-p "application/http"
                        (downcase (or (tiles--warc-field headers "Content-Type")
                                      "")))))

(defun tiles--warc-parse-head (pos limit)
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
             (headers (tiles--warc-parse-fields
                       (split-string (buffer-substring-no-properties
                                      fields-beg (match-beginning 0))
                                     "\r?\n" t)))
             (block (point))
             (length (max 0 (string-to-number
                             (or (tiles--warc-field headers "Content-Length") "0"))))
             (head (list :headers headers :header-len (- block pos)
                         :length length)))
        (when (and (tiles--warc-http-block-p headers)
                   (looking-at "HTTP/[0-9.]+[ \t]+\\([0-9][0-9][0-9]\\)"))
          (let ((status (string-to-number (match-string 1)))
                (status-end (line-beginning-position 2)))
            (setq head (plist-put head :http-status status))
            (when (re-search-forward "\n\r?\n" (min limit (+ block length)) t)
              (setq head (plist-put head :http-header-len (- (point) block)))
              (setq head (plist-put
                          head :http-headers
                          (tiles--warc-parse-fields
                           (split-string (buffer-substring-no-properties
                                          (min status-end (match-beginning 0))
                                          (match-beginning 0))
                                         "\r?\n" t)))))))
        head))))

(defun tiles--warc-make-record (head member offset)
  "Return a record described by HEAD, starting at OFFSET of MEMBER."
  (tiles-warc-record--create :member member
                         :offset offset
                         :header-len (plist-get head :header-len)
                         :length (plist-get head :length)
                         :headers (plist-get head :headers)
                         :http-status (plist-get head :http-status)
                         :http-headers (plist-get head :http-headers)
                         :http-header-len (plist-get head :http-header-len)))

(defun tiles--warc-read-head (reader offset)
  "Parse the head of the record at OFFSET in READER, or return nil."
  (let ((size (tiles--warc-reader-size reader))
        (length tiles--warc-head-size)
        head)
    (while (and (not head) length)
      (let ((pos (tiles--warc-reader-at reader offset length)))
        (with-current-buffer (tiles--warc-reader-buffer reader)
          (setq head (tiles--warc-parse-head pos (point-max)))))
      (unless head
        ;; Unusually large head: look further before giving up.
        (setq length (and (< (+ offset length) size)
                          (< length (* 16 tiles--warc-head-size))
                          (* 4 length)))))
    head))

(defun tiles--warc-skip-newlines (reader offset)
  "Return the offset of the first byte at or after OFFSET that is not a newline.
READER holds the data."
  (let ((size (tiles--warc-reader-size reader))
        done)
    (while (and (not done) (< offset size))
      (let ((pos (tiles--warc-reader-at reader offset 1)))
        (with-current-buffer (tiles--warc-reader-buffer reader)
          (goto-char pos)
          (skip-chars-forward "\r\n")
          (setq offset (+ offset (- (point) pos))
                done (< (point) (point-max))))))
    offset))

(defun tiles--warc-resync (reader from)
  "Return the offset of the first record head at or after FROM in READER."
  (let ((size (tiles--warc-reader-size reader))
        (case-fold-search nil)
        found)
    (while (and (not found) from (< from size))
      (let* ((pos (tiles--warc-reader-at reader from tiles--warc-head-size))
             (candidate (with-current-buffer (tiles--warc-reader-buffer reader)
                          (goto-char pos)
                          (and (re-search-forward "^WARC/1\\.[0-9]+\r?\n" nil t)
                               (+ (- from pos) (match-beginning 0))))))
        (cond
         ((not candidate)
          (let ((end (tiles--warc-reader-end reader)))
            (setq from (and (< end size) (max (1+ from) (- end 12))))))
         ((tiles--warc-read-head reader candidate)
          (setq found candidate))
         (t (setq from (1+ candidate))))))
    found))

(defun tiles--warc-scan-plain (reader member emit)
  "Call EMIT with each record of the uncompressed WARC data in READER.
MEMBER is the gzip member the data was inflated from, or nil."
  (let ((size (tiles--warc-reader-size reader))
        (offset 0)
        last)
    (while (< (setq offset (tiles--warc-skip-newlines reader offset)) size)
      (let ((head (tiles--warc-read-head reader offset)))
        (unless head
          ;; The previous record may be shorter than it claims, or be
          ;; followed by garbage: go on with the next record head.
          (let* ((block (and last (+ (tiles-warc-record-offset last)
                                     (tiles-warc-record-header-len last))))
                 (next (tiles--warc-resync reader (or block offset))))
            (unless next
              (error "No WARC record at offset %d" offset))
            (when (and block (< next offset))
              (setf (tiles-warc-record-length last) (- next block)))
            (setq offset next
                  head (tiles--warc-read-head reader next))))
        (setq last (tiles--warc-make-record head member offset))
        (funcall emit last)
        (setq offset (+ offset (plist-get head :header-len)
                        (plist-get head :length)))
        (unless member
          (tiles--warc-progress offset))))))

(defun tiles--warc-complete-end (reader offset)
  "Return the offset after the complete records from OFFSET in READER."
  (let ((size (tiles--warc-reader-size reader))
        done)
    (while (not done)
      (let* ((start (tiles--warc-skip-newlines reader offset))
             (head (and (< start size) (tiles--warc-read-head reader start)))
             (end (and head (+ start (plist-get head :header-len)
                               (plist-get head :length)))))
        (if (and end (<= end size))
            (setq offset end)
          (setq done t))))
    offset))

(defun tiles--warc-scan-gzip-segment (reader offset end next data emit)
  "Call EMIT with each record of the gzip members from OFFSET in READER.
The first member ends at END, where NEXT is the inflated start of the
following member.  DATA is nil or a buffer holding the inflated first
member.  Members are taken until their records are complete.  Return
\(END . NEXT) for the end of the last member taken."
  (let ((file (tiles--warc-reader-file reader))
        (size (tiles--warc-reader-size reader))
        (buffer (tiles--warc-unibyte-buffer " *tiles-warc-segment*"))
        (members (list (cons offset end)))
        (complete 0))
    (unwind-protect
        (progn
          (tiles--warc-append-member buffer file offset end data)
          (while (and (< end size)
                      (let ((inflated (tiles--warc-buffer-reader buffer)))
                        (setq complete (tiles--warc-complete-end inflated complete))
                        (< (tiles--warc-skip-newlines inflated complete)
                           (tiles--warc-reader-size inflated))))
            (pcase-let ((`(,member-end ,member-next ,_ ,member-data)
                         (tiles--warc-gzip-member-end reader end nil)))
              (tiles--warc-append-member buffer file end member-end member-data)
              (push (cons end member-end) members)
              (setq end member-end
                    next member-next)))
          (setq members (nreverse members))
          (tiles--warc-scan-plain (tiles--warc-buffer-reader buffer)
                              (if (cdr members) members (car members))
                              emit))
      (kill-buffer buffer))
    (cons end next)))

(defun tiles--warc-scan-gzip (reader emit)
  "Call EMIT with each record of the gzip-compressed WARC data in READER."
  (let ((size (tiles--warc-reader-size reader))
        (offset 0)
        (peek (car (tiles--warc-gunzip-peek reader 0))))
    (while (< offset size)
      (let* ((start (tiles--warc-warc-start peek))
             (head (and start
                        (with-temp-buffer
                          (set-buffer-multibyte nil)
                          (insert peek)
                          (tiles--warc-parse-head (1+ start) (point-max))))))
        (pcase-let ((`(,end ,next ,aligned ,data)
                     (tiles--warc-gzip-member-end
                      reader offset
                      (and head (+ start (plist-get head :header-len)
                                   (plist-get head :length) 4)))))
          (if (and head aligned)
              (funcall emit (tiles--warc-make-record head (cons offset end) start))
            (let ((segment (tiles--warc-scan-gzip-segment reader offset end next
                                                      data emit)))
              (setq end (car segment)
                    next (cdr segment))))
          (tiles--warc-progress end)
          (setq offset end
                peek next))))))

(defun tiles--warc-file-format (reader)
  "Return the format of the file read by READER: gzip, zstd, warc or nil."
  (let* ((pos (tiles--warc-reader-at reader 0 8))
         (magic (with-current-buffer (tiles--warc-reader-buffer reader)
                  (buffer-substring-no-properties pos (point-max)))))
    (cond
     ((string-prefix-p (unibyte-string #x1f #x8b) magic) 'gzip)
     ((string-prefix-p (unibyte-string #x28 #xb5 #x2f #xfd) magic) 'zstd)
     ((string-match-p "\\`[\r\n]*WARC/" magic) 'warc))))

(defun tiles-warc-read-archive (file)
  "Read the records of the WARC FILE and return a `tiles-warc-archive'."
  (unless (file-readable-p file)
    (user-error "Cannot read %s" file))
  (let* ((file (expand-file-name file))
         (reader (tiles--warc-file-reader file))
         (records nil)
         (count 0)
         (emit (lambda (record)
                 (setf (tiles-warc-record-index record) (cl-incf count))
                 (push record records)))
         (tiles--warc-progress (make-progress-reporter
                            (format "Reading %s..." (file-name-nondirectory file))
                            0 (max 1 (tiles--warc-reader-size reader))))
         problem)
    (unwind-protect
        (condition-case err
            (pcase (tiles--warc-file-format reader)
              ('gzip (tiles--warc-scan-gzip reader emit))
              ('warc (tiles--warc-scan-plain reader nil emit))
              ('zstd (user-error "Zstandard-compressed WARC files are not supported"))
              (_ (user-error "Not a WARC file: %s" file)))
          (error
           ;; Keep what was read from a truncated or damaged file.
           (if records
               (setq problem (error-message-string err))
             (signal (car err) (cdr err)))))
      (kill-buffer (tiles--warc-reader-buffer reader)))
    (progress-reporter-done tiles--warc-progress)
    (when problem
      (message "Stopped reading %s after %d records: %s"
               (file-name-nondirectory file) count problem))
    (tiles--warc-make-archive file (vconcat (nreverse records)))))

;;;; Archive indexes

(defun tiles-warc-record-header (record name)
  "Return the value of the WARC header field NAME of RECORD."
  (tiles--warc-field (tiles-warc-record-headers record) name))

(defun tiles-warc-record-type (record)
  "Return the WARC type of RECORD, such as \"response\"."
  (downcase (or (tiles-warc-record-header record "WARC-Type") "")))

(defun tiles-warc-record-uri (record)
  "Return the target URI of RECORD, or nil."
  (let ((uri (tiles-warc-record-header record "WARC-Target-URI")))
    (and uri (string-trim uri "[< \t]+" "[> \t]+"))))

(defun tiles-warc-record-content-type (record)
  "Return the content type of RECORD's payload, with its parameters."
  (or (if (tiles-warc-record-http-status record)
          (tiles--warc-field (tiles-warc-record-http-headers record) "Content-Type")
        (tiles-warc-record-header record "Content-Type"))
      ""))

(defun tiles-warc-record-mime (record)
  "Return the media type of RECORD's payload, such as \"text/html\"."
  (downcase (string-trim (car (split-string (tiles-warc-record-content-type record)
                                            ";")))))

(defun tiles--warc-payload-size (record)
  "Return the size of RECORD's payload as stored in the archive."
  (max 0 (- (tiles-warc-record-length record)
            (or (tiles-warc-record-http-header-len record) 0))))

(defun tiles--warc-normalize-uri (uri)
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

(defun tiles--warc-uri-variants (uri)
  "Return the forms of URI to look up, most specific first."
  (let* ((uri (tiles--warc-normalize-uri uri))
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

(defun tiles--warc-make-archive (file records)
  "Return an archive of the vector RECORDS read from FILE."
  (let ((by-uri (make-hash-table :test #'equal))
        (by-id (make-hash-table :test #'equal))
        (by-digest (make-hash-table :test #'equal)))
    (seq-doseq (record records)
      (let ((type (tiles-warc-record-type record))
            (id (tiles-warc-record-header record "WARC-Record-ID"))
            (uri (tiles-warc-record-uri record))
            (digest (tiles-warc-record-header record "WARC-Payload-Digest")))
        (when id
          (puthash id record by-id))
        (when (and uri (member type '("response" "resource" "revisit" "conversion")))
          (push record (gethash (tiles--warc-normalize-uri uri) by-uri)))
        (when (and digest (member type '("response" "resource"))
                   (not (gethash digest by-digest)))
          (puthash digest record by-digest))))
    (maphash (lambda (key value) (puthash key (nreverse value) by-uri)) by-uri)
    (tiles-warc-archive--create :file file :records records :by-uri by-uri
                            :by-id by-id :by-digest by-digest)))

(defun tiles--warc-lookup (archive url)
  "Return the record of ARCHIVE that best represents URL, or nil."
  (when-let* ((records (seq-some (lambda (form)
                                   (gethash form (tiles-warc-archive-by-uri archive)))
                                 (tiles--warc-uri-variants url))))
    (or (seq-find (lambda (record)
                    (let ((status (tiles-warc-record-http-status record)))
                      (and status (<= 200 status 299))))
                  records)
        (seq-find (lambda (record)
                    (equal (tiles-warc-record-type record) "resource"))
                  records)
        (car records))))

(defun tiles--warc-revisit-target (archive record)
  "Return the record of ARCHIVE holding the payload of the revisit RECORD."
  (or (when-let* ((id (tiles-warc-record-header record "WARC-Refers-To")))
        (gethash id (tiles-warc-archive-by-id archive)))
      (when-let* ((digest (tiles-warc-record-header record "WARC-Payload-Digest")))
        (gethash digest (tiles-warc-archive-by-digest archive)))
      (when-let* ((uri (or (tiles-warc-record-header record "WARC-Refers-To-Target-URI")
                           (tiles-warc-record-uri record))))
        (seq-find (lambda (other)
                    (member (tiles-warc-record-type other) '("response" "resource")))
                  (gethash (tiles--warc-normalize-uri uri)
                           (tiles-warc-archive-by-uri archive))))))

(defun tiles--warc-redirect-location (record)
  "Return the Location of RECORD if it is an HTTP redirect."
  (let ((status (tiles-warc-record-http-status record)))
    (and status (<= 300 status 399)
         (tiles--warc-field (tiles-warc-record-http-headers record) "Location"))))

(defun tiles--warc-resolve (archive record &optional keep-redirects)
  "Return the record of ARCHIVE whose payload should be shown for RECORD.
Revisit records are replaced by the record they refer to, and
redirects by their archived target unless KEEP-REDIRECTS is non-nil."
  (let (seen next)
    (while (and (not (memq record seen))
                (setq next
                      (if (equal (tiles-warc-record-type record) "revisit")
                          (tiles--warc-revisit-target archive record)
                        (when-let* (((not keep-redirects))
                                    (location (tiles--warc-redirect-location record)))
                          (tiles--warc-lookup archive
                                          (tiles--warc-expand-url
                                           location
                                           (tiles-warc-record-uri record)))))))
      (push record seen)
      (setq record next))
    record))

;;;; Payloads

(defvar tiles--warc-member-cache nil
  "The last inflated gzip member, as (KEY . BUFFER).")

(defun tiles--warc-member-data (file member)
  "Return a buffer holding the inflated gzip MEMBER of FILE.
MEMBER is (BEG . END), or a list of them to inflate one after the other."
  (let ((key (list file member
                   (file-attribute-modification-time (file-attributes file))))
        (buffer (cdr tiles--warc-member-cache)))
    (unless (and (buffer-live-p buffer)
                 (equal key (car tiles--warc-member-cache)))
      (if (buffer-live-p buffer)
          (with-current-buffer buffer (erase-buffer))
        (setq buffer (tiles--warc-unibyte-buffer " *tiles-warc-member*")))
      (dolist (range (if (consp (car member)) member (list member)))
        (tiles--warc-inflate-member file (car range) (cdr range) buffer))
      (setq tiles--warc-member-cache (cons key buffer)))
    buffer))

(defun tiles--warc-record-block (archive record)
  "Return the block of RECORD in ARCHIVE as a unibyte string."
  (let* ((file (tiles-warc-archive-file archive))
         (start (+ (tiles-warc-record-offset record)
                   (tiles-warc-record-header-len record)))
         (end (+ start (tiles-warc-record-length record)))
         (member (tiles-warc-record-member record)))
    (if member
        (with-current-buffer (tiles--warc-member-data file member)
          (buffer-substring-no-properties (min (1+ start) (point-max))
                                          (min (1+ end) (point-max))))
      (let ((size (file-attribute-size (file-attributes file))))
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert-file-contents-literally file nil (min start size)
                                          (min end size))
          (buffer-string))))))

(defun tiles--warc-split-http (block)
  "Split the HTTP message BLOCK into a list (START-LINE HEADERS BODY)."
  (if (string-match "\n\r?\n" block)
      (let* ((body (substring block (match-end 0)))
             (lines (split-string (substring block 0 (match-beginning 0))
                                  "\r?\n")))
        (list (car lines) (tiles--warc-parse-fields (cdr lines)) body))
    (list block nil "")))

(defun tiles--warc-dechunk (body)
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

(defun tiles--warc-inflate (data)
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

(defun tiles--warc-record-payload (archive record)
  "Return the payload of RECORD in ARCHIVE as a unibyte string.
Transfer and content codings of HTTP responses are removed."
  (let ((block (tiles--warc-record-block archive record)))
    (if (not (tiles-warc-record-http-status record))
        block
      (pcase-let ((`(,_ ,headers ,body) (tiles--warc-split-http block)))
        (let ((transfer (downcase (or (tiles--warc-field headers "Transfer-Encoding") "")))
              (content (downcase (string-trim
                                  (or (tiles--warc-field headers "Content-Encoding") "")))))
          (when (string-search "chunked" transfer)
            (setq body (tiles--warc-dechunk body)))
          (cond
           ((or (string-empty-p body)
                (member content '("" "identity" "none")))
            body)
           ((member content '("gzip" "x-gzip" "deflate"))
            ;; Some crawlers store the payload already decoded.
            (or (tiles--warc-inflate body) body))
           (t (user-error "Unsupported content encoding: %s" content))))))))

(defconst tiles--warc-charset-aliases
  '(("iso-8859-1" . windows-1252) ("latin1" . windows-1252)
    ("us-ascii" . windows-1252) ("ascii" . windows-1252)
    ("utf8" . utf-8) ("x-sjis" . shift_jis) ("shift-jis" . shift_jis)
    ("gb2312" . gbk) ("x-gbk" . gbk) ("ks_c_5601-1987" . euc-kr))
  "Charset names that differ from the Emacs coding system to use.")

(defun tiles--warc-coding-system (charset)
  "Return the coding system for the MIME CHARSET name, or nil."
  (let* ((name (downcase (string-trim charset "[\"' \t]+" "[\"' \t]+")))
         (coding (or (cdr (assoc name tiles--warc-charset-aliases)) (intern name))))
    (and (coding-system-p coding) coding)))

(defun tiles--warc-charset (content-type bytes)
  "Return the coding system of the text BYTES with CONTENT-TYPE."
  (let ((case-fold-search t)
        (head (substring bytes 0 (min 4096 (length bytes)))))
    (or (and (string-match "charset=\\([^;]+\\)" content-type)
             (tiles--warc-coding-system (match-string 1 content-type)))
        (and (string-match "<meta[^>]*charset=[\"']?\\([-_.:a-z0-9]+\\)" head)
             (tiles--warc-coding-system (match-string 1 head)))
        'prefer-utf-8)))

(defun tiles--warc-payload-text (archive record)
  "Return the payload of RECORD in ARCHIVE decoded as text."
  (let ((bytes (tiles--warc-record-payload archive record)))
    (decode-coding-string
     bytes (tiles--warc-charset (tiles-warc-record-content-type record) bytes))))

;;;; HTML

(defconst tiles--warc-dropped-tags
  '(script noscript style template link meta base
    img image picture source track video audio canvas svg math map area
    iframe object embed applet param
    input button select option optgroup textarea datalist output
    progress meter comment)
  "HTML elements removed from pages before they are shown.")

(defconst tiles--warc-kept-attributes
  '(href id name title start value colspan rowspan dir lang)
  "HTML attributes kept on elements of pages that are shown.")

(defun tiles--warc-expand-url (url base)
  "Return URL made absolute with respect to BASE."
  (let ((url (string-trim url)))
    (if (or (not base) (string-empty-p base)
            (string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*:" url))
        url
      (condition-case nil
          (url-expand-file-name url base)
        (error url)))))

(defun tiles--warc-link-url-p (url &optional absolute)
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

(defun tiles--warc-hidden-p (node)
  "Return non-nil if the HTML element NODE is not meant to be shown."
  (let ((attributes (dom-attributes node))
        (case-fold-search t))
    (or (assq 'hidden attributes)
        (equal (cdr (assq 'aria-hidden attributes)) "true")
        (when-let* ((style (cdr (assq 'style attributes))))
          (string-match-p "display[ \t]*:[ \t]*none\\|visibility[ \t]*:[ \t]*hidden"
                          style)))))

(defun tiles--warc-dom-text (node)
  "Return all the text in the DOM NODE."
  (if (fboundp 'dom-inner-text)
      (dom-inner-text node)
    (with-no-warnings (dom-texts node ""))))

(defun tiles--warc-has-text-p (nodes)
  "Return non-nil if the DOM NODES contain visible text."
  (seq-some (lambda (node)
              (if (stringp node)
                  (string-match-p "[^ \t\n\r]" node)
                (tiles--warc-has-text-p (dom-children node))))
            nodes))

(defun tiles--warc-image-alt (node)
  "Return the first non-empty alternative text of an image in NODE."
  (seq-some (lambda (image)
              (let ((alt (dom-attr image 'alt)))
                (and alt (string-match-p "[^ \t\n\r]" alt) (string-trim alt))))
            (dom-by-tag node 'img)))

(defun tiles--warc-sanitize (node)
  "Return a copy of the DOM NODE fit to be shown, or nil to drop it.
Scripts, styles, media, embedded frames and form controls are removed,
and so are all attributes but `tiles--warc-kept-attributes'."
  (cond
   ((stringp node) node)
   ((or (not (consp node))
        (memq (dom-tag node) tiles--warc-dropped-tags)
        (tiles--warc-hidden-p node))
    nil)
   ((eq (dom-tag node) 'head)
    (when-let* ((title (car (dom-by-tag node 'title))))
      `(head nil (title nil ,(tiles--warc-dom-text title)))))
   ((eq (dom-tag node) 'frame)
    ;; Framesets would be empty without their frames: link to them.
    (when-let* ((src (dom-attr node 'src))
                ((tiles--warc-link-url-p src)))
      `(p nil (a ((href . ,src)) ,(format "Frame: %s" (or (dom-attr node 'name) src))))))
   (t
    (let ((children (delq nil (mapcar #'tiles--warc-sanitize (dom-children node))))
          (attributes (seq-filter
                       (lambda (attribute)
                         (and (memq (car attribute) tiles--warc-kept-attributes)
                              (or (not (eq (car attribute) 'href))
                                  (tiles--warc-link-url-p (cdr attribute)))))
                       (dom-attributes node))))
      ;; Keep links whose only content was an image.
      (when (and (eq (dom-tag node) 'a) (not (tiles--warc-has-text-p children)))
        (when-let* ((alt (tiles--warc-image-alt node)))
          (setq children (list alt))))
      `(,(if (eq (dom-tag node) 'frameset) 'div (dom-tag node))
        ,attributes ,@children)))))

(defun tiles--warc-meta-refresh (dom)
  "Return the URL that a meta refresh element of DOM redirects to."
  (let ((case-fold-search t))
    (seq-some (lambda (meta)
                (when (string-equal-ignore-case (or (dom-attr meta 'http-equiv) "")
                                                "refresh")
                  (let ((content (or (dom-attr meta 'content) "")))
                    (when (string-match "url[ \t]*=[ \t]*[\"']?\\([^\"']+\\)" content)
                      (string-trim (match-string 1 content))))))
              (dom-by-tag dom 'meta))))

(defun tiles--warc-in-tag-p (pos)
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

(defun tiles--warc-mark-hidden (html)
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
            (when (tiles--warc-in-tag-p (match-beginning 1))
              (goto-char end)
              (insert "=\"\""))))
        (buffer-string)))))

(defun tiles--warc-parse-html (text)
  "Parse the HTML TEXT into a DOM."
  (with-temp-buffer
    (insert (tiles--warc-mark-hidden text))
    (libxml-parse-html-region (point-min) (point-max))))

;;;; Main content of a page

;; Finding the article of a page, as reading views in browsers do: the
;; element holding most of its paragraphs is the content, widened to
;; the article or main element around it; within it, what is around
;; the text of an article is left out.  This works on the page as
;; parsed, before `tiles--warc-sanitize' removes the classes, ids and roles
;; it relies on.

;; Word boundaries are spelled out: \\b and \\_< depend on the syntax
;; table of the current buffer.
(defconst tiles--warc-unlikely-regexp
  (concat "-ad-\\|\\(?:^\\|[^a-z]\\)ads?\\(?:[^a-z]\\|$\\)\\|advert\\|agegate"
          "\\|banner\\|breadcrumb\\|combx\\|comment\\|community\\|consent\\|cookie"
          "\\|disqus\\|footer\\|gdpr\\|header\\|legends\\|masthead\\|menu"
          "\\|modal\\|\\(?:^\\|[^a-z]\\)nav\\|newsletter\\|outbrain\\|pager"
          "\\|pagination\\|popup\\|promo\\|related\\|remark\\|replies\\|rss\\|share"
          "\\|sharing\\|shoutbox\\|sidebar\\|skip\\|skyscraper\\|social\\|sponsor"
          "\\|subscribe\\|supplemental\\|taboola\\|toolbar\\|widget")
  "Regexp matching the classes and ids of what is around the content of a page.")

(defconst tiles--warc-maybe-regexp
  (concat "article\\|\\(?:^\\|[^a-z]\\)body\\(?:[^a-z]\\|$\\)\\|column\\|content"
          "\\|\\(?:^\\|[^a-z]\\)main\\(?:[^a-z]\\|$\\)\\|story\\|entry"
          "\\|\\(?:^\\|[^a-z]\\)post")
  "Regexp matching classes and ids that may be the content of a page.
An element whose classes or ids match it is kept even when they also
match `tiles--warc-unlikely-regexp'.")

(defconst tiles--warc-boilerplate-roles
  '("navigation" "banner" "contentinfo" "complementary" "search" "menu"
    "menubar" "dialog" "alertdialog" "alert" "tooltip")
  "ARIA roles of what is around the content of a page.")

(defun tiles--warc-long-paragraphs (node)
  "Return how many paragraphs in NODE are long ones."
  (seq-count (lambda (paragraph) (>= (tiles--warc-text-length paragraph) 150))
             (dom-by-tag node 'p)))

(defun tiles--warc-holds-article-p (node &optional paragraphs)
  "Return non-nil if NODE holds the article of the page.
It does when it contains the main element, or an article element with
a few long paragraphs: not one that is a comment or a link to another
page.  With PARAGRAPHS non-nil, it also does when it has a few long
paragraphs anywhere, as when broken markup puts a page in its header."
  (or (dom-search node (lambda (child)
                         (or (eq (dom-tag child) 'main)
                             (equal (dom-attr child 'role) "main")
                             (and (eq (dom-tag child) 'article)
                                  (>= (tiles--warc-long-paragraphs child) 3)))))
      (and paragraphs (>= (tiles--warc-long-paragraphs node) 3))))

(defun tiles--warc-boilerplate-p (node)
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
                         tiles--warc-boilerplate-roles))
             (not (tiles--warc-holds-article-p node t)))
        (and (not (memq tag '(html body article main a)))
             (string-match-p tiles--warc-unlikely-regexp names)
             (not (string-match-p tiles--warc-maybe-regexp names))
             (not (tiles--warc-holds-article-p node))))))

(defun tiles--warc-text-length (node)
  "Return the length of the text in NODE, without surrounding whitespace."
  (length (string-trim (tiles--warc-dom-text node))))

(defun tiles--warc-link-density (node)
  "Return the share of the text of NODE that is in links, from 0 to 1."
  (let ((length (tiles--warc-text-length node)))
    (if (zerop length)
        0
      (/ (float (apply #'+ (mapcar #'tiles--warc-text-length (dom-by-tag node 'a))))
         length))))

(defun tiles--warc-link-list-p (node)
  "Return non-nil if NODE is a block of little more than links.
Menus and lists of related pages are such blocks.  Tables are not
looked at: tables of data often have a link on each row."
  (and (memq (dom-tag node) '(ul ol div section))
       (> (tiles--warc-link-density node) 0.5)
       (not (tiles--warc-holds-article-p node))))

(defun tiles--warc-readable-clean (node)
  "Return a copy of NODE without what is around the content of a page."
  (cond
   ((stringp node) node)
   ;; What is never shown goes first, so that the text of styles and
   ;; scripts does not count as text of the page.
   ((or (memq (dom-tag node) tiles--warc-dropped-tags) (tiles--warc-hidden-p node)) nil)
   ((or (tiles--warc-boilerplate-p node) (tiles--warc-link-list-p node)) nil)
   (t `(,(dom-tag node) ,(dom-attributes node)
        ,@(delq nil (mapcar #'tiles--warc-readable-clean (dom-children node)))))))

(defconst tiles--warc-block-tags
  '(address article aside blockquote details dialog dd div dl dt fieldset
    figcaption figure footer form h1 h2 h3 h4 h5 h6 header hr li main nav ol
    p pre section table ul)
  "HTML elements that start a block of their own.")

(defun tiles--warc-div-paragraph-p (node)
  "Return non-nil if NODE is a div used as a paragraph: text without blocks."
  (and (eq (dom-tag node) 'div)
       (not (seq-some (lambda (child) (memq (dom-tag child) tiles--warc-block-tags))
                      (dom-non-text-children node)))))

(defun tiles--warc-dom-parents (dom)
  "Return a hash table mapping each element of DOM to its parent."
  (let ((parents (make-hash-table :test #'eq)))
    (cl-labels ((walk (node)
                  (dolist (child (dom-non-text-children node))
                    (puthash child node parents)
                    (walk child))))
      (walk dom))
    parents))

(defun tiles--warc-main-element (body)
  "Return the element of BODY that holds the main content, or nil.
Each paragraph scores for its parent, and half as much for its
grandparent; the best element, its links discounted, wins.  It is
widened to the article or main element around it, if any."
  (let ((parents (tiles--warc-dom-parents body))
        (scores (make-hash-table :test #'eq))
        best best-score)
    (dolist (paragraph (append (dom-by-tag body 'p) (dom-by-tag body 'pre)
                               ;; Pages also write paragraphs as divs.
                               (dom-search body #'tiles--warc-div-paragraph-p)))
      (let* ((text (string-trim (tiles--warc-dom-text paragraph)))
             (score (+ 1 (cl-count ?, text) (min 3 (/ (length text) 100)))))
        (when (>= (length text) 25)
          (when-let* ((parent (gethash paragraph parents)))
            (puthash parent (+ score (gethash parent scores 0)) scores)
            (when-let* ((grandparent (gethash parent parents)))
              (puthash grandparent (+ (/ score 2.0) (gethash grandparent scores 0))
                       scores))))))
    (maphash (lambda (node score)
               (let ((score (* score (- 1 (tiles--warc-link-density node)))))
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
          (tiles--warc-with-content-siblings best (gethash best parents) scores
                                         (max 10 (* 0.2 best-score))))))))

(defun tiles--warc-with-content-siblings (best parent scores threshold)
  "Return BEST with the siblings that are content too, in their order.
An article is often split into several elements under PARENT: those
scoring at least THRESHOLD in SCORES are content, and so are the
paragraphs, headings, lists and quotes around them with few links."
  (let ((kept (seq-filter
               (lambda (sibling)
                 (and (consp sibling)
                      (or (eq sibling best)
                          (>= (* (gethash sibling scores 0)
                                 (- 1 (tiles--warc-link-density sibling)))
                              threshold)
                          (and (memq (dom-tag sibling)
                                     '(p h1 h2 h3 h4 h5 h6 ul ol dl blockquote pre figure table))
                               (tiles--warc-has-text-p (list sibling))
                               (< (tiles--warc-link-density sibling) 0.25)))))
               (and parent (dom-children parent)))))
    (if (cdr kept) `(div nil ,@kept) best)))

(defun tiles--warc-dom-title-text (node)
  "Return the text of NODE on one line, or nil if it has none."
  (let ((text (string-trim (replace-regexp-in-string
                            "[ \t\n\r]+" " " (tiles--warc-dom-text node)))))
    (and (not (string-empty-p text)) text)))

(defun tiles--warc-readable-dom (dom)
  "Return the title and main content of the page DOM, as (TITLE . DOM).
The DOM returned holds only the main content, without the title when
it opened it.  When no article is found, as on pages behind a paywall,
it is the whole page without what is around content: menus, headers,
footers, sidebars, dialogs.  Return nil if nothing is left."
  ;; What is around the content goes first, so that a long comment
  ;; section, say, can't be taken for the article.
  (when-let* ((body (tiles--warc-readable-clean (or (car (dom-by-tag dom 'body)) dom)))
              ((tiles--warc-has-text-p (list body)))
              (content (or (tiles--warc-main-element body) body)))
    (let* ((heading (car (dom-by-tag content 'h1)))
           (title (or (and heading (tiles--warc-dom-title-text heading))
                      (seq-some (lambda (meta)
                                  (and (equal (dom-attr meta 'property) "og:title")
                                       (dom-attr meta 'content)))
                                (dom-by-tag dom 'meta)))))
      ;; The title is shown as such: not again as the first heading.
      (when (and heading (equal title (tiles--warc-dom-title-text heading)))
        (dom-remove-node content heading))
      (cons title `(html nil (body nil ,content))))))

(defun tiles--warc-html-p (archive record)
  "Return non-nil if the payload of RECORD in ARCHIVE is an HTML page."
  (let ((mime (tiles-warc-record-mime record)))
    (or (member mime '("text/html" "application/xhtml+xml"))
        (tiles--warc-redirect-location record)
        (and (member mime '("" "application/octet-stream"))
             (let ((case-fold-search t)
                   (payload (tiles--warc-record-payload archive record)))
               (string-match-p "\\`\\(?:\357\273\277\\)?[ \t\r\n]*<\\(?:!doctype html\\|html\\|head\\|body\\)"
                               (substring payload 0 (min 512 (length payload)))))))))

(defun tiles--warc-page (archive record &optional readable)
  "Return the HTML page stored in RECORD of ARCHIVE, ready to be shown.
The result is a plist with :record, :uri, :date, :title, :base and
:dom, the sanitized DOM of the page.  With READABLE non-nil, the DOM
is only the main content of the page when it has one (see
`tiles--warc-readable-dom'), and :readable is then non-nil."
  (let* ((max-lisp-eval-depth (max max-lisp-eval-depth 4000))
         (payload (tiles--warc-record-payload archive record))
         (dom (if (string-blank-p payload)
                  (list 'html nil (list 'body nil))
                (tiles--warc-parse-html
                 (decode-coding-string
                  payload (tiles--warc-charset (tiles-warc-record-content-type record)
                                           payload)))))
         (uri (or (tiles-warc-record-uri record) ""))
         (base-href (dom-attr (car (dom-by-tag dom 'base)) 'href))
         (base (if base-href (tiles--warc-expand-url base-href uri) uri))
         (title (when-let* ((node (car (dom-by-tag dom 'title))))
                  (string-trim (replace-regexp-in-string
                                "[ \t\n\r]+" " " (tiles--warc-dom-text node)))))
         (redirect (or (tiles--warc-redirect-location record)
                       (tiles--warc-meta-refresh dom)))
         (main (and readable (not redirect) (tiles--warc-readable-dom dom)))
         (title (or (car main) title))
         (clean (or (tiles--warc-sanitize (if main (cdr main) dom)) (list 'html nil))))
    (when redirect
      (let ((target (tiles--warc-expand-url redirect base)))
        (dom-add-child-before (or (car (dom-by-tag clean 'body)) clean)
                              `(p nil "Redirects to " (a ((href . ,target)) ,target)))))
    (list :record record
          :uri uri
          :date (tiles-warc-record-header record "WARC-Date")
          :title (if (or (not title) (string-empty-p title)) uri title)
          :base base
          :readable (and main t)
          :dom clean)))

;;;; HTML to Org

(defconst tiles--warc-zwsp "\u200b"
  "Zero-width space, used where Org markup must be separated from text.")

(defvar tiles--warc-org-base nil
  "URL that relative links of the page being converted are relative to.")

(defvar tiles--warc-org-inline nil
  "Non-nil when converting inside a link, heading or table cell.
Block structure is then flattened into running text.")

(defvar tiles--warc-org-in-block nil
  "Non-nil inside list items, quotes and tables, where headings cannot go.")

(defvar tiles--warc-org-in-quote nil
  "Non-nil inside a quote block.")

(defvar tiles--warc-org-in-link nil
  "Non-nil inside a link.")

(defvar tiles--warc-org-markers nil
  "Emphasis markers of the enclosing emphasized text.")

(defun tiles--warc-org-after-space-p ()
  "Return non-nil if the Org text so far ends with a space or nothing."
  (goto-char (point-max))
  (or (bobp) (memq (char-before) '(?\s ?\t ?\n #x200b))))

(defun tiles--warc-org-text (string)
  "Insert the text of a DOM text node STRING."
  (let ((text (replace-regexp-in-string "[ \t\n\r\f\u00a0]+" " " string)))
    (when (and (string-prefix-p " " text) (tiles--warc-org-after-space-p))
      (setq text (substring text 1)))
    (unless (string-empty-p text)
      (goto-char (point-max))
      (when (or (and (bolp) (string-match-p "\\`[*#|:]" text))
                (and (> (point) (point-min))
                     (get-text-property (1- (point)) 'tiles-warc-closing)
                     (string-match-p "\\`[[:alnum:]]" text)))
        ;; Keep Org from reading the text as markup, or from missing
        ;; the end of the emphasis just before it.
        (insert tiles--warc-zwsp))
      (insert text))))

(defun tiles--warc-org-newlines (n)
  "End the Org text with at least N newlines, unless it is empty."
  (goto-char (point-max))
  (if tiles--warc-org-inline
      (unless (tiles--warc-org-after-space-p)
        (insert " "))
    (let ((end (point)))
      (skip-chars-backward " \t\n")
      (if (bobp)
          (delete-region (point) end)
        (let ((have (cl-count ?\n (buffer-substring-no-properties (point) end))))
          (delete-region (point) end)
          (insert (make-string (max n have) ?\n)))))))

(defun tiles--warc-org-children (node)
  "Insert the Org text of the children of NODE."
  (dolist (child (dom-children node))
    (tiles--warc-org-node child)))

(defun tiles--warc-org-block (node n)
  "Insert NODE as a block separated from its surroundings by N newlines."
  (tiles--warc-org-newlines n)
  (tiles--warc-org-children node)
  (tiles--warc-org-newlines n))

(defun tiles--warc-org-flat (text)
  "Return TEXT on a single line."
  (string-trim (replace-regexp-in-string "[ \t\n]+" " " text)))

(defun tiles--warc-org-render (nodes &optional inline)
  "Return the Org text of the DOM NODES, converted on their own.
If INLINE is non-nil, flatten block structure."
  (let ((tiles--warc-org-in-block t)
        (tiles--warc-org-inline (or inline tiles--warc-org-inline))
        (tiles--warc-org-markers nil))
    (with-temp-buffer
      (dolist (node nodes)
        (tiles--warc-org-node node))
      (tiles--warc-org-tidy)
      (buffer-string))))

(defun tiles--warc-org-tidy ()
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

(defun tiles--warc-org-indent (text width)
  "Return TEXT with all lines but the first indented by WIDTH spaces."
  (replace-regexp-in-string "\n\\([^\n]\\)"
                            (concat "\n" (make-string width ?\s) "\\1")
                            text t))

(defun tiles--warc-org-wrap (start marker)
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
      (insert (propertize marker 'tiles-warc-closing t))
      (goto-char beg)
      (unless (or (= beg (point-min))
                  (memq (char-before) '(?\s ?\t ?\n ?- ?\( ?\" ?' ?{ #x200b)))
        (insert tiles--warc-zwsp))
      (insert marker))
    (goto-char (point-max))))

(defun tiles--warc-org-emphasis (node marker)
  "Insert the children of NODE emphasized with MARKER."
  (if (member marker tiles--warc-org-markers)
      (tiles--warc-org-children node)
    (goto-char (point-max))
    (let ((start (copy-marker (point)))
          (tiles--warc-org-markers (cons marker tiles--warc-org-markers)))
      (tiles--warc-org-children node)
      (tiles--warc-org-wrap start marker)
      (set-marker start nil))))

(defun tiles--warc-org-verbatim (node)
  "Insert the children of NODE as verbatim text."
  (goto-char (point-max))
  (let ((start (copy-marker (point))))
    (tiles--warc-org-children node)
    (let* ((text (buffer-substring-no-properties start (point-max)))
           (marker (cond ((string-search "[[" text) nil)
                         ((not (string-search "~" text)) "~")
                         ((not (string-search "=" text)) "="))))
      (when marker
        (tiles--warc-org-wrap start marker)))
    (set-marker start nil)))

(defun tiles--warc-org-link (node)
  "Insert the link NODE."
  (let* ((href (string-trim (or (dom-attr node 'href) "")))
         (url (unless (or tiles--warc-org-in-link
                          (string-empty-p href)
                          (string-prefix-p "#" href))
                (tiles--warc-expand-url href tiles--warc-org-base))))
    ;; Checked again once made absolute: the page's base or redirect
    ;; could give it another scheme, and Org would take a path that is
    ;; still relative for a local file.
    (if (not (and url (tiles--warc-link-url-p url t)))
        (tiles--warc-org-children node)
      (goto-char (point-max))
      (let ((start (copy-marker (point))))
        (let ((tiles--warc-org-in-link t)
              (tiles--warc-org-inline t))
          (tiles--warc-org-children node))
        (let* ((raw (buffer-substring-no-properties start (point-max)))
               (description (tiles--warc-org-flat raw)))
          (delete-region start (point-max))
          (unless (string-empty-p description)
            (when (and (string-match-p "\\`[ \t\n]" raw)
                       (not (tiles--warc-org-after-space-p)))
              (insert " "))
            (insert (org-link-make-string url description))
            (when (string-match-p "[ \t\n]\\'" raw)
              (insert " "))))
        (set-marker start nil)))))

(defun tiles--warc-org-break ()
  "Insert a line break."
  (goto-char (point-max))
  (if tiles--warc-org-inline
      (unless (tiles--warc-org-after-space-p)
        (insert " "))
    (delete-region (progn (skip-chars-backward " \t") (point)) (point-max))
    (insert "\n")))

(defun tiles--warc-org-rule ()
  "Insert a horizontal rule."
  (unless tiles--warc-org-inline
    (tiles--warc-org-newlines 2)
    (insert "-----")
    (tiles--warc-org-newlines 2)))

(defun tiles--warc-org-heading (node level)
  "Insert the heading NODE of LEVEL."
  (cond
   (tiles--warc-org-inline
    (tiles--warc-org-children node))
   (tiles--warc-org-in-block
    (tiles--warc-org-newlines 2)
    (tiles--warc-org-emphasis node "*")
    (tiles--warc-org-newlines 2))
   (t
    (tiles--warc-org-newlines 2)
    (let ((start (copy-marker (point))))
      (let ((tiles--warc-org-inline t))
        (tiles--warc-org-children node))
      (let ((title (tiles--warc-org-flat (buffer-substring start (point-max)))))
        (delete-region start (point-max))
        (unless (string-empty-p title)
          (insert (make-string level ?*) " " title)
          (tiles--warc-org-newlines 2)))
      (set-marker start nil)))))

(defun tiles--warc-dom-raw-text (node)
  "Return the text of NODE with its white space preserved."
  (mapconcat (lambda (child)
               (cond ((stringp child) child)
                     ((eq (dom-tag child) 'br) "\n")
                     (t (tiles--warc-dom-raw-text child))))
             (dom-children node)
             ""))

(defun tiles--warc-org-pre (node)
  "Insert the preformatted NODE as an example block."
  (if tiles--warc-org-inline
      (tiles--warc-org-children node)
    (let ((text (string-trim-right
                 (string-remove-prefix
                  "\n" (string-replace "\r" "" (tiles--warc-dom-raw-text node))))))
      (unless (string-blank-p text)
        (tiles--warc-org-newlines 2)
        (insert "#+begin_example\n"
                (replace-regexp-in-string "^[ \t]*\\(,*\\(?:\\*\\|#\\+\\)\\)"
                                          ",\\1" text nil nil 1)
                "\n#+end_example")
        (tiles--warc-org-newlines 2)))))

(defun tiles--warc-org-quote (node)
  "Insert the block quote NODE."
  (if (or tiles--warc-org-inline tiles--warc-org-in-quote)
      (tiles--warc-org-block node 2)
    (let ((text (let ((tiles--warc-org-in-quote t))
                  (tiles--warc-org-render (dom-children node)))))
      (unless (string-empty-p text)
        (tiles--warc-org-newlines 2)
        (insert "#+begin_quote\n" text "\n#+end_quote")
        (tiles--warc-org-newlines 2)))))

(defun tiles--warc-attribute-number (node attribute)
  "Return the number in ATTRIBUTE of NODE, or nil."
  (let ((value (dom-attr node attribute)))
    (and value
         (string-match "\\`[ \t]*\\(-?[0-9]+\\)" value)
         (string-to-number (match-string 1 value)))))

(defun tiles--warc-org-list (node)
  "Insert the list NODE."
  (if tiles--warc-org-inline
      (tiles--warc-org-children node)
    (let ((ordered (eq (dom-tag node) 'ol))
          (number (or (tiles--warc-attribute-number node 'start) 1))
          (space (if tiles--warc-org-in-block 1 2)))
      (tiles--warc-org-newlines space)
      (dolist (child (dom-children node))
        (let ((text (cond
                     ((stringp child)
                      (and (string-match-p "[^ \t\n\r]" child)
                           (tiles--warc-org-render (list child))))
                     ((eq (dom-tag child) 'li)
                      (when-let* ((value (tiles--warc-attribute-number child 'value)))
                        (setq number value))
                      (tiles--warc-org-render (dom-children child)))
                     (t (tiles--warc-org-render (list child))))))
          (when (and text (not (string-empty-p text)))
            (let ((bullet (if ordered (format "%d. " number) "- ")))
              (tiles--warc-org-newlines 1)
              (insert bullet (tiles--warc-org-indent text (length bullet))))
            (setq number (1+ number)))))
      (tiles--warc-org-newlines space))))

(defun tiles--warc-description-items (node)
  "Return the term and description elements of the list NODE."
  (let (items)
    (dolist (child (dom-children node))
      (when (consp child)
        (if (eq (dom-tag child) 'div)
            (dolist (item (tiles--warc-description-items child))
              (push item items))
          (push child items))))
    (nreverse items)))

(defun tiles--warc-org-description-item (terms text)
  "Insert a description list item for TERMS described by TEXT."
  (let ((terms (seq-remove #'string-empty-p terms)))
    (when (or terms (not (string-empty-p text)))
      (tiles--warc-org-newlines 1)
      (insert "- "
              (if terms (concat (string-join terms ", ") " :: ") "")
              (tiles--warc-org-indent text 2)))))

(defun tiles--warc-org-description-list (node)
  "Insert the description list NODE."
  (if tiles--warc-org-inline
      (tiles--warc-org-children node)
    (let ((space (if tiles--warc-org-in-block 1 2))
          terms)
      (tiles--warc-org-newlines space)
      (dolist (item (tiles--warc-description-items node))
        (pcase (dom-tag item)
          ('dt (push (tiles--warc-org-flat (tiles--warc-org-render (dom-children item) t))
                     terms))
          ('dd (tiles--warc-org-description-item
                (reverse terms) (tiles--warc-org-render (dom-children item)))
               (setq terms nil))))
      (when terms
        (tiles--warc-org-description-item (reverse terms) ""))
      (tiles--warc-org-newlines space))))

(defun tiles--warc-table-row (row header)
  "Return (HEADER-P . CELLS) for the table ROW.
HEADER non-nil means the row is in the table head."
  (let ((cells (seq-filter (lambda (child)
                             (and (consp child) (memq (dom-tag child) '(td th))))
                           (dom-children row))))
    (cons (or header
              (and cells (seq-every-p (lambda (cell) (eq (dom-tag cell) 'th))
                                      cells)))
          cells)))

(defun tiles--warc-table-rows (table)
  "Return the rows of TABLE as a list of (HEADER-P . CELLS)."
  (let (rows)
    (dolist (child (dom-children table))
      (when (consp child)
        (pcase (dom-tag child)
          ((or 'thead 'tbody 'tfoot)
           (dolist (row (dom-children child))
             (when (and (consp row) (eq (dom-tag row) 'tr))
               (push (tiles--warc-table-row row (eq (dom-tag child) 'thead)) rows))))
          ('tr (push (tiles--warc-table-row child nil) rows)))))
    (nreverse rows)))

(defun tiles--warc-layout-table-p (rows)
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
                (> (length (tiles--warc-dom-text cell)) 300)))
          (cdr row)))
       rows)))

(defun tiles--warc-org-table (node)
  "Insert the table NODE."
  (let ((rows (tiles--warc-table-rows node)))
    (cond
     (tiles--warc-org-inline
      (tiles--warc-org-children node))
     ((tiles--warc-layout-table-p rows)
      ;; Show the cells of layout tables one after the other.
      (tiles--warc-org-newlines 1)
      (if rows
          (dolist (row rows)
            (dolist (cell (cdr row))
              (tiles--warc-org-block cell 1)))
        (tiles--warc-org-children node))
      (tiles--warc-org-newlines 1))
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
                                    (tiles--warc-org-flat
                                     (tiles--warc-org-render (dom-children cell) t))))
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
            (tiles--warc-org-block caption 2))
          (tiles--warc-org-newlines 2)
          (insert (string-join (nreverse lines) "\n"))
          (tiles--warc-org-newlines 2)))))))

(defun tiles--warc-org-node (node)
  "Insert the Org text of the DOM NODE."
  (cond
   ((stringp node)
    (tiles--warc-org-text node))
   ((consp node)
    (pcase (dom-tag node)
      ((or 'p 'address 'figure 'figcaption 'details 'summary 'center
           'fieldset 'legend 'caption)
       (tiles--warc-org-block node 2))
      ((or 'div 'section 'article 'main 'header 'footer 'nav 'aside 'form
           'hgroup 'dialog 'body 'html 'li 'dt 'dd 'tr 'td 'th 'thead 'tbody
           'tfoot)
       (tiles--warc-org-block node 1))
      ((or 'h1 'h2 'h3 'h4 'h5 'h6)
       (tiles--warc-org-heading
        node (string-to-number (substring (symbol-name (dom-tag node)) 1))))
      ((or 'b 'strong) (tiles--warc-org-emphasis node "*"))
      ((or 'i 'em 'cite 'dfn 'var) (tiles--warc-org-emphasis node "/"))
      ((or 'u 'ins) (tiles--warc-org-emphasis node "_"))
      ((or 's 'strike 'del) (tiles--warc-org-emphasis node "+"))
      ((or 'code 'kbd 'samp 'tt) (tiles--warc-org-verbatim node))
      ('a (tiles--warc-org-link node))
      ('br (tiles--warc-org-break))
      ('hr (tiles--warc-org-rule))
      ('pre (tiles--warc-org-pre node))
      ('blockquote (tiles--warc-org-quote node))
      ((or 'ul 'ol 'menu 'dir) (tiles--warc-org-list node))
      ('dl (tiles--warc-org-description-list node))
      ('table (tiles--warc-org-table node))
      ('q (tiles--warc-org-text "“")
          (tiles--warc-org-children node)
          (tiles--warc-org-text "”"))
      ((or 'head 'title) nil)
      (_ (tiles--warc-org-children node))))))

(defun tiles--warc-page-org (page)
  "Return the Org document for PAGE, as returned by `tiles--warc-page'."
  (require 'ol)
  (let* ((max-lisp-eval-depth (max max-lisp-eval-depth 4000))
         (dom (plist-get page :dom))
         (text (let ((tiles--warc-org-base (plist-get page :base))
                     (tiles--warc-org-inline nil)
                     (tiles--warc-org-in-block nil)
                     (tiles--warc-org-in-quote nil)
                     (tiles--warc-org-in-link nil)
                     (tiles--warc-org-markers nil))
                 (with-temp-buffer
                   (tiles--warc-org-node (or (car (dom-by-tag dom 'body)) dom))
                   (tiles--warc-org-tidy)
                   (buffer-string))))
         (date (plist-get page :date)))
    (concat ":PROPERTIES:\n"
            ":WARC-Target-URI: " (plist-get page :uri) "\n"
            (if date (concat ":WARC-Date: " date "\n") "")
            ":END:\n"
            "#+title: " (tiles--warc-org-flat (plist-get page :title)) "\n\n"
            text "\n")))

;;;; Showing records

(defvar-local tiles--warc-archive nil
  "The archive shown in this buffer.")

(defvar-local tiles--warc-record nil
  "The record shown in this buffer.")

(defun tiles--warc-current-archive ()
  "Return the archive of the current buffer."
  (or (and (derived-mode-p 'tiles-warc-eww-mode)
           (plist-get eww-data :tiles-warc-archive))
      tiles--warc-archive
      (user-error "No WARC archive here")))

(defun tiles--warc-current-record ()
  "Return the record at point or shown in the current buffer."
  (or (cond
       ((derived-mode-p 'tiles-warc-mode) (tabulated-list-get-id))
       ((derived-mode-p 'tiles-warc-eww-mode) (plist-get eww-data :tiles-warc-record))
       (t tiles--warc-record))
      (user-error "No WARC record here")))

(defun tiles--warc-short-uri (uri)
  "Return URI without its scheme, for use in buffer names."
  (truncate-string-to-width (replace-regexp-in-string "\\`[a-z]+://" "" uri)
                            60 nil nil "…"))

(defun tiles--warc-web-url-p (url &rest _)
  "Return non-nil if URL is an HTTP or HTTPS URL."
  (string-match-p "\\`https?:" url))

(defun tiles--warc-browse-url (url &rest _)
  "Follow URL from a buffer showing an archive; for `browse-url-handlers'."
  (tiles--warc-follow-url url))

(defun tiles--warc-browse-live (url)
  "Open URL, which is not in the archive, as `tiles-warc-follow-live-links' says."
  (if (if (eq tiles-warc-follow-live-links 'ask)
          (y-or-n-p (format "%s is not in the archive; open it live? " url))
        tiles-warc-follow-live-links)
      (let ((browse-url-handlers (default-value 'browse-url-handlers))
            (browse-url-browser-function
             (default-value 'browse-url-browser-function)))
        (browse-url url))
    (message "Not in the archive: %s" url)))

(defun tiles--warc-follow-url (url)
  "Show the archived copy of URL, or handle it as a live link."
  (let* ((archive (tiles--warc-current-archive))
         (found (tiles--warc-lookup archive url))
         (target (and found (tiles--warc-resolve archive found)))
         (fragment (url-target (url-generic-parse-url url))))
    (cond
     ((not target)
      (tiles--warc-browse-live url))
     ((derived-mode-p 'tiles-warc-eww-mode)
      (if (and fragment (eq target (plist-get eww-data :tiles-warc-record)))
          (tiles--warc-eww-jump fragment)
        (tiles--warc-show-eww archive target fragment)))
     ((eq target tiles--warc-record)
      (message "The link points to this page"))
     (t (tiles--warc-display archive target 'org)))))

(defun tiles-warc-follow-link ()
  "Follow the link at point, preferring pages in the archive."
  (interactive)
  (cond
   ((derived-mode-p 'org-mode) (org-open-at-point))
   ((thing-at-point 'url) (tiles--warc-follow-url (thing-at-point 'url)))
   (t (user-error "No link at point"))))

(defvar-keymap tiles-warc-view-mode-map
  :doc "Keymap of `tiles-warc-view-mode'."
  "q" #'quit-window
  "^" #'tiles-warc-list-records
  "R" #'tiles-warc-toggle-readable
  "RET" #'tiles-warc-follow-link
  "o" #'tiles-warc-show-org
  "e" #'tiles-warc-show-eww
  "v" #'tiles-warc-view-source
  "s" #'tiles-warc-save-payload)

(define-minor-mode tiles-warc-view-mode
  "Minor mode of buffers showing a record of a WARC archive.

\\{tiles-warc-view-mode-map}"
  :lighter " WARC"
  :keymap tiles-warc-view-mode-map)

(defun tiles--warc-setup-view (archive record)
  "Set up the current buffer to show RECORD of ARCHIVE."
  (setq tiles--warc-archive archive
        tiles--warc-record record
        buffer-read-only t
        buffer-undo-list t)
  (setq-local browse-url-handlers
              (cons (cons #'tiles--warc-web-url-p #'tiles--warc-browse-url)
                    browse-url-handlers))
  (tiles-warc-view-mode 1)
  (set-buffer-modified-p nil)
  (goto-char (point-min)))

(defun tiles--warc-record-buffer (record)
  "Return the buffer for showing RECORD."
  (get-buffer-create
   (format "*tiles-warc: %s*" (tiles--warc-short-uri (or (tiles-warc-record-uri record) "")))))

(defvar-local tiles--warc-readable 'default
  "Whether the Org view in this buffer shows only the main content of pages.
`default' means as `tiles-warc-readable' says, until the view is toggled.")
;; Rendering sets `org-mode' again, which must not reset it.
(put 'tiles--warc-readable 'permanent-local t)

(defvar-local tiles--warc-main-content-shown nil
  "Non-nil when the Org view in this buffer shows only the main content.")

(defun tiles--warc-readable-p ()
  "Return non-nil if the Org view in this buffer shows main content only."
  (if (eq tiles--warc-readable 'default) tiles-warc-readable tiles--warc-readable))

(defun tiles-warc-toggle-readable ()
  "Switch the page shown between its main content and the whole page.
The main content is the title and text of the article, without the
menus, headers, footers and sidebars around it (see `tiles-warc-readable')."
  (interactive)
  (unless (and tiles--warc-record (derived-mode-p 'org-mode))
    (user-error "Only pages shown as Org have a main content view"))
  (setq tiles--warc-readable (not (tiles--warc-readable-p)))
  (tiles--warc-render-org (tiles--warc-current-archive) tiles--warc-record)
  (message (cond (tiles--warc-main-content-shown "Showing the main content of the page")
                 (tiles--warc-readable "No main content found: showing the whole page")
                 (t "Showing the whole page"))))

(defun tiles--warc-render-org (archive record)
  "Show the HTML page in RECORD of ARCHIVE as Org in the current buffer."
  (require 'org)
  (require 'org-table)
  (let* ((page (tiles--warc-page archive record (tiles--warc-readable-p)))
         (text (tiles--warc-page-org page))
         (inhibit-read-only t))
    (erase-buffer)
    (insert text)
    (let ((org-startup-folded 'showeverything)
          (org-startup-with-inline-images nil)
          (org-startup-with-latex-preview nil))
      (org-mode))
    (setq tiles--warc-main-content-shown (plist-get page :readable))
    (with-demoted-errors "Aligning tables: %S"
      (org-table-map-tables #'org-table-align t)))
  (visual-line-mode 1)
  (tiles--warc-setup-view archive record))

(defun tiles--warc-show-org (archive record)
  "Show the HTML page in RECORD of ARCHIVE as an Org document."
  (let ((buffer (tiles--warc-record-buffer record)))
    (with-current-buffer buffer
      (tiles--warc-render-org archive record))
    (pop-to-buffer-same-window buffer)))

(defun tiles--warc-text-mode (mime)
  "Return the major mode for showing text of the media type MIME."
  (cond
   ((string-match-p "css" mime) #'css-mode)
   ((string-match-p "json" mime)
    (if (fboundp 'js-json-mode) #'js-json-mode #'js-mode))
   ((string-match-p "javascript\\|ecmascript" mime) #'js-mode)
   ((string-match-p "xml" mime) #'nxml-mode)
   ((string-prefix-p "text/plain" mime) #'text-mode)
   (t #'fundamental-mode)))

(defun tiles--warc-render-text (archive record)
  "Show the text payload of RECORD in ARCHIVE in the current buffer."
  (let ((text (tiles--warc-payload-text archive record))
        (inhibit-read-only t))
    (erase-buffer)
    (insert text)
    (funcall (tiles--warc-text-mode (tiles-warc-record-mime record))))
  (tiles--warc-setup-view archive record))

(defun tiles--warc-show-text (archive record)
  "Show the text payload of RECORD in ARCHIVE."
  (let ((buffer (tiles--warc-record-buffer record)))
    (with-current-buffer buffer
      (tiles--warc-render-text archive record))
    (pop-to-buffer-same-window buffer)))

(defun tiles--warc-text-mime-p (mime)
  "Return non-nil if the media type MIME is text."
  (or (string-prefix-p "text/" mime)
      (string-match-p "json\\|xml\\|javascript\\|ecmascript\\|warc-fields\\|application/http\\|x-www-form-urlencoded"
                      mime)))

(defun tiles--warc-media-mime-p (mime)
  "Return non-nil if the media type MIME is an image, audio or video."
  (string-match-p "\\`\\(?:image\\|audio\\|video\\)/" mime))

(defun tiles--warc-showable-p (archive record)
  "Return non-nil if RECORD of ARCHIVE can be shown as a page or as text."
  (and (member (tiles-warc-record-type record) '("response" "resource" "conversion"))
       (ignore-errors
         (or (tiles--warc-html-p archive record)
             (let ((mime (tiles-warc-record-mime record)))
               (and (tiles--warc-text-mime-p mime)
                    (not (tiles--warc-media-mime-p mime))))))))

(defun tiles--warc-page-kind (record)
  "Return what RECORD is when looking for the page of an archive.
Return `page' for successful HTML pages, `error' for HTML error pages,
`text' for other text and nil for anything else.  Only the content
type is looked at."
  (let ((mime (tiles-warc-record-mime record))
        (status (tiles-warc-record-http-status record)))
    (cond
     ((not (member (tiles-warc-record-type record) '("response" "resource" "conversion")))
      nil)
     ((or (member mime '("text/html" "application/xhtml+xml"))
          (tiles--warc-redirect-location record))
      (if (and status (>= status 400)) 'error 'page))
     ((and (tiles--warc-text-mime-p mime) (not (tiles--warc-media-mime-p mime)))
      'text))))

(defun tiles--warc-single-page (archive)
  "Return the record of the only page in ARCHIVE, or nil.
The pages are the HTML pages that listed records lead to: redirects
and revisits count as the page they lead to, while the images,
styles and scripts that come with a page do not count.  Error pages
count when the archive has no other page, and other text when it has
no page at all.  Return nil if the page found cannot be shown."
  (let ((found (list (cons 'page nil) (cons 'error nil) (cons 'text nil))))
    ;; FOUND maps each kind to nil, a record, or `several'.
    (catch 'several
      (seq-doseq (record (tiles-warc-archive-records archive))
        (when (member (tiles-warc-record-type record) tiles-warc-record-types)
          (let* ((target (tiles--warc-resolve archive record))
                 (slot (assq (tiles--warc-page-kind target) found)))
            (when slot
              (cond
               ((not (cdr slot)) (setcdr slot target))
               ((not (eq (cdr slot) target))
                (setcdr slot 'several)
                (when (eq (car slot) 'page)
                  (throw 'several nil)))))))))
    (let ((page (seq-some (lambda (slot) (cdr slot)) found)))
      (and (tiles-warc-record-p page)
           (tiles--warc-showable-p archive page)
           page))))

(defun tiles--warc-render (archive record view)
  "Show RECORD of ARCHIVE in the current buffer, HTML pages as VIEW.
RECORD must satisfy `tiles--warc-showable-p'."
  (cond
   ((not (tiles--warc-html-p archive record))
    (tiles--warc-render-text archive record))
   ((eq view 'eww)
    (tiles--warc-require-eww)
    (tiles-warc-eww-mode)
    (tiles--warc-eww-render archive record nil))
   (t (tiles--warc-render-org archive record))))

(defun tiles--warc-display (archive record view)
  "Show RECORD of ARCHIVE; HTML pages are shown as VIEW, org or eww."
  (let* ((target (tiles--warc-resolve archive record))
         (type (tiles-warc-record-type target))
         (mime (tiles-warc-record-mime target)))
    (cond
     ((not (member type '("response" "resource" "revisit" "conversion")))
      (tiles-warc-view-source archive target))
     ((equal type "revisit")
      (user-error "The record this revisit refers to is not in the archive"))
     ((tiles--warc-html-p archive target)
      (if (eq view 'eww)
          (tiles--warc-show-eww archive target)
        (tiles--warc-show-org archive target)))
     ((tiles--warc-media-mime-p mime)
      (user-error "Not showing %s content; press `s' to save it" mime))
     ((tiles--warc-text-mime-p mime)
      (tiles--warc-show-text archive target))
     (t
      (user-error "Not a text payload (%s); press `s' to save it"
                  (if (string-empty-p mime) "unknown type" mime))))
    (unless (eq target record)
      (message "Showing %s" (or (tiles-warc-record-uri target) "the original record")))))

(defun tiles-warc-open-record ()
  "Show the record at point, with `tiles-warc-default-view' for HTML pages."
  (interactive)
  (tiles--warc-display (tiles--warc-current-archive) (tiles--warc-current-record)
                   tiles-warc-default-view))

(defun tiles-warc-show-org ()
  "Show the record at point, or of this buffer, as an Org document."
  (interactive)
  (tiles--warc-display (tiles--warc-current-archive) (tiles--warc-current-record) 'org))

(defun tiles-warc-show-eww ()
  "Show the record at point, or of this buffer, with EWW."
  (interactive)
  (tiles--warc-display (tiles--warc-current-archive) (tiles--warc-current-record) 'eww))

(defun tiles-warc-view-source (&optional archive record)
  "Show the headers and payload of RECORD of ARCHIVE as stored.
Interactively, show the record at point or of the current buffer."
  (interactive)
  (let* ((archive (or archive (tiles--warc-current-archive)))
         (record (or record (tiles--warc-current-record)))
         (mime (tiles-warc-record-mime record))
         (buffer (get-buffer-create
                  (format "*tiles-warc source: %s*"
                          (tiles--warc-short-uri
                           (or (tiles-warc-record-uri record)
                               (format "record %d" (tiles-warc-record-index record))))))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (fundamental-mode)
        (dolist (field (tiles-warc-record-headers record))
          (insert (propertize (car field) 'face 'bold) ": " (cdr field) "\n"))
        (insert "\n")
        (if (tiles-warc-record-http-status record)
            (pcase-let ((`(,start ,headers ,_)
                         (tiles--warc-split-http (tiles--warc-record-block archive record))))
              (insert (propertize start 'face 'bold) "\n")
              (dolist (field headers)
                (insert (propertize (car field) 'face 'bold) ": " (cdr field) "\n"))
              (insert "\n")
              (if (and (tiles--warc-text-mime-p mime)
                       (not (string-match-p "\\`\\(?:image\\|audio\\|video\\)/" mime)))
                  (insert (condition-case err
                              (tiles--warc-payload-text archive record)
                            (user-error (format "[%s]" (error-message-string err)))))
                (insert (format "[%s of %s data]"
                                (file-size-human-readable (tiles--warc-payload-size record))
                                (if (string-empty-p mime) "unknown" mime)))))
          (if (or (tiles--warc-text-mime-p mime) (string-empty-p mime))
              (insert (decode-coding-string (tiles--warc-record-block archive record)
                                            (tiles--warc-charset
                                             (tiles-warc-record-content-type record) "")))
            (insert (format "[%s of %s data]"
                            (file-size-human-readable (tiles-warc-record-length record))
                            mime)))))
      (tiles--warc-setup-view archive record))
    (pop-to-buffer-same-window buffer)))

(defun tiles--warc-payload-file-name (record)
  "Return a file name for saving the payload of RECORD."
  (let* ((uri (or (tiles-warc-record-uri record) ""))
         (path (car (url-path-and-query (url-generic-parse-url uri))))
         (name (and path (file-name-nondirectory (url-unhex-string path)))))
    (if (or (not name) (string-empty-p name))
        (if (equal (tiles-warc-record-mime record) "text/html") "index.html" "payload")
      (string-replace "/" "_" name))))

(defun tiles-warc-save-payload (archive record file)
  "Save the payload of RECORD in ARCHIVE to FILE.
Interactively, save the record at point or of the current buffer."
  (interactive
   (let ((archive (tiles--warc-current-archive))
         (record (tiles--warc-current-record)))
     (list archive record
           (read-file-name "Save payload to: " nil nil nil
                           (tiles--warc-payload-file-name record)))))
  (setq file (expand-file-name file))
  (when (file-directory-p file)
    (setq file (expand-file-name (tiles--warc-payload-file-name record) file)))
  (when (and (file-exists-p file)
             (not (y-or-n-p (format "Overwrite %s? " file))))
    (user-error "Payload not saved"))
  (let ((payload (tiles--warc-record-payload archive
                                         (tiles--warc-resolve archive record t))))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert payload)
      (let ((coding-system-for-write 'no-conversion))
        (write-region nil nil file)))
    (message "Saved %s (%s)" file (file-size-human-readable (length payload)))))

;;;; EWW

(defun tiles--warc-require-eww ()
  "Load EWW, before `tiles-warc-eww-mode' is first used.
TILES does not load it otherwise: the Org view does not need it."
  (require 'eww))

(defvar-keymap tiles-warc-eww-mode-map
  :doc "Keymap of `tiles-warc-eww-mode'."
  "<remap> <eww-follow-link>" #'tiles-warc-eww-follow-link
  "<remap> <eww-reload>" #'tiles-warc-eww-reload
  "<remap> <eww-view-source>" #'tiles-warc-view-source
  "^" #'tiles-warc-list-records
  "o" #'tiles-warc-show-org)

(define-derived-mode tiles-warc-eww-mode eww-mode "WARC"
  "Major mode for archived pages rendered by EWW.
Links to pages in the archive show the archived page; reloading
shows the page from the archive again.

\\{tiles-warc-eww-mode-map}"
  :interactive nil
  (setq-local desktop-save-buffer nil))

(defun tiles--warc-eww-render (archive record fragment)
  "Render the HTML page in RECORD of ARCHIVE in the current buffer.
Move to the element named FRAGMENT, if any."
  (let ((page (tiles--warc-page archive record)))
    (setq eww-data (list :title (plist-get page :title)
                         :url (plist-get page :uri)
                         ;; `eww-readable' parses this again.
                         :source (with-temp-buffer
                                   (dom-print (plist-get page :dom))
                                   (buffer-string))
                         :tiles-warc-archive archive
                         :tiles-warc-record record))
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

(defun tiles--warc-show-eww (archive record &optional fragment)
  "Show the HTML page in RECORD of ARCHIVE with EWW.
Move to the element named FRAGMENT, if any.  Pages shown from an EWW
buffer replace its page, so that they are in its history."
  (tiles--warc-require-eww)
  (let ((buffer (if (derived-mode-p 'tiles-warc-eww-mode)
                    (current-buffer)
                  (get-buffer-create "*tiles-warc-eww*"))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'tiles-warc-eww-mode)
        (tiles-warc-eww-mode))
      (when (plist-get eww-data :url)
        (eww-save-history))
      (when (fboundp 'eww--before-browse)
        (eww--before-browse))
      (setq eww-history-position 0)
      (tiles--warc-eww-render archive record fragment))
    (pop-to-buffer-same-window buffer)))

(defun tiles--warc-eww-jump (target)
  "Move to the element named TARGET on the current EWW page."
  (let ((match (save-excursion
                 (goto-char (point-min))
                 (text-property-search-forward 'shr-target-id target #'member))))
    (if match
        (goto-char (prop-match-beginning match))
      (message "No element named %s on this page" target))))

(defun tiles-warc-eww-follow-link (&optional external mouse-event)
  "Follow the link at point, or at MOUSE-EVENT, preferring the archive.
With a \\[universal-argument] prefix EXTERNAL, open the live page with
an external browser."
  (interactive (list current-prefix-arg last-nonmenu-event) tiles-warc-eww-mode)
  (when (mouse-event-p mouse-event)
    (mouse-set-point mouse-event))
  (let ((url (get-text-property (point) 'shr-url)))
    (cond
     ((not url) (message "No link under point"))
     (external (eww-browse-with-external-browser url))
     (t (tiles--warc-follow-url url)))))

(defun tiles-warc-eww-reload ()
  "Show the current page from the archive again."
  (interactive nil tiles-warc-eww-mode)
  (let ((point (point)))
    (tiles--warc-eww-render (plist-get eww-data :tiles-warc-archive)
                        (plist-get eww-data :tiles-warc-record)
                        nil)
    (goto-char (min point (point-max)))))

;;;; Record list

(defvar-local tiles--warc-show-all nil
  "Non-nil means list records of all types.")

(defvar-local tiles--warc-filter nil
  "Regexp that listed URIs or content types must match, or nil.")

(defvar-keymap tiles-warc-mode-map
  :doc "Keymap of `tiles-warc-mode'."
  :parent tabulated-list-mode-map
  "RET" #'tiles-warc-open-record
  "o" #'tiles-warc-show-org
  "e" #'tiles-warc-show-eww
  "v" #'tiles-warc-view-source
  "s" #'tiles-warc-save-payload
  "a" #'tiles-warc-toggle-all-records
  "/" #'tiles-warc-filter)

(defun tiles--warc-sort-by-index (a b)
  "Return non-nil if the record of entry A comes before that of entry B."
  (< (tiles-warc-record-index (car a)) (tiles-warc-record-index (car b))))

(defun tiles--warc-sort-by-size (a b)
  "Return non-nil if the payload of entry A is smaller than that of entry B."
  (< (tiles--warc-payload-size (car a)) (tiles--warc-payload-size (car b))))

(define-derived-mode tiles-warc-mode tabulated-list-mode "WARC"
  "Major mode for listing the records of a WARC file.

\\{tiles-warc-mode-map}"
  (setq tabulated-list-format
        [("#" 5 tiles--warc-sort-by-index :right-align t)
         ("Type" 8 t)
         ("Status" 6 t :right-align t)
         ("Content type" 24 t)
         ("Size" 7 tiles--warc-sort-by-size :right-align t)
         ("Date" 19 t)
         ("URI" 40 t)])
  (setq tabulated-list-padding 1)
  (setq-local revert-buffer-function #'tiles--warc-revert)
  (tabulated-list-init-header)
  (when buffer-file-name
    (tiles--warc-take-over-visited-buffer)))

(defun tiles--warc-forget-visited-file ()
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

(defun tiles--warc-take-over-visited-buffer ()
  "Replace the contents of the WARC file visited here with its records."
  (let* ((archive (tiles-warc-read-archive buffer-file-name))
         (file (tiles--warc-forget-visited-file)))
    (setq-local list-buffers-directory file)
    (setq tiles--warc-archive archive)
    (tiles--warc-refresh)))

(defun tiles--warc-list-archive (archive)
  "List the records of ARCHIVE in the current buffer."
  (tiles-warc-mode)
  (setq tiles--warc-archive archive)
  (tiles--warc-refresh))

(defun tiles-warc-visit ()
  "Show the WARC file visited in the current buffer.
If the archive holds a single page, show it as `tiles-warc-default-view'
says; otherwise list the records of the archive.  This function is
meant for `auto-mode-alist', where it is while `tiles-archive-urls' is
on.  New and empty files are left alone."
  (if (or (not (file-exists-p buffer-file-name))
          (zerop (file-attribute-size (file-attributes buffer-file-name))))
      (fundamental-mode)
    (tiles--warc-visit-archive)))

(defun tiles--warc-visit-archive ()
  "Show the WARC file visited in the current buffer, as `tiles-warc-visit' does."
  (let* ((archive (tiles-warc-read-archive buffer-file-name))
         (page (tiles--warc-single-page archive))
         (file (tiles--warc-forget-visited-file)))
    (if (not page)
        (tiles--warc-list-archive archive)
      (condition-case err
          (tiles--warc-render archive page tiles-warc-default-view)
        (error
         (message "Cannot show %s: %s" (tiles-warc-record-uri page)
                  (error-message-string err))
         (tiles--warc-list-archive archive))))
    ;; The buffer shows archived web content: local variables in it
    ;; must not take effect.
    (setq-local local-enable-local-variables nil)
    (setq-local list-buffers-directory file)))

(defun tiles--warc-status-face (status)
  "Return the face for the HTTP STATUS."
  (cond ((>= status 400) 'error)
        ((>= status 300) 'shadow)))

(defun tiles--warc-listed-p (record)
  "Return non-nil if RECORD belongs in the list."
  (and (or tiles--warc-show-all
           (member (tiles-warc-record-type record) tiles-warc-record-types))
       (or (not tiles--warc-filter)
           (let ((case-fold-search t))
             (or (string-match-p tiles--warc-filter (or (tiles-warc-record-uri record) ""))
                 (string-match-p tiles--warc-filter (tiles-warc-record-mime record)))))))

(defun tiles--warc-entry (record)
  "Return the list entry of RECORD."
  (let ((status (tiles-warc-record-http-status record))
        (date (or (tiles-warc-record-header record "WARC-Date") "")))
    (list record
          (vector (number-to-string (tiles-warc-record-index record))
                  (tiles-warc-record-type record)
                  (if status
                      (propertize (number-to-string status)
                                  'face (tiles--warc-status-face status))
                    "")
                  (tiles-warc-record-mime record)
                  (file-size-human-readable (tiles--warc-payload-size record))
                  (if (>= (length date) 19)
                      (concat (substring date 0 10) " " (substring date 11 19))
                    date)
                  (or (tiles-warc-record-uri record)
                      (tiles-warc-record-header record "WARC-Filename")
                      "")))))

(defun tiles--warc-refresh ()
  "List the records of the archive of the current buffer."
  (let ((records (tiles-warc-archive-records tiles--warc-archive))
        entries)
    (seq-doseq (record records)
      (when (tiles--warc-listed-p record)
        (push (tiles--warc-entry record) entries)))
    (setq tabulated-list-entries (nreverse entries))
    (setq mode-line-process
          (concat (format " %d/%d" (length tabulated-list-entries) (length records))
                  (and tiles--warc-filter (format " /%s/" tiles--warc-filter))))
    (tabulated-list-print t)
    (when (and (null tabulated-list-entries) (> (length records) 0))
      (message (substitute-command-keys
                "\\<tiles-warc-mode-map>No records to list; \\[tiles-warc-toggle-all-records] lists all records")))))

(defun tiles--warc-revert (&rest _)
  "Read the archive of the current buffer again and list its records."
  (setq tiles--warc-archive (tiles-warc-read-archive (tiles-warc-archive-file tiles--warc-archive)))
  (tiles--warc-refresh))

(defun tiles-warc-toggle-all-records ()
  "Toggle between listing all records and those in `tiles-warc-record-types'."
  (interactive nil tiles-warc-mode)
  (setq tiles--warc-show-all (not tiles--warc-show-all))
  (tiles--warc-refresh))

(defun tiles-warc-filter (regexp)
  "Only list records whose URI or content type matches REGEXP.
An empty REGEXP lists all records again."
  (interactive (list (read-regexp "List records matching (empty for all)"))
               tiles-warc-mode)
  (setq tiles--warc-filter (and regexp (not (string-empty-p regexp)) regexp))
  (tiles--warc-refresh))

(defun tiles--warc-listing-buffer (file)
  "Return the buffer that lists the records of FILE, or nil."
  (seq-find (lambda (buffer)
              (with-current-buffer buffer
                (and (derived-mode-p 'tiles-warc-mode)
                     tiles--warc-archive
                     (equal (tiles-warc-archive-file tiles--warc-archive) file))))
            (buffer-list)))

(defun tiles--warc-list-buffer (archive)
  "Return the buffer that lists the records of ARCHIVE, creating it if needed."
  (let ((file (tiles-warc-archive-file archive)))
    (or (tiles--warc-listing-buffer file)
        (let ((buffer (generate-new-buffer
                       (format "*tiles-warc %s*" (file-name-nondirectory file)))))
          (with-current-buffer buffer
            (tiles--warc-list-archive archive)
            (setq default-directory (file-name-directory file)))
          buffer))))

(defun tiles--warc-goto-uri (uri)
  "Move to the last listed page of URI, or to the first record."
  (let ((target (tiles--warc-normalize-uri uri))
        found)
    (goto-char (point-max))
    (while (and (not found) (zerop (forward-line -1)))
      (let ((record (tabulated-list-get-id)))
        (setq found (and record
                         (member (tiles-warc-record-type record)
                                 '("response" "resource" "conversion"))
                         (equal (tiles--warc-normalize-uri (or (tiles-warc-record-uri record) ""))
                                target)))))
    (unless found
      (goto-char (point-min)))))

(defun tiles--warc-goto-record (record)
  "Move to the listed RECORD, or to the last listed page of its URI."
  (goto-char (point-min))
  (while (and (not (eobp)) (not (eq (tabulated-list-get-id) record)))
    (forward-line 1))
  (when (eobp)
    (tiles--warc-goto-uri (or (tiles-warc-record-uri record) ""))))

(defun tiles-warc-list-records ()
  "List the records of the archive shown in the current buffer."
  (interactive)
  (let ((archive (tiles--warc-current-archive))
        (record (if (derived-mode-p 'tiles-warc-eww-mode)
                    (plist-get eww-data :tiles-warc-record)
                  tiles--warc-record)))
    (pop-to-buffer-same-window (tiles--warc-list-buffer archive))
    (when record
      (tiles--warc-goto-record record))))

;;;###autoload
(defun tiles-warc-open (file &optional list)
  "Show the WARC FILE.
If the archive holds a single page, show it as `tiles-warc-default-view'
says.  Otherwise, or if LIST is non-nil, list the records of the
archive.  Interactively, LIST is the prefix argument."
  (interactive
   (list (read-file-name "WARC file: " nil nil t
                         (and (derived-mode-p 'dired-mode)
                              (dired-get-filename 'no-dir t)))
         current-prefix-arg))
  (let* ((file (expand-file-name file))
         (listing (tiles--warc-listing-buffer file))
         (archive (if listing
                      (buffer-local-value 'tiles--warc-archive listing)
                    (tiles-warc-read-archive file)))
         (page (and (not list) (tiles--warc-single-page archive))))
    (if (not page)
        (pop-to-buffer-same-window (tiles--warc-list-buffer archive))
      (tiles--warc-display archive page tiles-warc-default-view)
      (message (substitute-command-keys
                (if tiles--warc-main-content-shown
                    "\\<tiles-warc-view-mode-map>Main content of the page: \\[tiles-warc-toggle-readable] shows the whole page, \\[tiles-warc-list-records] lists the records"
                  "\\<tiles-warc-view-mode-map>Single page archive: \\[tiles-warc-list-records] lists its records"))))))

;;;; Compressing

;; Emacs can inflate gzip data but has no way to deflate it, so this is
;; a small DEFLATE encoder (RFC 1951).  Repeated strings are found with
;; hash chains, and each block of the result is written with fixed or
;; dynamic Huffman codes, or stored, whichever is smallest.

(defconst tiles--warc-length-bases
  [3 4 5 6 7 8 9 10 11 13 15 17 19 23 27 31 35 43 51 59 67 83 99 115 131
   163 195 227 258]
  "Smallest match length of each length code, from code 257.")

(defconst tiles--warc-length-extra
  [0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0]
  "Number of extra bits of each length code, from code 257.")

(defconst tiles--warc-distance-bases
  [1 2 3 4 5 7 9 13 17 25 33 49 65 97 129 193 257 385 513 769 1025 1537
   2049 3073 4097 6145 8193 12289 16385 24577]
  "Smallest distance of each distance code.")

(defconst tiles--warc-distance-extra
  [0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13]
  "Number of extra bits of each distance code.")

(defconst tiles--warc-code-length-order
  [16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15]
  "Order in which the lengths of the code length code are written.")

(defconst tiles--warc-lz-chain 32
  "Number of earlier occurrences examined to find a match.")

(defconst tiles--warc-lz-nice 128
  "Length of a match good enough to stop looking for a longer one.")

(defconst tiles--warc-lz-insert 32
  "Length of the longest match whose positions are all remembered.")

(defconst tiles--warc-block-symbols 16384
  "Number of symbols in a compressed block.")

(defvar tiles--warc-length-codes nil
  "Vector of the length code (from 0 for 257) of each match length.")

(defvar tiles--warc-distance-codes nil
  "Vector of the distance code of each distance.")

(defvar tiles--warc-fixed-codes nil
  "Fixed Huffman codes, as (LENGTHS CODES DISTANCE-LENGTHS DISTANCE-CODES).")

(defvar tiles--warc-crc-table nil
  "Table used to compute CRC-32 checksums.")

(defvar tiles--warc-bits 0
  "Bits not yet written, the first ones lowest.")

(defvar tiles--warc-bit-count 0
  "Number of bits in `tiles--warc-bits'.")

(defun tiles--warc-reverse-bits (code length)
  "Return the LENGTH lowest bits of CODE in reverse order."
  (let ((result 0))
    (dotimes (_ length)
      (setq result (logior (ash result 1) (logand code 1))
            code (ash code -1)))
    result))

(defun tiles--warc-huffman-codes (lengths)
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
          (aset codes symbol (tiles--warc-reverse-bits (aref next length) length))
          (cl-incf (aref next length)))))
    codes))

(defun tiles--warc-tree-depths (freqs symbols)
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

(defun tiles--warc-huffman-lengths (freqs limit)
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
    (while (> (seq-max (setq lengths (tiles--warc-tree-depths freqs symbols))) limit)
      ;; Flatten the tree by making rare symbols less rare.
      (dolist (symbol symbols)
        (aset freqs symbol (max 1 (ash (aref freqs symbol) -1)))))
    lengths))

(defun tiles--warc-deflate-tables ()
  "Make the tables used to compress data, unless they exist."
  (unless tiles--warc-crc-table
    (let ((length-codes (make-vector 259 0))
          (distance-codes (make-vector 32769 0))
          (fixed (make-vector 288 8))
          (crc-table (make-vector 256 0)))
      (dotimes (code 29)
        (let ((base (aref tiles--warc-length-bases code)))
          (dotimes (i (ash 1 (aref tiles--warc-length-extra code)))
            (when (<= (+ base i) 258)
              (aset length-codes (+ base i) code)))))
      ;; 258 has a code of its own, though it is also 227 plus 31.
      (aset length-codes 258 28)
      (dotimes (code 30)
        (let ((base (aref tiles--warc-distance-bases code)))
          (dotimes (i (ash 1 (aref tiles--warc-distance-extra code)))
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
      (setq tiles--warc-length-codes length-codes
            tiles--warc-distance-codes distance-codes
            tiles--warc-fixed-codes (let ((distances (make-vector 30 5)))
                                  (list fixed (tiles--warc-huffman-codes fixed)
                                        distances (tiles--warc-huffman-codes distances)))
            tiles--warc-crc-table crc-table))))

(defun tiles--warc-crc32 (data &optional crc)
  "Return the CRC-32 of the unibyte string DATA.
CRC is the CRC-32 of data that came before DATA, if any."
  (let ((table tiles--warc-crc-table)
        (crc (logxor (or crc 0) #xffffffff)))
    (dotimes (i (length data))
      (setq crc (logxor (aref table (logand (logxor crc (aref data i)) 255))
                        (ash crc -8))))
    (logxor crc #xffffffff)))

(defsubst tiles--warc-lz-hash (data i)
  "Return the hash of the three bytes at I in DATA."
  (logand (logxor (ash (aref data i) 10) (ash (aref data (1+ i)) 5)
                  (aref data (+ i 2)))
          32767))

(defun tiles--warc-lz77 (data)
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
          (let* ((hash (tiles--warc-lz-hash data i))
                 (candidate (aref head hash))
                 (oldest (- i 32768))
                 (longest (min 258 (- size i)))
                 (chain tiles--warc-lz-chain))
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
              (if (>= best (min longest tiles--warc-lz-nice))
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
          (when (<= best tiles--warc-lz-insert)
            (let ((j (1+ i))
                  (end (min (+ i best) (- size 2))))
              (while (< j end)
                (let ((hash (tiles--warc-lz-hash data j)))
                  (aset prev (logand j 32767) (aref head hash))
                  (aset head hash j))
                (setq j (1+ j)))))
          (setq i (+ i best))))
      (setq count (1+ count)))
    (cons symbols count)))

(defsubst tiles--warc-put-bits (value count)
  "Write the COUNT lowest bits of VALUE at point, lowest first."
  (setq tiles--warc-bits (logior tiles--warc-bits (ash value tiles--warc-bit-count))
        tiles--warc-bit-count (+ tiles--warc-bit-count count))
  (while (>= tiles--warc-bit-count 8)
    (insert (logand tiles--warc-bits 255))
    (setq tiles--warc-bits (ash tiles--warc-bits -8)
          tiles--warc-bit-count (- tiles--warc-bit-count 8))))

(defun tiles--warc-align-bits ()
  "Write the bits not yet written, padded to a whole byte."
  (when (> tiles--warc-bit-count 0)
    (insert (logand tiles--warc-bits 255)))
  (setq tiles--warc-bits 0
        tiles--warc-bit-count 0))

(defun tiles--warc-write-stored (data start end final)
  "Write the bytes of DATA from START to END as stored blocks.
FINAL non-nil means the last of these blocks ends the data."
  (while (progn
           (let* ((stop (min end (+ start 65535)))
                  (size (- stop start)))
             (tiles--warc-put-bits (if (and final (= stop end)) 1 0) 1)
             (tiles--warc-put-bits 0 2)
             (tiles--warc-align-bits)
             (insert (logand size 255) (ash size -8)
                     (logand (lognot size) 255) (logand (ash (lognot size) -8) 255))
             (insert (substring data start stop))
             (setq start stop))
           (< start end))))

(defun tiles--warc-length-runs (lengths)
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

(defun tiles--warc-write-huffman (data symbols from to start end final)
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
                  (let ((length-code (aref tiles--warc-length-codes (ash symbol -16)))
                        (distance-code (aref tiles--warc-distance-codes
                                             (logand symbol #xffff))))
                    (cl-incf (aref freqs (+ 257 length-code)))
                    (cl-incf (aref distance-freqs distance-code))
                    (setq extra (+ extra (aref tiles--warc-length-extra length-code)
                                   (aref tiles--warc-distance-extra distance-code))))))
    (aset freqs 256 1)
    (let* ((lengths (tiles--warc-huffman-lengths freqs 15))
           (distance-lengths (tiles--warc-huffman-lengths distance-freqs 15))
           (lit-count (max 257 (1+ (cl-position-if #'cl-plusp lengths :from-end t))))
           (distance-count (1+ (cl-position-if #'cl-plusp distance-lengths :from-end t)))
           (runs (tiles--warc-length-runs
                  (vconcat (seq-take lengths lit-count)
                           (seq-take distance-lengths distance-count))))
           (run-freqs (make-vector 19 0))
           (run-lengths (progn
                          (dolist (run runs)
                            (cl-incf (aref run-freqs (car run))))
                          (tiles--warc-huffman-lengths run-freqs 7)))
           (run-count (max 4 (1+ (cl-position-if
                                  (lambda (symbol) (> (aref run-lengths symbol) 0))
                                  tiles--warc-code-length-order :from-end t))))
           (fixed (nth 0 tiles--warc-fixed-codes))
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
           (fixed-cost (+ 3 (funcall cost fixed (nth 2 tiles--warc-fixed-codes))))
           (stored-cost (+ (* 8 (- end start))
                           (* 42 (max 1 (ceiling (- end start) 65535.0))))))
      (cond
       ((and (<= stored-cost fixed-cost) (<= stored-cost dynamic-cost))
        (tiles--warc-write-stored data start end final))
       ((<= fixed-cost dynamic-cost)
        (tiles--warc-put-bits (if final 1 0) 1)
        (tiles--warc-put-bits 1 2)
        (tiles--warc-write-symbols symbols from to fixed (nth 1 tiles--warc-fixed-codes)
                               (nth 2 tiles--warc-fixed-codes) (nth 3 tiles--warc-fixed-codes)))
       (t
        (let ((run-codes (tiles--warc-huffman-codes run-lengths)))
          (tiles--warc-put-bits (if final 1 0) 1)
          (tiles--warc-put-bits 2 2)
          (tiles--warc-put-bits (- lit-count 257) 5)
          (tiles--warc-put-bits (1- distance-count) 5)
          (tiles--warc-put-bits (- run-count 4) 4)
          (dotimes (i run-count)
            (tiles--warc-put-bits (aref run-lengths (aref tiles--warc-code-length-order i)) 3))
          (dolist (run runs)
            (let ((symbol (car run)))
              (tiles--warc-put-bits (aref run-codes symbol) (aref run-lengths symbol))
              (pcase symbol
                (16 (tiles--warc-put-bits (cdr run) 2))
                (17 (tiles--warc-put-bits (cdr run) 3))
                (18 (tiles--warc-put-bits (cdr run) 7)))))
          (tiles--warc-write-symbols symbols from to
                                 lengths (tiles--warc-huffman-codes lengths)
                                 distance-lengths
                                 (tiles--warc-huffman-codes distance-lengths))))))))

(defun tiles--warc-write-symbols (symbols from to lengths codes distance-lengths
                                      distance-codes)
  "Write SYMBOLS FROM to TO and an end of block with the given codes.
LENGTHS and CODES are the literal and length codes, DISTANCE-LENGTHS
and DISTANCE-CODES the distance codes."
  (let ((length-codes tiles--warc-length-codes)
        (distance-table tiles--warc-distance-codes))
    (cl-loop for k from from below to
             for symbol = (aref symbols k)
             do (if (< symbol 256)
                    (tiles--warc-put-bits (aref codes symbol) (aref lengths symbol))
                  (let* ((length (ash symbol -16))
                         (distance (logand symbol #xffff))
                         (code (aref length-codes length))
                         (distance-code (aref distance-table distance)))
                    (tiles--warc-put-bits (aref codes (+ 257 code)) (aref lengths (+ 257 code)))
                    (tiles--warc-put-bits (- length (aref tiles--warc-length-bases code))
                                      (aref tiles--warc-length-extra code))
                    (tiles--warc-put-bits (aref distance-codes distance-code)
                                      (aref distance-lengths distance-code))
                    (tiles--warc-put-bits (- distance (aref tiles--warc-distance-bases distance-code))
                                      (aref tiles--warc-distance-extra distance-code)))))
    (tiles--warc-put-bits (aref codes 256) (aref lengths 256))))

(defun tiles--warc-deflate (parts)
  "Insert at point the DEFLATE data of PARTS.
PARTS is a list of (DATA . COMPRESS), where DATA is a unibyte string
and COMPRESS is nil for data that would not get smaller, which is
stored as it is."
  (tiles--warc-deflate-tables)
  (let ((tiles--warc-bits 0)
        (tiles--warc-bit-count 0)
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
          (pcase-let ((`(,symbols . ,count) (tiles--warc-lz77 data))
                      (from 0)
                      (start 0))
            (while (< from count)
              (let ((to (min count (+ from tiles--warc-block-symbols)))
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
          (tiles--warc-put-bits 1 1)
          (tiles--warc-put-bits 1 2)
          (tiles--warc-put-bits 0 7))
      (let ((last (car blocks)))
        (dolist (block (nreverse blocks))
          (pcase block
            (`(stored ,data ,start ,end)
             (tiles--warc-write-stored data start end (eq block last)))
            (`(huffman ,data ,start ,end ,symbols ,from ,to)
             (tiles--warc-write-huffman data symbols from to start end
                                    (eq block last)))))))
    (tiles--warc-align-bits)))

(defun tiles--warc-u32 (n)
  "Return N as four bytes, lowest first."
  (unibyte-string (logand n 255) (logand (ash n -8) 255)
                  (logand (ash n -16) 255) (logand (ash n -24) 255)))

(defun tiles--warc-gzip (parts)
  "Return PARTS compressed as one gzip member, as a unibyte string.
PARTS is a list of (DATA . COMPRESS), as for `tiles--warc-deflate'."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert (unibyte-string #x1f #x8b 8 0 0 0 0 0 0 255))
    (tiles--warc-deflate parts)
    (let ((crc 0)
          (size 0))
      (dolist (part parts)
        (setq crc (tiles--warc-crc32 (car part) crc)
              size (+ size (length (car part)))))
      (insert (tiles--warc-u32 crc) (tiles--warc-u32 (logand size #xffffffff))))
    (buffer-string)))

;;;; Capturing

(defconst tiles--warc-max-redirects 10
  "Number of redirects followed when capturing a page.")

(defun tiles--warc-base32 (bytes)
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

(defun tiles--warc-digest (data)
  "Return the digest of the unibyte string DATA, as written in WARC headers."
  (concat "sha1:" (tiles--warc-base32 (secure-hash 'sha1 data nil nil t))))

(defun tiles--warc-record-id ()
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

(defun tiles--warc-capture-url (url)
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

(defun tiles--warc-clipboard-url ()
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

(defun tiles--warc-read-capture-url ()
  "Read the URL of a page to capture, offering the one in the clipboard."
  (let ((default (tiles--warc-clipboard-url)))
    (read-string (format-prompt "Capture URL" default) nil nil default)))

(defun tiles--warc-capture-directory ()
  "Return the directory where new archives are saved."
  (file-name-as-directory
   (expand-file-name (or tiles-warc-capture-directory default-directory))))

(defun tiles--warc-capture-file-name (url)
  "Return the name of a new archive for the page at URL."
  (let* ((host (replace-regexp-in-string
                "[^[:alnum:].-]" "_" (url-host (url-generic-parse-url url))))
         (base (expand-file-name (concat host (format-time-string "-%Y%m%d-%H%M%S"))
                                 (tiles--warc-capture-directory)))
         (extension (if tiles-warc-capture-compress ".warc.gz" ".warc"))
         (file (concat base extension))
         (count 1))
    (while (file-exists-p file)
      (setq file (format "%s-%d%s" base (cl-incf count) extension)))
    file))

(defun tiles--warc-archive-format (file)
  "Return the format of the WARC FILE, `warc' or `gzip', or nil if not WARC."
  (let ((reader (tiles--warc-file-reader file)))
    (unwind-protect
        (pcase (tiles--warc-file-format reader)
          ('warc 'warc)
          ('gzip (and (tiles--warc-warc-start (car (tiles--warc-gunzip-peek reader 0)))
                      'gzip)))
      (kill-buffer (tiles--warc-reader-buffer reader)))))

(defun tiles--warc-check-capture-file (file url)
  "Return the file to save the page at URL in, when FILE was chosen.
Signal a `user-error' if FILE is not a WARC file, and ask before
adding to an existing archive."
  (let ((file (expand-file-name file)))
    (when (file-directory-p file)
      (setq file (expand-file-name
                  (file-name-nondirectory (tiles--warc-capture-file-name url)) file)))
    (when (and (file-exists-p file)
               (> (file-attribute-size (file-attributes file)) 0))
      (unless (tiles--warc-archive-format file)
        (user-error "%s is not a WARC file" (abbreviate-file-name file)))
      (unless (y-or-n-p (format "Add the page to %s? " (abbreviate-file-name file)))
        (user-error "Nothing captured")))
    file))

(defun tiles--warc-http-request (url)
  "Return the HTTP request that fetches URL."
  (let ((parsed (url-generic-parse-url url)))
    (concat "GET " (url-filename parsed) " HTTP/1.1\r\n"
            "Host: " (url-host parsed)
            (if (url-portspec parsed) (format ":%d" (url-port parsed)) "") "\r\n"
            "User-Agent: " tiles-warc-capture-user-agent "\r\n"
            "Accept: text/html,application/xhtml+xml,*/*;q=0.8\r\n"
            ;; Compressed pages are smaller to fetch and to store.
            "Accept-Encoding: gzip\r\n"
            "Connection: close\r\n"
            "\r\n")))

(defun tiles--warc-response-complete-p (buffer)
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

(defun tiles--warc-fetch-start (url callback &optional progress)
  "Start fetching URL in the background and return a function that stops it.
When the whole response has arrived, call CALLBACK with the exchange
with the server: a plist with :url, :date, :ip, :status, :location,
and :request and :response, the bytes sent and received.  If URL
cannot be fetched, call CALLBACK with an error message instead.  With
PROGRESS non-nil, report progress in the echo area."
  (let* ((parsed (url-generic-parse-url url))
         (https (equal (url-type parsed) "https"))
         (host (string-trim (url-host parsed) "\\[" "]"))
         (request (tiles--warc-http-request url))
         (buffer (tiles--warc-unibyte-buffer " *tiles-warc-capture*"))
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
                                         (tiles--warc-field (nth 1 (tiles--warc-split-http response))
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
           (when (tiles--warc-response-complete-p buffer)
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
           (when (> (- (float-time) active) tiles-warc-capture-timeout)
             (finish (format "No data from %s for %d seconds"
                             url tiles-warc-capture-timeout)))))
      (let ((problem
             (if (and https (not (gnutls-available-p)))
                 "This Emacs has no GnuTLS support, so it cannot fetch https URLs"
               (when progress
                 (message "Connecting to %s..." host))
               (condition-case err
                   (progn
                     (setq process (open-network-stream "tiles-warc-capture" buffer host
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

(defun tiles--warc-fetch-page-start (url callback &optional progress)
  "Start fetching URL in the background, following redirects.
Return a function that stops fetching.  Call CALLBACK with the list of
exchanges with the servers, as described in `tiles--warc-fetch-start', or
with an error message if the page cannot be fetched.  With PROGRESS
non-nil, report progress in the echo area."
  (let (seen exchanges stop)
    (cl-labels
        ((fetch (url)
           (cond
            ((member url seen)
             (funcall callback (format "Redirect loop at %s" url)))
            ((> (length seen) tiles--warc-max-redirects)
             (funcall callback (format "More than %d redirects from %s"
                                       tiles--warc-max-redirects (car (last seen)))))
            (t
             (push url seen)
             (setq stop (tiles--warc-fetch-start url #'fetched progress)))))
         (fetched (result)
           (if (stringp result)
               (funcall callback result)
             (push result exchanges)
             (let* ((location (plist-get result :location))
                    (next (and location
                               (tiles--warc-expand-url location (plist-get result :url)))))
               (cond
                ((not (and next (string-match-p "\\`https?://" next)))
                 (funcall callback (reverse exchanges)))
                ((setq next (ignore-error user-error (tiles--warc-capture-url next)))
                 (fetch next))
                (t
                 (funcall callback (format "Cannot follow the redirect to %s"
                                           location))))))))
      (fetch url)
      (lambda ()
        (funcall stop)))))

(defun tiles--warc-wait (start)
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

(defun tiles--warc-fetch-page (url)
  "Fetch URL, following redirects, and return the list of exchanges.
Signal a `user-error' if the page cannot be fetched."
  (tiles--warc-wait (lambda (callback)
                  (tiles--warc-fetch-page-start url callback t))))

(defun tiles--warc-warc-record (fields block)
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

(defun tiles--warc-compressible-p (headers)
  "Return non-nil if the body of an HTTP message with HEADERS may compress.
Bodies that are compressed already do not."
  (let ((encoding (downcase (string-trim (or (tiles--warc-field headers "Content-Encoding")
                                             ""))))
        (type (downcase (or (tiles--warc-field headers "Content-Type") ""))))
    (and (member encoding '("" "identity"))
         (not (and (tiles--warc-media-mime-p type)
                   (not (string-search "svg" type))))
         (not (string-match-p
               "\\`application/\\(?:zip\\|gzip\\|x-gzip\\|zstd\\|x-xz\\|x-bzip2\\|x-7z\\|pdf\\)"
               type)))))

(defun tiles--warc-http-parts (message)
  "Return the HTTP MESSAGE as a list of (DATA . COMPRESS).
The head is compressed, and the body when `tiles--warc-compressible-p'."
  (if (string-match "\r?\n\r?\n" message)
      (let ((body-start (match-end 0)))
        `((,(substring message 0 body-start) . t)
          (,(substring message body-start)
           . ,(tiles--warc-compressible-p
               (nth 1 (tiles--warc-split-http message))))))
    `((,message . t))))

(defun tiles--warc-warcinfo-record (file)
  "Return the record that describes the new archive FILE."
  (tiles--warc-warc-record
   `(("WARC-Type" . "warcinfo")
     ("WARC-Record-ID" . ,(tiles--warc-record-id))
     ("WARC-Date" . ,(format-time-string "%FT%TZ" nil t))
     ("WARC-Filename" . ,(file-name-nondirectory file))
     ("Content-Type" . "application/warc-fields"))
   `((,(encode-coding-string
        (format "software: warcel/%s (GNU Emacs %s)\r\nformat: WARC File Format 1.1\r\n"
                tiles--warc-version emacs-version)
        'utf-8)
      . t))))

(defun tiles--warc-exchange-records (exchange)
  "Return the response and request records of EXCHANGE."
  (let* ((date (format-time-string "%FT%TZ" (plist-get exchange :date) t))
         (url (plist-get exchange :url))
         (ip (plist-get exchange :ip))
         (request (plist-get exchange :request))
         (response (plist-get exchange :response))
         (response-id (tiles--warc-record-id))
         (payload (pcase-let ((`(,_ ,headers ,body) (tiles--warc-split-http response)))
                    ;; The payload is the body without its transfer coding.
                    (if (string-search "chunked"
                                       (downcase (or (tiles--warc-field headers
                                                                    "Transfer-Encoding")
                                                     "")))
                        (tiles--warc-dechunk body)
                      body))))
    (list (tiles--warc-warc-record
           `(("WARC-Type" . "response")
             ("WARC-Record-ID" . ,response-id)
             ("WARC-Date" . ,date)
             ("WARC-Target-URI" . ,url)
             ,@(and ip `(("WARC-IP-Address" . ,ip)))
             ("WARC-Block-Digest" . ,(tiles--warc-digest response))
             ("WARC-Payload-Digest" . ,(tiles--warc-digest payload))
             ("Content-Type" . "application/http; msgtype=response"))
           (tiles--warc-http-parts response))
          (tiles--warc-warc-record
           `(("WARC-Type" . "request")
             ("WARC-Record-ID" . ,(tiles--warc-record-id))
             ("WARC-Date" . ,date)
             ("WARC-Target-URI" . ,url)
             ("WARC-Concurrent-To" . ,response-id)
             ("WARC-Block-Digest" . ,(tiles--warc-digest request))
             ("Content-Type" . "application/http; msgtype=request"))
           (tiles--warc-http-parts request)))))

(defun tiles--warc-write-capture (file exchanges)
  "Add the records of EXCHANGES to the archive FILE, creating it if needed.
A new archive is compressed if FILE ends in .gz; records are added to
an existing archive in its own format."
  (let* ((new (or (not (file-exists-p file))
                  (zerop (file-attribute-size (file-attributes file)))))
         (compress (if new
                       (string-match-p "\\.gz\\'" file)
                     (eq (tiles--warc-archive-format file) 'gzip)))
         (records (append (and new (list (tiles--warc-warcinfo-record file)))
                          (mapcan #'tiles--warc-exchange-records exchanges))))
    (make-directory (file-name-directory file) t)
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (dolist (record records)
        (if compress
            ;; One gzip member per record, as other tools write them.
            (insert (tiles--warc-gzip record))
          (dolist (part record)
            (insert (car part)))))
      (let ((coding-system-for-write 'no-conversion))
        (write-region nil nil file (not new) 'silent)))))

;;;###autoload
(defun tiles-warc-capture (url &optional file)
  "Save the web page at URL in a WARC archive and show it.
Interactively, ask for URL, offering the URL in the clipboard.

The page goes to a new archive in `tiles-warc-capture-directory', named
after the host and the time, and is shown.  With a prefix argument,
ask for the archive FILE; the page is added to it if it already
exists, and the records of the archive are listed.

The page is saved with the redirects that lead to it, but without
its images, styles and scripts, which TILES does not show.  New
archives are compressed when `tiles-warc-capture-compress' is non-nil,
or when FILE ends in .gz."
  (interactive
   (let ((url (tiles--warc-capture-url (tiles--warc-read-capture-url))))
     (list url
           (and current-prefix-arg
                (let ((name (file-name-nondirectory (tiles--warc-capture-file-name url))))
                  (read-file-name "Save in archive: " (tiles--warc-capture-directory)
                                  nil nil name))))))
  (let* ((url (tiles--warc-capture-url url))
         (file (if file
                   (tiles--warc-check-capture-file file url)
                 (tiles--warc-capture-file-name url)))
         (exchanges (tiles--warc-fetch-page url))
         (last (car (last exchanges)))
         (status (plist-get last :status)))
    (tiles--warc-write-capture file exchanges)
    (when-let* ((buffer (tiles--warc-listing-buffer file)))
      (with-current-buffer buffer
        (revert-buffer)))
    (tiles-warc-open file)
    (when (derived-mode-p 'tiles-warc-mode)
      (tiles--warc-goto-uri (plist-get last :url)))
    (message "Saved %s in %s%s"
             (plist-get last :url) (abbreviate-file-name file)
             (if (<= 200 status 299) "" (format " (the server answered %d)" status)))
    file))

(defun tiles-warc-capture-in-background (url file &optional callback)
  "Save the web page at URL in a WARC archive, without waiting for it.
The page is fetched in the background and saved as `tiles-warc-capture'
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
  (tiles--warc-fetch-page-start
   (tiles--warc-capture-url url)
   (lambda (result)
     (let (archive status problem)
       (if (stringp result)
           (setq problem result)
         (let* ((response (plist-get (car (last result)) :response))
                (type (downcase (string-trim
                                 (car (split-string
                                       (or (tiles--warc-field (nth 1 (tiles--warc-split-http response))
                                                          "Content-Type")
                                           "")
                                       ";"))))))
           (setq status (plist-get (car (last result)) :status))
           (condition-case err
               (when (setq archive (if (functionp file) (funcall file status type) file))
                 (tiles--warc-write-capture archive result))
             (error (setq archive nil
                          problem (error-message-string err))))))
       (when callback
         (funcall callback archive status problem))))))

(defun tiles-warc-canonical-url (url)
  "Return URL in the form that `tiles-warc-capture' fetches and archives.
The fragment is dropped, the host name IDNA-encoded and the rest
percent-encoded.  Return nil if URL is not an http or https URL."
  (ignore-error user-error
    (tiles--warc-capture-url url)))

(defun tiles-warc-archived-urls (file)
  "Return the URLs of the pages archived in the WARC FILE.
They are the target URIs of its response and resource records, so they
include the redirects that led to a page, in the form given by
`tiles-warc-canonical-url' for pages saved by TILES."
  (let ((inhibit-message t)
        (message-log-max nil))
    (delete-dups
     (delq nil (mapcar (lambda (record)
                         (and (member (tiles-warc-record-type record) '("response" "resource"))
                              (tiles-warc-record-uri record)))
                       (tiles-warc-archive-records (tiles-warc-read-archive file)))))))

;;;; Visiting compressed archives

;; Emacs visits compressed files with `jka-compr', which runs the gzip
;; program, and would stop after the first gzip member if it used zlib
;; instead.  This file name handler reads .warc.gz files with zlib,
;; member after member.  It only handles the insertion of .warc.gz
;; files, and is only installed while `tiles-archive-urls' is on.

(defun tiles--warc-gunzip-file (file)
  "Return a unibyte buffer holding all the decompressed gzip members of FILE.
Return nil if FILE does not start with a gzip member."
  (let ((reader (tiles--warc-file-reader file))
        (offset 0)
        data)
    (unwind-protect
        (when (eq (tiles--warc-file-format reader) 'gzip)
          (setq data (tiles--warc-unibyte-buffer " *tiles-warc-gunzip*"))
          (while (< offset (tiles--warc-reader-size reader))
            (pcase-let ((`(,end ,_ ,_ ,member) (tiles--warc-gzip-member-end reader offset nil)))
              (tiles--warc-append-member data file offset end member)
              (setq offset end)))
          data)
      (kill-buffer (tiles--warc-reader-buffer reader)))))

(defun tiles--warc-insert-gzip-file (file &optional visit beg end replace)
  "Insert the decompressed contents of the gzip FILE.
VISIT, BEG, END and REPLACE are as for `insert-file-contents'."
  (barf-if-buffer-read-only)
  (when (and visit (or beg end))
    (error "Attempt to visit less than an entire file"))
  (let* ((filename (expand-file-name file))
         (data (tiles--warc-gunzip-file filename))
         size)
    (if (not data)
        ;; Not gzip data after all: let the other handlers try.
        (let ((inhibit-file-name-handlers
               (cons 'tiles-warc-file-handler
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


;;;; Opening WARC files

;; Installed while `tiles-archive-urls' is on, and removed when it is
;; turned off: see `tiles--warc-setup'.

(defun tiles-warc-file-handler (operation &rest args)
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
      (apply #'tiles--warc-insert-gzip-file args)
    (let ((inhibit-file-name-handlers
           (cons 'tiles-warc-file-handler
                 (and (eq inhibit-file-name-operation operation)
                      inhibit-file-name-handlers)))
          (inhibit-file-name-operation operation))
      (apply operation args))))
(put 'tiles-warc-file-handler 'operations '(insert-file-contents))

(defun tiles-warc-dired-find-file ()
  "In Dired, show the WARC file at point with `tiles-warc-open'.
Visit other files with `dired-find-file'.  While `tiles-archive-urls'
is on, this command replaces `dired-find-file' in Dired, so that RET
opens WARC files this way."
  (interactive nil dired-mode)
  (let ((file (dired-get-file-for-visit)))
    (if (and (string-match-p "\\.warc\\(?:\\.gz\\)?\\'" file)
             (file-regular-p file))
        (tiles-warc-open file)
      (dired-find-file))))

(defconst tiles--warc-file-handler-entry '("gz\\'" . tiles-warc-file-handler)
  "The entry of `file-name-handler-alist' that reads .warc.gz files.
The handler whose match starts last wins: this one matches after the
dot that the `jka-compr' handler matches.")

(defconst tiles--warc-auto-mode-entry '("\\.warc\\'" . tiles-warc-visit)
  "The entry of `auto-mode-alist' that shows the WARC files visited.")

(defun tiles--warc-dired-setup (on)
  "Make RET in Dired open WARC files with `tiles-warc-open' if ON, else not."
  (if on
      (keymap-set dired-mode-map "<remap> <dired-find-file>"
                  #'tiles-warc-dired-find-file)
    (when (eq (lookup-key dired-mode-map [remap dired-find-file])
              'tiles-warc-dired-find-file)
      (keymap-unset dired-mode-map "<remap> <dired-find-file>" t))))

(defun tiles--warc-setup (on)
  "Set up how Emacs opens WARC files if ON, otherwise undo it.
That is: visiting a .warc file shows it with `tiles-warc-visit',
.warc.gz files are read with the zlib built into Emacs, and RET in
Dired opens WARC files with `tiles-warc-open'.  It follows
`tiles-archive-urls', see `tiles--archive-urls-watcher'."
  (if on
      (progn
        (add-to-list 'file-name-handler-alist tiles--warc-file-handler-entry)
        (add-to-list 'auto-mode-alist tiles--warc-auto-mode-entry))
    (setq file-name-handler-alist (delete tiles--warc-file-handler-entry
                                          file-name-handler-alist)
          auto-mode-alist (delete tiles--warc-auto-mode-entry auto-mode-alist)))
  (when (featurep 'dired)
    (tiles--warc-dired-setup on)))

;; Dired loaded later is set up as `tiles-archive-urls' says by then.
(with-eval-after-load 'dired
  (tiles--warc-dired-setup tiles-archive-urls))

(defun tiles--archive-urls-watcher (_symbol newval operation _where)
  "Follow a new value of `tiles-archive-urls' with `tiles--warc-setup'.
For `add-variable-watcher': NEWVAL is the new value and OPERATION the
kind of change.  It runs for `setq' as well as for Customize, so a
plain (setq tiles-archive-urls t) is enough to turn archiving on."
  (unless (eq operation 'defvaralias)
    (tiles--warc-setup newval)))

(add-variable-watcher 'tiles-archive-urls #'tiles--archive-urls-watcher)
;; Also when it was set before TILES was loaded.
(when tiles-archive-urls
  (tiles--warc-setup t))

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

;; To bind the command map, add to your init.el:
;; (global-set-key (kbd "C-c m") tiles-command-map)

(provide 'tiles)
;;; tiles.el ends here
