# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What is TILES

TILES (Tagged Instant Lightweight Emacs Snippets) is a single-file Emacs package (`tiles.el`) for quick, title-less paragraph notes stored as individual org files. Notes are organized through tags and bold keywords rather than hierarchies or titles.

## Development

This is an Emacs Lisp package with no build system. To test changes, evaluate the buffer in Emacs (`M-x eval-buffer`) or byte-compile with `emacs -Q --batch -f batch-byte-compile tiles.el`. Notes are stored in `tiles-directory` (default `~/notes/tiles/`), loaded recursively from subdirectories.

## Note file format

Each file is named `TYYYYMMDDHHMMSS.org`:
- First part: paragraph content with org formatting (`*bold*` = keywords, `/italic/`, `[[links]]`, inline footnotes `[fn:: text]`)
- Optional private paragraphs prefixed with `&&` (hidden from previews, stitched views, search panels, and dynamic blocks; only visible via TAB expansion in the dashboard or direct file editing)
- Blank line separator
- Last line: tags separated by `/`

## Architecture

Everything lives in `tiles.el`. The code is organized in these sections:

### Parsing and caching
- `tiles--parse-note-file` / `tiles--parse-note-file-uncached`: Read note files. The last non-empty line is always the tag line (split on `/`). Everything above it (minus trailing blank lines) is content. Bold `*words*` in the content are extracted as searchable keywords. Hyphens in keywords are normalized to spaces during extraction (e.g. `*Falcon-9*` becomes keyword "Falcon 9"), but note content is never modified. `delete-dups` removes duplicates after normalization. This approach supports multi-paragraph notes.
- `tiles--cache`: Hash table keyed by filepath, values are `(mtime . parsed-data)`. Invalidated automatically by mtime comparison, manually via `tiles-clear-cache` or `remhash`.
- `tiles--private-paragraph-p` / `tiles--strip-private-paragraphs` / `tiles--extract-private-paragraphs`: Handle `&&`-prefixed private paragraphs. Content stored in `:content` always includes private paragraphs (for keyword extraction etc.), but display functions use `tiles--strip-private-paragraphs` to filter them out. `tiles--extract-private-paragraphs` returns the private text with the `&&` prefix removed, used by TAB expansion.

### Dashboard (`tiles-show-notes`, `tiles-notes-view-mode`)
- Chronological listing of all notes, newest first, with header showing stats, keybinding help, and a status line.
- Header layout: title line (TILES name, version via `tiles-version`, note count, load time), `════` (or `====` when `tiles-fancy-separators` is nil), two keybinding lines, status line (moon phase + active filters/exclusions separated by `>`), `────` (or `----`).
- Moon phase: uses Emacs built-in `lunar` library to show days until next New Moon or Full Moon. Always displayed (no toggle). `tiles--next-lunar-event` computes this.
- Each line: 2-space indent, color-coded timestamp (without seconds: `YYYY-MM-DD HH:MM`), org-rendered preview (truncated at `tiles-preview-length` chars, default 105, with `…` ellipsis), tags in red.
- Preview rendering: when `tiles-preview-raw` is nil (default), strips footnotes/links and applies bold/italic faces. When non-nil, `tiles--strip-org-markup` removes all org markup to plain text. `f` in dashboard toggles this. Link stripping happens before truncation to avoid breaking mid-link. When formatted preview is on, notes with private `&&` paragraphs show a red `&` indicator replacing the first separator space before the preview text.
- Timestamp faces: today (`#228b22`), <2 weeks (`#3a5a2a`), older (`#999999`). Selection line: Lufthansa yellow (`#FFC700`).
- `SPC` opens the actual file in a split for editing; navigating with `n`/`p` updates the preview split automatically.
- `TAB` toggles expanded view: inserts extra lines below the selected note — private `&&` paragraph(s) if present (in `font-lock-doc-face`, multiple paragraphs joined by ` | `), keywords line (in green), and stats line (chars, words, filesize). Expanded lines carry the `tiles-expanded` text property. The first (note) line is not re-rendered during expansion. `n`/`p` navigation skips expanded lines and cursor is locked during expansion.
- `M-up`/`M-down` reorder notes in the dashboard. `tiles--notes-displayed-files` reads the current display order from the buffer. Stitching (`0`) respects this manual order.
- `d` renames the file to change its date. `D` deletes the note (with `yes-or-no-p` confirmation showing filename and preview). `t`/`k` filter by tag/keyword. `F` excludes tags (hides notes with specified tags). `T`/`K` list all tags/keywords. `c` clears search filter (keeps exclusion). `C` clears tag exclusion (keeps search filter). `0` stitches currently displayed (possibly filtered/reordered) notes; prompts for confirmation when no filter is active. `l` creates a new note (same as `C-c m n`, with focus mode if `tiles-focus-default` is non-nil). `u` touches (updates timestamp). Filter state in `tiles--notes-filter`, exclusion state in `tiles--notes-exclude`.
- Pagination: `tiles-dashboard-limit` (default 50) caps displayed notes per page. `+` loads the next batch (`tiles--notes-page` tracks offset). Page resets on filter change, clear, or quit.
- After-save hook (`tiles--after-save-hook`) invalidates cache and refreshes the changed line in-place. Hook is added on mode entry, removed on quit.
- Cursor is restricted to note lines only (skips header/separator).

### Tag and keyword listing (`tiles-list-tags`, `tiles-list-keywords`)
- `tiles-list-tags` (`T` in dashboard): displays all unique tags with occurrence counts `[N]` in a navigable `*Tiles Tags*` buffer. Tags that also appear as keywords are shown in bold. `RET` filters the dashboard by the selected tag.
- `tiles-list-keywords` (`K` in dashboard): displays all unique keywords with occurrence counts `[N]` in a navigable `*Tiles Keywords*` buffer. Keywords that also appear as tags are shown in bold. `RET` filters the dashboard by the selected keyword.
- Both buffers support sorting: `a` alphabetical (a-z default), `o` by occurrence (high-to-low default), `d` toggles ascending/descending.
- `tiles-list-rename` (`R` in keyword list): renames a keyword across all note files. Prompts for a new name, finds all files containing the keyword (via parsed/normalized keyword matching), replaces every `*old*` bold occurrence with `*new*` in the file content, invalidates affected cache entries, and re-renders the list. Only available in the keyword list, not the tag list. Respects hyphen normalization (e.g. `*Falcon-9*` matches keyword "Falcon 9").

### Search (`tiles-tag-search`, `tiles-keyword-search`)
- Full scan of all files. Keyword search uses OR logic (space-separated terms).
- Tag search uses AND/OR: `/` means AND (all tags in group must match), space means OR (any group matches). `tiles--parse-tag-query` parses `"b218/lx2026 misc"` into `(("b218" "lx2026") ("misc"))`. `tiles--note-matches-tag-p` takes these AND-groups and uses `seq-every-p` within groups, `seq-some` across groups.
- Results feed into two-panel search view or stitched view.

### Two-panel search view (`tiles-search-view-mode`)
- Upper panel: navigable file list. Lower panel: read-only org preview that updates on cursor movement.
- `SPC` toggles to stitched view.

### Stitched view (`tiles-stitched-view-mode`)
- Concatenates matching note contents into a single flowing org buffer, stripped of tag lines and `&&` private paragraphs.
- Notes appear in inverse chronological order separated by blank lines.
- `tiles--stitched-boundaries` tracks `(position . filepath)` pairs for navigation (`n`/`p` jump between notes) and opening source files (`RET`/`e`).
- `RET`/`e` opens the source file with tag-line fontification and focus mode (when `tiles-focus-default` is non-nil).
- Toggle back to two-panel view with `SPC`.

### Tag-line fontification
- `tiles--tag-line-matcher`: font-lock matcher that highlights the last non-empty line in red (`tiles-tags` face) when editing a note, provided it is preceded by a blank line and does not start with `&` or `&&` (private paragraph).
- `tiles--enable-tag-line-fontification`: adds the matcher via `font-lock-add-keywords` and triggers `font-lock-flush`.
- Enabled in: capture mode, dashboard open (`RET`), search open, stitched open. Not enabled in the SPC preview split.

### Capture (`tiles-capture-mode`)
- Minor mode over org-mode. `C-c C-c` saves, `C-c C-k` cancels.
- Validates format before saving (needs paragraph + blank line + tags). If tags are missing, prompts the user to add them interactively rather than rejecting the save outright. Aborts if the user provides empty input.

### Quick capture (`tiles-quick`, `tiles-yank`)
- `tiles-quick`: prompts for content and tags in the minibuffer, creates the note. Shared helper `tiles--quick-save`.
- `tiles-yank`: same but pre-fills content from the kill ring (clipboard).

### Focus mode (`tiles-focus-mode`)
- Distraction-free editing minor mode, similar to olivetti-mode. Centers buffer content with ~80-char line width using window margins, hides fringes, enables `visual-line-mode` for soft word wrap.
- Adds 2 empty lines at the top via an overlay (`before-string`) — purely visual, never saved to the file.
- `tiles-focus-default` (`defcustom`, default `t`): when non-nil, `tiles-new` (`l` / `C-c m n`), dashboard open (`RET`), and stitched open (`RET`/`e`) enable focus mode automatically. Set to `nil` to disable.
- On save (`tiles-capture-save`), focus mode is deactivated before extracting buffer content.

### Touch (`tiles-touch`)
- `M-x tiles-touch`: updates the current note's timestamp to now and renames the file. Asks for confirmation. Refreshes the dashboard if open.
- Also available as `u` in the dashboard via `tiles-notes-touch`.

### Dynamic blocks
- `org-dblock-write:tiles-notes`: Inserts org link list of matching notes. Parameters: `:tags`, `:keywords`, `:sort` ("newest"/"oldest"), `:limit`.
- `org-dblock-write:tiles-files`: Embeds note contents. Additional parameter: `:separator`.
- `tiles--dblock-get-files`: Shared helper that filters, sorts, and limits files based on dblock params.
- Registered in `org-dynamic-block-alist` for the `C-c C-x x` insertion menu.

## Global keybindings

Bound under `C-c m`:
- `C-c m m` — dashboard (`tiles-show-notes`)
- `C-c m n` — new note (`tiles-new`)
- `C-c m q` — quick capture via minibuffer (`tiles-quick`)
- `C-c m y` — quick capture from clipboard (`tiles-yank`)
- `C-c m t` — tag search (`tiles-tag-search`)
- `C-c m k` — keyword search (`tiles-keyword-search`)

## Customization

All settings under the `tiles` customize group.

### Constants
- `tiles-version` — current version string, displayed in the dashboard title

### Variables (`defcustom`)
- `tiles-directory` (default `~/notes/tiles/`) — root directory, loaded recursively
- `tiles-preview-length` (default `105`) — max chars for inline dashboard preview
- `tiles-line-padding` (default `22`) — extra padding beyond preview and tags for line width
- `tiles-preview-raw` (default `t`) — when non-nil, strip all org formatting from previews
- `tiles-dashboard-limit` (default `50`) — max notes per page, `nil` for unlimited
- `tiles-focus-default` (default `t`) — when non-nil, enable focus mode for new notes
- `tiles-fancy-separators` (default `t`) — when non-nil, use Unicode box-drawing characters (═/─) for dashboard separators instead of ASCII (=/-).

### Faces (`defface`)
- `tiles-timestamp-today` (`#228b22`) — today's notes
- `tiles-timestamp-recent` (`#3a5a2a`) — less than 2 weeks old
- `tiles-timestamp-old` (`#999999`) — older than 2 weeks
- `tiles-tags` (`#a00000`) — tag display
- `tiles-keywords` (`#006600`) — keyword display in expanded view
- `tiles-notes-hl-line` (`#FFC700`) — Lufthansa yellow selection line
- `tiles-notes-expanded` (`#FFF8DC`) — background of expanded lines

## Key design decisions

- Single-file package — everything in `tiles.el`
- Preview rendering manually applies bold/italic faces and strips inline footnotes rather than using full org font-lock (needed for single-line display in the dashboard)
- The after-save hook is global (not buffer-local) — added when dashboard opens, removed on quit
- Dynamic blocks reuse the same filtering/parsing infrastructure as interactive search
- Tags are case-sensitive; no normalization applied
- Private paragraphs (`&&`) are stripped at display time, not parse time — `:content` always contains the full text so keyword extraction and stats reflect the complete note. The `&&` prefix was chosen over single `&` to avoid false positives with org HTML entities
- Keyword hyphen normalization happens only in the extracted `:keywords` list, never in `:content` — note files are never modified by the extraction process

## Future ideas (not yet implemented)

- AND/NOT search operators
- Tag completion/suggestions during capture
- Combined tag + keyword search in a single query
- Search history and recent searches
- Alternative sorting (relevance, custom)
- Integration with org-roam, deft, or similar packages
