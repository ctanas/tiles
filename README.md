# Tagged Instant Lightweight Emacs Snippets (TILES)

TILES is an Emacs package for taking quick, title-less notes. Each note is a single paragraph stored in its own org file, organized through tags and bold keywords rather than hierarchies. TILES tries to keep it simple: there are no dependencies (except for Emacs, version 27.1 at least), no links between notes (except the [Org Mode](https://orgmode.org/org.html) syntax), no backlinks, no graphs, no database; every note is a paragraph in its own text file.

I created this package because I wanted a note taking system with the following features:

- focus on one paragraph (like [Logseq](https://logseq.com)): one paragraph = one note;
- offers a bird's-eye view (quick preview) of recent notes (similar to [Howm](https://kaorahi.github.io/howm));
- quick note preview, quick note edit;
- color coding depending on the note's age (sort of like [Howm](https://kaorahi.github.io/howm), but not really);
- title-less, to reduce friction (why having to stop the thought process to create a title that's never used afterwards?);
- can use the Dynamic Block features in Org Mode (like [Denote](https://protesilaos.com/emacs/denote) and [Denote Org](https://protesilaos.com/emacs/denote-org), ideal if you want to use your notes to create other documents;
- can stitch notes together, after applying a search filter (like [Howm](https://kaorahi.github.io/howm), ideal if you want to use your notes to create other documents;
- uses tags for hierarchy but also uses bold keywords (extracted automatically from words that are marked as bold);
- search after tags and/or keywords only (who really wants to search for anything else?);
- no external dependencies needed except at least version 27.1 of Emacs and Org Mode (built-in);
- uses [Org Mode](https://orgmode.org/org.html) format for bold, italic, links, in-line footnotes;

## Screenshots

<img width="1453" height="765" alt="Screenshot 2026-02-07 at 14 08 18" src="https://github.com/user-attachments/assets/59752a09-63ba-40b3-9ab4-15f65c30a4ed" />
<img width="1456" height="762" alt="Screenshot 2026-02-07 at 14 07 29" src="https://github.com/user-attachments/assets/e20ee23f-3a80-4743-8f22-fbe10bd99d1b" />
<img width="1458" height="763" alt="Screenshot 2026-02-07 at 14 07 08" src="https://github.com/user-attachments/assets/fde720c4-c626-4917-bc93-690cb30623d3" />

## Note format

Each note is a file named `TYYYYMMDDHHMMSS.org` (T followed by a timestamp up to seconds) and stored in a predefined folder:

```
The Mars Sample Return (*MSR*) mission involved
a collaboration between *NASA* and *ESA* to 
retrieve samples collected by the *Perseverance* 
rover[fn:: Launched in July 2020].

space/mars
```

- Content: paragraph(s) with full org-mode formatting (`*bold*`, `/italic/`, `[[links]]`, inline footnotes);
- Blank line separator after the content;
- Last non-empty line: tags separated by `/` (always parsed as the tag line); tags are mandatory;
- Bold words (`*word*`) double as searchable keywords (optional);
- Multi-paragraph notes are supported but discouraged; the parser always takes the last non-empty line as tags.

## Installation

Clone the repository and add to your load path:

```elisp
(add-to-list 'load-path "/path/to/tiles")
(require 'tiles)
```

Set your notes directory (default `~/notes/tiles/`):

```elisp
(setq tiles-directory "~/notes/tiles/")
```

## Usage

### Global keybindings

All commands are under the `C-c m` prefix:

| Key       | Command                | Description                    |
|-----------|------------------------|--------------------------------|
| `C-c m m` | `tiles-show-notes`     | Open dashboard                 |
| `C-c m n` | `tiles-new`            | Create a new note (buffer)     |
| `C-c m q` | `tiles-quick`          | Quick capture via minibuffer   |
| `C-c m y` | `tiles-yank`           | Quick capture from clipboard   |
| `C-c m t` | `tiles-tag-search`     | Search by tags                 |
| `C-c m k` | `tiles-keyword-search` | Search by keywords             |

### Dashboard

`C-c m m` launches the dashboard, which displays a chronological list of all notes. Each entry shows color-coded timestamps (showing hours and minutes to save space), inline previews, tags, and keywords. Timestamps are color-coded: green for today, darker green for recent (< 2 weeks), faded grey for older notes. The selection highlight is Lufthansa yellow. While the dashboard displays truncated timestamps for brevity, the actual filenames include timestamps down to the second level, allowing you to create multiple notes within the same minute without conflicts.

```
  *T*agged *I*nstant *L*ightweight *E*macs *S*nippet (TILES) | 42 notes | loaded in 0.023s
  SPC:preview  RET:open  d:change date  t:filter tag  k:filter keyword  c:clear filter  g:refresh  q:quit
  ========================================================================

  2026-02-06 08:12  Hello world, I'm the first tile!  meta/test
  2026-02-06 08:12  This note is ready for production  meta/prod
```

Dashboard keybindings:

| Key       | Action                                        |
|-----------|-----------------------------------------------|
| `n/p`     | Navigate notes                                |
| `SPC`     | Open editable preview split (follows cursor)  |
| `RET`     | Open note file                                |
| `TAB`     | Toggle expanded view (keywords + stats)       |
| `M-up`    | Move selected note up                         |
| `M-down`  | Move selected note down                       |
| `d`       | Change note date/timestamp (renames file)     |
| `D`       | Delete note (with confirmation)               |
| `t`       | Filter displayed notes by tag                 |
| `k`       | Filter displayed notes by keyword             |
| `c`       | Clear filter                                  |
| `f`       | Toggle raw preview (strip org formatting)     |
| `+`       | Load next batch of notes                      |
| `0`       | Stitch displayed notes into flowing view      |
| `g`       | Refresh                                       |
| `q`       | Quit                                          |

### Tag search syntax

Tag queries use `/` for **AND** and SPC for **OR**:

| Query              | Meaning                                    |
|--------------------|--------------------------------------------|
| `b218/lx2026`      | Notes with **both** `b218` and `lx2026`    |
| `b218 misc`        | Notes with **either** `b218` or `misc`     |
| `b218/lx2026 misc` | (`b218` AND `lx2026`) OR `misc`            |

This syntax applies everywhere: `tiles-tag-search`, dashboard filter (`t`), and dynamic block `:tags` parameter.

### Keyword search syntax

Keyword queries use SPC-separated terms with **OR** logic — any matching term is enough:

| Query              | Meaning                                           |
|--------------------|---------------------------------------------------|
| `emacs`            | Notes with `emacs` as a bold keyword              |
| `emacs lisp`       | Notes with **either** `emacs` or `lisp`           |

Keywords are the `*bold*` words extracted from note content. This syntax applies to `tiles-keyword-search`, dashboard filter (`k`), and dynamic block `:keywords` parameter.

### Search views

Tag and keyword searches (`C-c m t` / `C-c m k`) open a **two-panel view**: results list on top, live preview below.

| Key   | Action                          |
|-------|---------------------------------|
| `n/p` | Navigate results                |
| `RET` | Open note file                  |
| `SPC` | Toggle to stitched view         |
| `r`   | Refine search (new query)       |
| `t/k` | Switch to tag/keyword search    |
| `q`   | Quit                            |

### Stitched view

Press `SPC` from the search view to enter the **stitched view**: all matching notes concatenated into a single flowing org buffer, stripped of tag lines, in inverse chronological order. This is useful for reading related notes as continuous prose or if you want to include multiple related notes into another document, like a newsletter.

| Key     | Action                            |
|---------|-----------------------------------|
| `n/p`   | Jump between note boundaries      |
| `RET/e` | Open the source file at point     |
| `SPC`   | Toggle back to two-panel view     |
| `r`     | Refine search                     |
| `q`     | Quit                              |

### Capturing notes

`C-c m n` opens a capture buffer. Write your paragraph, add a blank line, then your tags. Press `C-c C-c` to save, `C-c C-k` to cancel. While keywords are not mandatory, tags are, so if the user forgets to add tags, it will be asked to do so.

For faster capture, `C-c m q` prompts for content and tags directly in the minibuffer. `C-c m y` does the same but pre-fills the content from the clipboard (kill ring), which you can edit before confirming.

### Updating a note's timestamp

While editing a note, `M-x tiles-touch` updates the file's timestamp to the current time and renames the file accordingly. Asks for confirmation before proceeding. Useful for bumping a note to the top of the chronological list after editing.

### Org dynamic blocks

TILES provides two dynamic block types for embedding note data in org files:

**`tiles-notes`** - Insert a linked list of matching notes:

```org
#+BEGIN: tiles-notes :tags "space mars" :sort "newest" :limit 10
- [[file:~/notes/tiles/T20260206081250.org][2026-02-06 08:12:50]] The Mars Sample Return...  space/mars
- [[file:~/notes/tiles/T20260206081227.org][2026-02-06 08:12:27]] NASA announced today...  space/nasa
#+END:
```

**`tiles-files`** - Embed note contents directly:

```org
#+BEGIN: tiles-files :tags "journal" :keywords "review" :separator "\n-----\n"
First matching note content...

-----
Second matching note content...
#+END:
```

**Parameters** (all optional):

| Parameter    | Description                                | Default     |
|--------------|--------------------------------------------|-------------|
| `:tags`      | Space-separated tags (OR logic)            | —           |
| `:keywords`  | Space-separated keywords (OR logic)        | —           |
| `:sort`      | `"newest"` or `"oldest"`                   | `"newest"`  |
| `:limit`     | Maximum number of notes                    | unlimited   |
| `:separator` | String between notes (`tiles-files` only)  | blank line  |

- `C-c C-x x` to insert a dynamic block from a menu
- `C-c C-x C-u` to update the block under cursor

## Performance

TILES uses an in-memory cache that stores parsed note data keyed by filepath. Files are only re-read from disk when their modification time changes. The first load reads all files; subsequent operations are fast. Use `M-x tiles-clear-cache` to force a full reload.

## Customization

All settings are available via `M-x customize-group RET tiles`.

### Variables

| Variable               | Description                              | Default              |
|------------------------|------------------------------------------|----------------------|
| `tiles-directory`      | Root directory for notes (recursive)     | `~/notes/tiles/`     |
| `tiles-preview-length` | Max characters for inline preview        | `105`                |
| `tiles-line-padding`   | Extra padding beyond preview and tags    | `22`                 |
| `tiles-preview-raw`    | Strip all org formatting from previews   | `t`                  |
| `tiles-dashboard-limit`| Max notes per page (`nil` = unlimited)   | `50`                 |

Example configuration:

```elisp
(setq tiles-directory "~/Documents/tiles/")
(setq tiles-preview-length 120)
```

### Faces (colors)

All faces can be customized via `M-x customize-face` or in your config:

| Face                     | Description                          | Default       |
|--------------------------|--------------------------------------|---------------|
| `tiles-timestamp-today`  | Today's timestamps                   | `#228b22`     |
| `tiles-timestamp-recent` | Recent timestamps (< 2 weeks)        | `#3a5a2a`     |
| `tiles-timestamp-old`    | Older timestamps (> 2 weeks)         | `#999999`     |
| `tiles-tags`             | Tag display                          | `#a00000`     |
| `tiles-keywords`         | Keyword display in expanded view     | `#006600`     |
| `tiles-notes-hl-line`    | Selection highlight                  | `#FFC700`     |
| `tiles-notes-expanded`   | Background of expanded lines         | `#FFF8DC`     |

Example:

```elisp
(set-face-attribute 'tiles-timestamp-today nil :foreground "#008800")
(set-face-attribute 'tiles-notes-hl-line nil :background "#FFD700")
```

## Acknowledgements

Many thanks to Protesilaos Stavrou for [Denote](https://protesilaos.com/emacs/denote) and [Denote Org](https://protesilaos.com/emacs/denote-org), Kazuyuki Hiraoka for [Howm](https://kaorahi.github.io/howm/), Andrei Sukhovskii for [Howm Manual](https://emacs101.github.io/howm.html), Jethro Kuan for [Org-roam](https://www.orgroam.com/),  Jason Blevins for [Deft](https://github.com/jrblevin/deft), Zachary Schneirov for [Notational Velocity](https://notational.net/), and to all the developers of [Logseq](https://logseq.com/) and [Obsidian](https://obsidian.md/) for their inspiration into creating this package.

## Disclaimer

This package was developed with the assistance of Claude, an AI assistant created by Anthropic.

## License

GNU GPLv3
