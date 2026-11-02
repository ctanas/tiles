# warcel

Browse WARC web archives in Emacs.

Warcel lists the records of a WARC (Web ARChive) file and shows the
archived pages as Org documents or rendered by EWW.  Scripts, styles,
images, video, audio, frames and form controls are removed before a
page is shown, so viewing a page never runs code or fetches anything
from the network.

Warcel has no dependencies: plain and gzip-compressed WARC files are
read with Emacs's built-in zlib support, and HTML is parsed with its
built-in libxml2 support.  It needs Emacs 29.1 or later, built with
both (most builds are; check with `(zlib-available-p)` and
`(libxml-available-p)`).

## Installation

The simplest way is to install `warcel.el` as a package:

    M-x package-install-file RET ~/path/to/warcel/warcel.el RET

This byte-compiles it and sets everything up when Emacs starts; run it
again after updating `warcel.el`.

Or run `make` in the warcel directory, which byte-compiles `warcel.el`
and writes `warcel-autoloads.el`, and add to your init file:

```elisp
(add-to-list 'load-path "~/path/to/warcel")
(require 'warcel-autoloads)
```

Either way, warcel itself is only loaded when first used.  The
autoloads make `.warc` and `.warc.gz` files open with warcel, and let
Emacs read `.warc.gz` files with its own zlib: otherwise it would run
the `gzip` program to read them, as it does for other compressed
files.  Options can then be set as usual, for example:

```elisp
(setq warcel-default-view 'eww
      warcel-capture-directory "~/warc/")
```

## Usage

To save a web page: copy its URL, run `M-x warcel-capture` and press
`RET` (see [Capturing pages](#capturing-pages)).

To browse an archive, `M-x warcel-open` asks for a `.warc` or
`.warc.gz` file.  An archive that holds a single page opens on that
page, as set by `warcel-default-view`; other archives open on the list
of their records.  Visiting a `.warc` or `.warc.gz` file with `C-x C-f`
does the same.

Only HTML pages count: the images, styles and scripts saved with a
page, redirects to it and error pages do not.  Archives made by
`warcel-capture` hold a single page unless you add pages to them, so
they always open on the page.  In a page, `^` lists the records of the
archive, and `C-u M-x warcel-open` lists them straight away.

For large archives, `warcel-open` is the better way in: it reads only
the parts of the file it needs, while `C-x C-f` first reads the whole
file, decompressed, into a buffer.  In Dired, `RET` on a `.warc` or
`.warc.gz` file uses `warcel-open`, and shows the archive again if it
is already open; other files open as usual.  To visit archives from
Dired as other files instead:

```elisp
(with-eval-after-load 'dired
  (keymap-unset dired-mode-map "<remap> <dired-find-file>"))
```

### The record list

| Key   | Command                     |                                                                 |
|-------|-----------------------------|-----------------------------------------------------------------|
| `RET` | `warcel-open-record`        | Show the record, as set by `warcel-default-view`                |
| `o`   | `warcel-show-org`           | Show the page as an Org document                                |
| `e`   | `warcel-show-eww`           | Show the page with EWW                                          |
| `v`   | `warcel-view-source`        | Show the record as stored: WARC and HTTP headers, then payload  |
| `s`   | `warcel-save-payload`       | Save the payload (decompressed) to a file                       |
| `a`   | `warcel-toggle-all-records` | List all records, or only the types in `warcel-record-types`    |
| `/`   | `warcel-filter`             | Only list records whose URI or content type match a regexp      |
| `S`   | `tabulated-list-sort`       | Sort by the column at point                                     |
| `g`   | `revert-buffer`             | Read the file again                                             |

By default only `response`, `resource`, `revisit` and `conversion`
records are listed; `a` shows the `warcinfo`, `request` and `metadata`
records as well.

Revisit records show the record they refer to, and redirects show
their target when it is in the archive.  Images, audio and video are
not shown; use `s` to save them.  Other text payloads (CSS, JSON, plain
text…) open in a buffer of the matching mode.

### Org view

Pages become Org documents: headings, paragraphs, bold, italic,
underline, strike-through, code, links, lists, description lists,
tables, quotes and preformatted text keep their meaning.  The target
URI and capture date are in the document's property drawer.  Tables
used only for page layout become a sequence of paragraphs.

By default only the main content of a page is shown, as the reading
view of a browser shows it: the title of the article and its text,
without the menus, headers, footers, sidebars, sharing buttons, lists
of related pages, comments and cookie notices around it.  `R` switches
between that and the whole page.  The article is the part of the page
that holds most of its paragraphs, with the article or main element
around it; when a page has none, as behind a paywall, it is shown
without its menus, headers and footers.  Set `warcel-readable` to nil
to show whole pages by default.

`RET` or a mouse click on a link opens the archived copy of the linked
page when the archive has one.  `q` goes back, `^` lists the records of
the archive, `R` switches between the main content and the whole page,
`e` shows the same page with EWW and `v` shows its source.

### EWW view

Pages are rendered by EWW, which keeps its usual commands: `l` and `r`
go back and forward, `R` shows the readable part of the page, `w`
copies the URL.  Following a link opens the archived copy of the
linked page; `g` shows the page from the archive again, `^` lists the
records of the archive, `v` shows the record's source and `o` shows
the page as an Org document.

### Links to pages that are not in the archive

`warcel-follow-live-links` says what happens then: `ask` (the default)
asks before opening the live page with `browse-url`, `t` opens it
without asking and `nil` only reports it.

## Capturing pages

`M-x warcel-capture` saves a web page in a new WARC archive and shows
it.  It asks for the URL, offering the one in the clipboard (or at the
head of the kill ring) as the default, so copying a URL and pressing
`RET` is enough.

The archive is saved in `warcel-capture-directory` (the current
directory when nil), named after the host and the time, such as
`en.wikipedia.org-20260923-224650.warc.gz`.  With a prefix argument,
`C-u M-x warcel-capture` asks for the archive file; choosing an
existing archive adds the page to it, so a collection of pages can be
built up in one file; the record list is then shown, on
the new page.

The page is fetched by Emacs itself (over TLS with its built-in GnuTLS
support), and the archive holds the request and the response exactly
as sent and received, with the redirects that led to the page.  Only
the page itself is saved: its images, styles and scripts are not,
since warcel does not show them.

Archives are gzip-compressed record by record, the usual form of
`.warc.gz` files, which other WARC tools read as well.  Emacs can only
decompress data, so warcel has a small DEFLATE compressor of its own
written in Emacs Lisp.  Pages are asked for compressed, and those that
servers send compressed are stored as they are; the rest compresses
to about the size `gzip` makes (80 KB instead of 75 KB for a 400 KB
Wikipedia page, in 0.2 s once `warcel.el` is byte-compiled).  Set
`warcel-capture-compress` to nil for plain `.warc` files; the archive
you choose with `C-u` is compressed if its name ends in `.gz`, and
pages added to an existing archive keep its format.

Pages are fetched without cookies, logins or JavaScript, so pages
that need those are saved as the server sends them to a new visitor.
`url-proxy-services` is not used.

### From Lisp

`warcel-capture-in-background` saves a page the same way without
waiting for it or showing it, and calls a function when it is done:

```elisp
(warcel-capture-in-background
 "https://example.com/" "~/warc/example.warc.gz"
 (lambda (file status problem)
   (message "%s" (or problem (format "Saved in %s (%d)" file status)))))
```

The archive can also be given as a function of the HTTP status,
called once the page has arrived, that returns the file or nil not to
save the page.  `warcel-archived-urls` returns the URLs of the pages
in an archive, in the form `warcel-canonical-url` gives, so a program
can tell which pages it has saved already.  TILES uses these to
archive the pages its notes link to.

## Supported archives

- Uncompressed WARC files.
- Gzip-compressed WARC files, whether each record has its own gzip
  member (the usual way), several records share one, the whole file is
  one gzip stream, or records are split across members.
- HTTP payloads with chunked transfer coding and gzip or deflate
  content coding.
- Truncated files, and records shorter than their `Content-Length`
  says, as far as they can be read.

Not supported: Zstandard-compressed WARC files (`.warc.zst`), the older
ARC format, and Brotli-encoded payloads.

## Customization

- `warcel-readable`: whether the Org view shows only the main content
  of pages (the default) or all of them.
- `warcel-default-view`: `org` (the default) or `eww`, for `RET` on
  HTML pages.
- `warcel-record-types`: record types listed by default.
- `warcel-follow-live-links`: see above.
- `warcel-capture-directory`: where `warcel-capture` saves new archives.
- `warcel-capture-compress`: whether new archives are `.warc.gz` (the
  default) or `.warc` files.
- `warcel-capture-timeout`: seconds to wait for a server that stopped
  sending data (30).
- `warcel-capture-user-agent`: the `User-Agent` header sent.

## Development

Byte-compiling `warcel.el` makes compressing pages about five times
faster.

```sh
make compile   # byte-compile, failing on warnings
make test      # run the ERT tests
make checkdoc
```
