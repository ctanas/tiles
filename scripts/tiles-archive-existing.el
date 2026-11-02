#!/usr/bin/env -S emacs -Q --script
;;; tiles-archive-existing.el --- Archive the pages linked from existing notes  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; With `tiles-archive-urls' on, saving a note archives the web pages it
;; links to.  This script does the same for the notes already in a TILES
;; directory, with the same rules: links archived already are skipped,
;; links to excluded hosts are left alone, only pages the server answers
;; with success are saved, and each page goes in warc/YYYY/MM/ next to
;; its note, named after the note with a letter added.
;;
;; Usage, from the tiles-src directory:
;;
;;   scripts/tiles-archive-existing.el --notes ~/Emacs/tiles \
;;     --exclude parsec.ro --exclude-type pdf
;;
;; Options:
;;
;;   --notes DIR          the TILES directory (default: `tiles-directory')
;;   --exclude HOST       do not archive links to HOST and its subdomains
;;   --exclude-type EXT   do not archive files of the type with the
;;                        extension EXT, such as pdf
;;   --dry-run            only list the links that would be archived
;;
;; --exclude and --exclude-type can be repeated; give the same ones as
;; `tiles-archive-exclude-hosts' and `tiles-archive-exclude-file-types'
;; in your init file.
;;
;; Pages are downloaded one at a time, newest notes first.  The script
;; can be stopped with C-c and run again: it carries on with the links
;; not archived yet.  The links that could not be archived are listed at
;; the end; the next run tries them again, and so does saving their note.
;;
;; Avoid adding links to notes in Emacs while the script runs: both
;; could then archive the same page.

;;; Code:

(setq load-prefer-newer t)
(require 'tiles (expand-file-name "../tiles" (file-name-directory load-file-name)))

(defun tiles-archive-existing--fail (format-string &rest args)
  "Print FORMAT-STRING formatted with ARGS and exit with an error status."
  (message "%s" (apply #'format format-string args))
  (kill-emacs 1))

(defun tiles-archive-existing--name (note)
  "Return the name of NOTE relative to `tiles-directory'."
  (file-relative-name note tiles-directory))

(defun tiles-archive-existing--pending ()
  "Return the notes with links to archive, as (NOTE . URLS), newest first."
  (delq nil (mapcar (lambda (note)
                      (when-let* ((urls (tiles--note-pending-urls note)))
                        (cons (expand-file-name note) urls)))
                    (tiles--get-all-tile-files))))

(defun tiles-archive-existing--count (pending)
  "Return the number of links in PENDING."
  (apply #'+ (mapcar (lambda (entry) (length (cdr entry))) pending)))

(defun tiles-archive-existing--elapsed (start)
  "Return the time since START, as minutes and seconds."
  (let ((seconds (round (- (float-time) start))))
    (format "%dm%02ds" (/ seconds 60) (% seconds 60))))

(let ((args command-line-args-left)
      (dry-run nil)
      (excluded nil)
      (excluded-types nil))
  ;; Keep Emacs from taking the arguments for files to visit.
  (setq command-line-args-left nil)
  (while args
    (pcase (pop args)
      ("--notes"
       (setq tiles-directory
             (file-name-as-directory
              (expand-file-name
               (or (pop args) (tiles-archive-existing--fail "--notes needs a directory"))))))
      ("--exclude"
       (push (or (pop args) (tiles-archive-existing--fail "--exclude needs a host name"))
             excluded))
      ("--exclude-type"
       (push (or (pop args) (tiles-archive-existing--fail "--exclude-type needs an extension"))
             excluded-types))
      ("--dry-run" (setq dry-run t))
      (arg (tiles-archive-existing--fail "Unknown argument: %s" arg))))
  (unless (fboundp 'warcel-capture-in-background)
    (tiles-archive-existing--fail "Warcel is not available: it needs Emacs 29.1 or later"))
  (unless (file-directory-p tiles-directory)
    (tiles-archive-existing--fail "No such directory: %s" tiles-directory))
  (setq tiles-archive-urls t
        tiles-archive-exclude-hosts excluded
        tiles-archive-exclude-file-types excluded-types
        ;; Nobody is typing: start each download at once.
        tiles--archive-idle-delay 0
        ;; Leave the cache of the running Emacs alone.
        tiles-cache-file nil
        tiles-watch-files nil)

  (let* ((start (float-time))
         (notes (length (tiles--get-all-tile-files)))
         (pending (tiles-archive-existing--pending))
         (total (tiles-archive-existing--count pending)))
    (message "%d notes in %s; %d links to archive, from %d notes%s"
             notes (abbreviate-file-name tiles-directory) total (length pending)
             (if (or excluded excluded-types)
                 (format " (excluding %s)"
                         (string-join (append (reverse excluded)
                                              (mapcar (lambda (type) (concat "." type))
                                                      (reverse excluded-types)))
                                      ", "))
               ""))
    (cond
     ((zerop total))
     (dry-run
      (dolist (entry pending)
        (message "%s" (tiles-archive-existing--name (car entry)))
        (dolist (url (cdr entry))
          (message "  %s" url))))
     (t
      (setq tiles--archive-queue
            (mapcan (lambda (entry)
                      (mapcar (lambda (url) (cons (car entry) url)) (cdr entry)))
                    pending))
      (tiles--archive-next)
      (let ((reported (float-time)))
        (while (or tiles--archive-current tiles--archive-queue)
          (accept-process-output nil 0.2)
          (when (> (- (float-time) reported) 60)
            (setq reported (float-time))
            (message "-- %d of %d links done, %s"
                     (- total (length tiles--archive-queue)
                        (if tiles--archive-current 1 0))
                     total (tiles-archive-existing--elapsed start)))))
      ;; What is still pending could not be archived; the links found
      ;; to be of an excluded type are not pending any more.
      (let* ((left (tiles-archive-existing--pending))
             (failed (tiles-archive-existing--count left))
             (skipped (apply #'+ (mapcar (lambda (entry)
                                           (seq-count (lambda (url)
                                                        (gethash url tiles--archive-skipped))
                                                      (cdr entry)))
                                         pending))))
        (message "Done in %s: %d of %d links archived%s"
                 (tiles-archive-existing--elapsed start)
                 (- total failed skipped) total
                 (if (zerop skipped)
                     ""
                   (format ", %d skipped for their file type" skipped)))
        (when left
          (message "Not archived (%d), tried again on the next run or save:" failed)
          (dolist (entry left)
            (dolist (url (cdr entry))
              (message "  %s  %s" (tiles-archive-existing--name (car entry)) url)))))))))

;;; tiles-archive-existing.el ends here
