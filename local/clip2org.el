;;; clip2org.el --- Convert Kindle's My Clippings.txt into Org

;; Author: Thamer Mahmoud <thamer.mahmoud@gmail.com>
;; Version: 1.1
;; Time-stamp: <2012-05-28 10:53:45 thamer>
;; URL: https://github.com/thamer/clip2org
;; Keywords: Kindle, Org mode, Amazon, My Clippings.txt
;; Compatibility: Tested on GNU Emacs 23.4 and 24.1
;; Copyright (C) 2012 Thamer Mahmoud, all rights reserved.

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This package converts Kindle's "My Clippings.txt" to a format
;; useable in Org mode. The result will be sorted by book title and
;; displayed in a temporary buffer named "*clippings*".
;;
;;; Install:
;;
;; Put this file in your Emacs-Lisp load path, and add the following
;; into your $HOME/.emacs startup file.
;;
;;     (require 'clip2org)
;;     (setq clip2org-clippings-file "/path/to/My Clippings.txt")
;;
;; You can also use the customize interface to see all the options:
;;
;;     M-x customize-group <ENTER> clip2org
;;
;;; Usage:
;;
;; After setting clip2org-clippings-file, do:
;;
;;     M-x clip2org
;;
;;; Code
(defgroup clip2org nil "clip2org group"
  :group 'org)

(defcustom clip2org-clippings-file
  (convert-standard-filename "~/My Clippings.txt")
  "Path to My Clippings.txt, including file name."
  :type 'file
  :group 'clip2org)

(defcustom clip2org-include-pdf-links nil
  "If t, add PDF page links under each clipping. See also
clip2org-include-pdf-folder."
  :type 'boolean
  :group 'clip2org)

(defcustom clip2org-include-pdf-folder ""
  "Folder used to generate page links to pdf files."
  :type 'file
  :group 'clip2org)

(defcustom clip2org-include-date nil
  "If t, include added date information as an Org property."
  :type 'boolean
  :group 'clip2org)

(defcustom clip2org-skip-bookmarks t
  "If t, Bookmarks are ignored from the clippings file."
  :type 'boolean
  :group 'clip2org)

(defcustom clip2org-clipping-tags nil
  "When non-nil the string is used as a tag for clippings."
  :type 'string
  :group 'clip2org)

(defcustom clip2org-persistence-file
  (expand-file-name "clip2org-persist.txt" user-emacs-directory)
  "Path of the file where data of clip2org runs is persisted."
  :type 'file
  :group 'clip2org)

(defun clip2org-get-next-book-as-list ()
  (let (title is-highlight header loc date page start end content)
    (setq start (point))
    (when (re-search-forward "==========" nil t 1)
      (setq end (point))
      (goto-char start)
      (setq title (buffer-substring-no-properties
                   (line-beginning-position)
                   (line-end-position)))
      (when (re-search-forward "Highlight" end t 1)
        (setq is-highlight t))
      (beginning-of-line)
      (when (re-search-forward "- \\(.*\\)|" end t 1)
        (setq header (match-string 1)))
      (beginning-of-line)
      ;;FIXME: pages can be roman numerals also.
      (when (re-search-forward "Page \\([0-9-]+\\)" end t 1)
        (setq page (match-string 1)))
      (when (re-search-forward "Loc.*? \\([0-9-]+\\)" end t 1)
        (setq loc (match-string 1)))
      (when (re-search-forward "Added on \\(.*\\)\n" end t 1)
        (setq date (match-string 1)))
      ;; From the end of date to ==========
      (if (re-search-forward
           "\n\\(.*?\\)\n==========" end t 1)
          (setq content (match-string 1)))
      (when (equal title "==========")
        (error "Clip2org: failed in getting content or quoted text."))
      (message (format "Clip2org: now processing \"%s\"" title))
      (forward-line)

      ;; Return assoc list
      `((title . ,title)
        (is-highlight . ,is-highlight)
        (page . ,page)
        (loc . ,loc)
        (date . ,date)
        (content . ,content)
        (header . ,header)))))

(defun clip2org-convert-to-org (clist all)
  "Process clip2org-alist and generate the output buffer."
  (with-current-buffer (get-buffer-create "*clippings*")
    (delete-region (point-min) (point-max))
    (org-mode)

    (let ((last-run (and (not all)
                         (clip2org--get-last-run-timestamp))))

      (when last-run
        (setq last-run (apply 'encode-time (org-parse-time-string last-run))))

      ;; Process each book
      (dolist (book clist)
        (let ((note-list
               (remove-if
                '(lambda (x) (clip2org--skip-clip x last-run))
                (cdr book))))

          (when note-list
            (insert "\n* " (car book)))

          ;; Process each clipping
          (dolist (item note-list)
            (let ((is-highlight (cdr (assoc 'is-highlight item)))
                  (page (cdr (assoc 'page item)))
                  (loc (cdr (assoc 'loc item)))
                  (date (cdr (assoc 'date item)))
                  (content (cdr (assoc 'content item))))

              (insert "\n- " content)
              (fill-paragraph)

              (when clip2org-include-date
                (org-set-property "DATE"
                                  (format-time-string
                                   (concat "[" (substring (cdr org-time-stamp-formats) 1 -1) "]")
                                   (org-read-date t t date))))

              ;; (org-set-property "PAGE" page)
              ;; (org-set-property "LOCATION" loc)
              ;; (org-set-property "BOOK" (car book))

              (when clip2org-clipping-tags
                (org-set-tags-to clip2org-clipping-tags))

              ;; Insert pdf link
              (if (and clip2org-include-pdf-links page)
                  (insert (concat "[[docview:" clip2org-include-pdf-folder
                                  (caar clist) ".pdf"
                                  "::" page "][View Page]]\n")))))))))

  (switch-to-buffer "*clippings*"))

(defun clip2org-append-to-alist-key (key value alist)
  "Append a value to the key part of an alist. This function is
used to create associated lists. If Key is not found, add new key
to the list"
  (let ((templ) (results) (found))
    (while alist
      ;; check if key is already in list
      (if (equal (caar alist) key)
          (progn
            (setq found t)
            (setq templ (list (nconc (car alist) (list value) templ)))
            ;; increment while loop
            (setq alist (cdr alist))
            ;; add/create to a new list
            (setq results (append results templ)))
        (progn
          (setq results (append (list (car alist)) results))
          (setq alist (cdr alist)))))
    ;; add the new key/value to old list
    (if (not (eq found t))
        (setq results (append (list (list key value)) results)))
    results))

(defun clip2org--is-bookmark (booklist)
  "Returns t if is-highlight is false"
  ;FIXME: not is-highlight may mean a note as well? not just a bookmark?
  (not (cdr (assoc 'is-highlight booklist))))

(defun clip2org--save-last-run-timestamp (&optional timestamp)
  "Save the timestamp to last-run file."
  (with-temp-file clip2org-persistence-file
    (org-insert-time-stamp (or timestamp (current-time)) t t)))

(defun clip2org--get-last-run-timestamp ()
  "Return the timestamp of the last run.

Returns nil if there is no data for last run."
  (when (file-exists-p clip2org-persistence-file)
    (with-temp-buffer
      (save-match-data
        (insert-file clip2org-persistence-file)
        (when (org-at-timestamp-p t)
          (match-string 1))))))

(defun clip2org--skip-clip (clip &optional last-run-ts)
  "Return t if the clip should be skipped from the org tree"

  (or (and clip2org-skip-bookmarks
           (clip2org--is-bookmark clip))

      (and last-run-ts (time-less-p
                        (org-read-date t t (cdr  (assoc 'date clip)))
                        last-run-ts))))

(defun clip2org (&optional all clipping-file)
  "Parse clippings and convert to org headlines.

Only converts the clippings which have been added after the last
time this command was run.  If ALL is non-nil, converts all the
clippings.  The last run timestamp is updated only if ALL is nil.

CLIPPING-FILE is a path to the clipping file. If none is
provided, the `clip2org-clippings-file' value is used.
"
  (interactive "P")
  (save-excursion
    (with-temp-buffer
      (insert-file (or clipping-file clip2org-clippings-file))
      (goto-char (point-min))
      (let (clist (booklist (clip2org-get-next-book-as-list)))
        (while booklist
          (setq clist (clip2org-append-to-alist-key
                       (cdr (assoc 'title booklist))
                       booklist
                       clist))
          (setq booklist (clip2org-get-next-book-as-list)))
        (clip2org-convert-to-org clist all))))

  (unless all
    (clip2org--save-last-run-timestamp)))


(provide 'clip2org)
;;; clip2org.el ends here.
