;;; counsel-firefox-bookmarks.el --- Complete Firefox bookmarks with Ivy  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2018  Free Software Foundation, Inc.

;; Author: tangxinfa <tangxinfa@gmail.com>
;; Keywords: matching

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'ivy)

;;** `counsel-firefox-bookmarks'
(defvar counsel-firefox-bookmarks-file
  (car (file-expand-wildcards "~/.mozilla/firefox/*/bookmarks.html"))
  "Firefox's automatically exported HTML bookmarks file.")

(defface counsel-firefox-bookmarks-tag
  '((t :inherit font-lock-comment-face))
  "Face used by `counsel-firefox-bookmarks' for tags."
  :group 'ivy-faces)

(defun counsel-firefox-bookmarks-action (x)
  "Browse candidate X."
  (browse-url (cdr x)))

(declare-function xml-substitute-special "xml")

(defun counsel-firefox-bookmarks--candidates ()
  "Return list of `counsel-firefox-bookmarks' candidates."
  (unless (and counsel-firefox-bookmarks-file
               (file-readable-p counsel-firefox-bookmarks-file))
    (signal 'file-error (list "Opening `counsel-firefox-bookmarks-file'"
                              "No such readable file"
                              counsel-firefox-bookmarks-file)))
  (require 'xml)
  (with-temp-buffer
    (insert-file-contents counsel-firefox-bookmarks-file)
    (let ((case-fold-search t)
          candidates)
      (while (re-search-forward
              "<a href=\"\\([^\"]+?\\)\"[^>]*?>\\([^<]*?\\)</a>" nil t)
        (let* ((a (match-string 0))
               (href (match-string 1))
               (text (if (= (match-beginning 2) (match-end 2))
                         href
                       (save-match-data
                         (xml-substitute-special (match-string 2)))))
               (tags (and (string-match "tags=\"\\([^\"]+?\\)\"" a)
                          (mapconcat
                           (lambda (tag)
                             (put-text-property 0 (length tag) 'face
                                                'counsel-firefox-bookmarks-tag
                                                tag)
                             tag)
                           (split-string (match-string 1 a) "," t)
                           ":"))))
          (push (cons (if tags (concat text " :" tags ":") text)
                      href)
                candidates)))
      candidates)))

;;;###autoload
(defun counsel-firefox-bookmarks ()
  "Complete Firefox bookmarks with Ivy.
This requires HTML bookmark export to be enabled in Firefox.
To do this, open URL `about:config' in Firefox, make sure that
the value of the setting \"browser.bookmarks.autoExportHTML\" is
\"true\" by, say, double-clicking it, and then restart Firefox."
  (interactive)
  (ivy-read "bookmark: " (counsel-firefox-bookmarks--candidates)
            :history 'counsel-firefox-bookmarks-history
            :action #'counsel-firefox-bookmarks-action
            :caller 'counsel-firefox-bookmarks
            :require-match t))

(provide 'counsel-firefox-bookmarks)
;;; counsel-firefox-bookmarks.el ends here
