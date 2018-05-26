;;; counsel-firefox-bookmarks.el --- Ivy interface of firefox bookmarks  -*- lexical-binding: t; -*-

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

;;** `counsel-firefox-bookmarks`
(defvar counsel-firefox-bookmarks-html-file
  (car (file-expand-wildcards "~/.mozilla/firefox/*/bookmarks.html"))
  "Firefox's auto exported html bookmarks file.")

(defun counsel-firefox-bookmarks-action (x)
  "Browse candidate X."
  (browse-url (get-text-property 0 'href x)))

(defun counsel-firefox-bookmarks--candidates ()
  (let ((candidates))
    (if (and counsel-firefox-bookmarks-html-file (file-exists-p counsel-firefox-bookmarks-html-file))
      (with-temp-buffer
        (insert-file-contents counsel-firefox-bookmarks-html-file)
        (goto-char (point-min))
        (while (re-search-forward "*?<A HREF=\"\\([^\"]+\\)\"[^>]*>\\([^<]+\\)</A>" nil t)
          (let ((a (match-string 0))
                (href (match-string 1))
                (text (match-string 2))
                (tags nil))
            (if (string-match "TAGS=\"\\([^\"]+\\)\"" a)
                (setq tags (match-string 1 a)))
            (push (propertize (format "%s%s" text (if tags (concat "    :" (replace-regexp-in-string "," ":" tags) ":") "")) 'href href) candidates))))
      (warn "`counsel-firefox-bookmarks-html-file` not exists"))
      candidates))

;;;###autoload
(defun counsel-firefox-bookmarks ()
  "Ivy interface of firefox bookmarks.
You will have to enable html bookmarks in firefox:
open \"about:config\" in firefox and double click on this line to enable value
to true:

    user_pref(\"browser.bookmarks.autoExportHTML\", false);

You should have now:

    user_pref(\"browser.bookmarks.autoExportHTML\", true);

After closing firefox, you will be able to browse your bookmarks."
  (interactive)
  (ivy-read "Firefox Bookmarks: " (counsel-firefox-bookmarks--candidates)
            :history 'counsel-firefox-bookmarks-history
            :action 'counsel-firefox-bookmarks-action
            :caller 'counsel-firefox-bookmarks
            :require-match t))

(provide 'counsel-firefox-bookmarks)
;;; counsel-firefox-bookmarks.el ends here
