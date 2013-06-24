;;; open-file-in-github.el --- Open current buffer's file on GitHub

;; Copyright (C) 2009 Michael Ivey & Daniel Jackoway <gweezlebur.com>

;; Version: 0.1.0
;; Keywords: github git open highlight
;; Created: 19 Aug 2009
;; Authors:
;; Michael Ivey
;; Daniel Jackoway

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.


;;; Commentary:

;; Opens the GitHub page for the current file in a browser
;; use M-x open-file-in-github to simply open the file
;; use M-x open-file-in-github-line-highlighted to open it with the current line highlighted
;; use M-x open-file-in-github-region-highlighted to open it with the current region highlighted

;;; Code:


(defun depth-from-git-root ()
  (length (split-string
           (shell-command-to-string "git rev-parse --show-cdup")
           "/")))
 
(defun relative-file-name ()
  (mapconcat 'identity (last
              (split-string (buffer-file-name) "/")
              (depth-from-git-root))
             "/"))
 
(defun my-filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))
 
(defun current-branchp (branch)
  (if (string-match "^\\* " branch)
      t nil))
 
(defun current-git-branch ()
  (substring
   (car (my-filter 'current-branchp
                   (split-string (shell-command-to-string "git branch") "\n")))
   2))


(defun repo-url (git-url)
  (replace-regexp-in-string
   ":" "/"
   (replace-regexp-in-string
    "\.git\n" "" ; strip .git
     (replace-regexp-in-string
     "^.*://" "" ; strip git:// or http://
    (replace-regexp-in-string
     "^.*@" "" ; strip git@
     git-url)))))


(defun github-url ()
  (concat
   "http://" (repo-url (shell-command-to-string "git config remote.origin.url")) "/blob/"
   (current-git-branch) "/" (relative-file-name)))
 
(defun open-file-in-github ()
  (interactive)
  (browse-url (github-url)))

(defun open-file-in-github-line-highlighted ()
  (interactive)
  (browse-url (github-url-line-marked)))

(defun open-file-in-github-region-highlighted ()
  (interactive)
  (browse-url (github-url-region-marked)))

(defun github-url-line-marked ()
  (concat 
   (github-url) "#L" (number-to-string (line-number-at-pos))))

(defun github-url-region-marked ()
  (concat
   (github-url) "#L"
   (number-to-string (min (line-number-at-pos (mark)) (line-number-at-pos (point))))
   "-"
   (number-to-string (max (line-number-at-pos (mark)) (line-number-at-pos (point))))))

(provide 'open-file-in-github)