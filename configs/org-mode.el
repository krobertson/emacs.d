;; load org-mode
(add-to-list 'load-path (emacs-d "vendor/org-mode/lisp"))
(add-to-list 'load-path (emacs-d "vendor/org-mode/contrib/lisp"))
(require 'org)

(setq org-directory "~/Dropbox/org")
(setq org-default-notes-file (concat org-directory "/personal.org"))
(setq org-agenda-files (concat org-directory "/*.org"))
(setq org-mobile-inbox-for-pull (concat org-directory "/flagged.org"))
(setq org-mobile-directory (concat org-directory "/MobileOrg"))

(defun org-make-slug (s)
  "Turn a string into a slug."
  (replace-regexp-in-string
   " " "-" (downcase
            (replace-regexp-in-string
             "[^A-Za-z0-9 ]" "" s))))

(defun kr-new-apcera-note (title)
  "Prompts for a new note org file to create"
  (interactive "sTitle: ")
  (let ((note-file (concat org-directory "/apcera/" (org-make-slug title) ".org")))
    (if (file-exists-p note-file)
        (find-file note-file)
      (find-file note-file)
      (insert (format "#+Title: %s\n\n" title)))))

(defun kr-org-insert-bullet ()
  "Inserts a new line and bullet on the next line."
  (interactive)
  (move-end-of-line nil)
  (insert "\n+ "))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

(setq org-capture-templates
  '(("t" "Todo" entry (file+headline (concat org-directory "/personal.org") "Tasks")
      "** TODO %?\n  %i\n  %a")
    ("tl" "Todo w/ Link" entry (file+headline (concat org-directory "/personal.org") "Tasks")
      "** TODO %?\n  %i\n  %a")
    ("n" "Note" entry (file+datetree (concat org-directory "/notes.org") "Notes")
      "* %U %?\n  %i\n  %a")
    ("j" "Journal" entry (file+datetree "~/org/journal.org")
      "* %?\nEntered on %U\n  %i\n  %a")))

;; reset org-mode's Control-tab to cycle windows. Was org-force-cycle-archived
(define-key org-mode-map [C-tab] 'other-window)
(define-key org-mode-map (kbd "S-s-<return>") 'kr-org-insert-bullet)
