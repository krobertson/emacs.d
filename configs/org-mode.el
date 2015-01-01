;; load org-mode
(add-to-list 'load-path (emacs-d "vendor/org-mode/lisp"))
(add-to-list 'load-path (emacs-d "vendor/org-mode/contrib/lisp"))
(require 'org)

(setq org-directory "~/Dropbox/org")
(setq org-default-notes-file (concat org-directory "/personal.org"))
(setq org-mobile-inbox-for-pull (concat org-directory "/flagged.org"))
(setq org-mobile-directory (concat org-directory "/MobileOrg"))
(setq org-agenda-files
      (delq nil
            (mapcar (lambda (x) (and (file-exists-p x) x))
                    '("~/Dropbox/org/personal.org"))))

(defun org-make-slug (s)
  "Turn a string into a slug."
  (replace-regexp-in-string
   " " "-" (downcase
            (replace-regexp-in-string
             "[^A-Za-z0-9 ]" "" s))))

(defun kr-org-insert-bullet ()
  "Inserts a new line and bullet on the next line."
  (interactive)
  (move-end-of-line nil)
  (insert "\n+ "))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

;(setq org-capture-templates
;  '(("t" "Todo" entry (file+headline (concat org-directory "/personal.org") "Tasks")
;;      "** TODO %?\n  %i\n  %a")
;    ("tl" "Todo w/ Link" entry (file+headline (concat org-directory "/personal.org") "Tasks")
;      "** TODO %?\n  %i\n  %a")
;    ("n" "Note" entry (file+datetree (concat org-directory "/notes.org") "Notes")
;      "* %U %?\n  %i\n  %a")
;    ("j" "Journal" entry (file+datetree "~/org/journal.org")
;      "* %?\nEntered on %U\n  %i\n  %a")))

(defvar sacha/org-basic-task-template "* TODO %^{Task}
SCHEDULED: %^t
%?
:PROPERTIES:
:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
:END:" "Basic task data")
(setq org-capture-templates
      `(("t" "Tasks" entry
         (file+headline "~/Dropbox/org/organizer.org" "Tasks")
         ,sacha/org-basic-task-template)
        ("p" "People task" entry
         (file+headline "~/Dropbox/org/people.org" "Tasks")
         ,sacha/org-basic-task-template)
        ("dp" "Done - People" entry
         (file+headline "~/Dropbox/org/people.org" "Tasks")
         "* DONE %^{Task}\nSCHEDULED: %^t\n%?")
        ("dt" "Done - Task" entry
         (file+headline "~/Dropbox/org/organizer.org" "Tasks")
         "* DONE %^{Task}\nSCHEDULED: %^t\n%?")
        ("q" "Quick note" item
         (file+headline "~/Dropbox/org/organizer.org" "Quick notes"))
        ("l" "Ledger entries")
        ("lm" "MBNA" plain
         (file "~/Dropbox/personal/financial/current.ledger")
         "%(org-read-date) %^{Payee}
  Liabilities:MBNA
  Expenses:%^{Account}  $%^{Amount}
" :immediate-finish)
        ("ln" "No Frills" plain
         (file "~/Dropbox/personal/financial/current.ledger")
         "%(let ((org-read-date-prefer-future nil)) (org-read-date)) * No Frills
  Liabilities:MBNA
  Assets:Wayne:Groceries  $%^{Amount}
" :immediate-finish)
        ("lc" "Cash" plain
         (file "~/Dropbox/personal/financial/current.ledger")
         "%(org-read-date) * %^{Payee}
  Expenses:Cash
  Expenses:%^{Account}  %^{Amount}
")
         ("c" "Contact" entry (file "~/Dropbox/org/contacts.org")
          "* %(org-contacts-template-name)
:PROPERTIES:
:EMAIL: %(org-contacts-template-email)
:END:")
         ("n" "Daily note" table-line (file+olp "~/Dropbox/org/organizer.org" "Daily notes")
          "| %u | %^{Note} |"
          :immediate-finish)
         ("r" "Notes" entry
          (file+datetree "~/Dropbox/org/organizer.org")
          "* %?\n\n%i\n"
          )))

;; reset org-mode's Control-tab to cycle windows. Was org-force-cycle-archived
(define-key org-mode-map [C-tab] 'other-window)
(define-key org-mode-map (kbd "S-s-<return>") 'kr-org-insert-bullet)
