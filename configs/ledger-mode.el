; Load the ledger 3.0.3 ledger-mode files. The files are currently vendored
; rather than brought in from the install, since the install location may vary
; depending on the machine, and upgrading to ledger 3.1 brought in some weird
; changes to the highlighting, so wanted to go back to the 3.0.3 files.
(add-to-list 'load-path
             (expand-file-name (emacs-d "vendor/ledger-mode/")))
(load "ledger-mode")

; I use .ledger files. Add them to the auto mode list.
(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))
