(add-to-list 'load-path
             (expand-file-name (emacs-d "vendor/ebuild-mode/")))
(load "ebuild-mode")

(add-to-list 'auto-mode-alist '("\\.ebuild$" . ebuild-mode))
