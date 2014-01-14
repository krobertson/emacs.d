(setq org-src-fontify-natively t)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook
  '(lambda() (set-fill-column 80)))

(require 'ox-confluence)
(require 'ox-html)

;; always use auto-fill-mode in org-mode
(auto-fill-mode t)
