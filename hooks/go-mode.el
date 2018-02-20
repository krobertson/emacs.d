;; go autocomplete mode
(auto-complete-mode t)

;; tab preferences - use real tabs and 2 width
(set (make-local-variable 'indent-tabs-mode) 't)
(set (make-local-variable 'tab-width) 2)

;; highlight anything over 80 characters
(whitespace-mode)

;; run gofmt on save
(add-hook 'before-save-hook 'gofmt-before-save)

;; enable flycheck mode
(setq-default flycheck-disabled-checkers '(go-golint))
(flycheck-mode)
