;; go autocomplete mode
(auto-complete-mode t)
(use-package go-autocomplete
  :ensure t)

;; tab preferences - use real tabs and 4 width
(set (make-local-variable 'indent-tabs-mode) 't)
(set (make-local-variable 'tab-width) 4)

;; highlight anything over 80 characters
(whitespace-mode)

;; enable flycheck mode
;(load (concat (getenv "GOPATH") "/src/github.com/dougm/goflymake/go-flycheck"))
(flycheck-mode)

;; run gofmt on save
(add-hook 'before-save-hook 'gofmt-before-save)
