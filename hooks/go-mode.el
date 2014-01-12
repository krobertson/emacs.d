;; go autocomplete mode
(auto-complete-mode t)

;; tab preferences - use real tabs and 4 width
(set (make-local-variable 'indent-tabs-mode) 't)
(set (make-local-variable 'tab-width) 4)

;; highlight anything over 80 characters
(whitespace-mode)

;; enable flycheck mode
;(load (concat (getenv "GOPATH") "/src/github.com/dougm/goflymake/go-flycheck"))
(flycheck-declare-checker go-fmt
      "A Go syntax and style checker using the gofmt utility."
      :command '("gofmt" source-inplace)
      :error-patterns '(("^\\(?1:.*\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\): \\(?4:.*\\)$" error))
      :modes 'go-mode)
    (add-to-list 'flycheck-checkers 'go-gofmt)
(flycheck-mode)

;; run gofmt on save
(add-hook 'before-save-hook 'gofmt-before-save)
