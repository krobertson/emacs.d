;; add the "encoding" local variable to the safe list
(setq safe-local-variable-values (quote ((encoding . utf-8))))

;; linum-mode
(linum-mode t)

;; untabify the buffer on save
(add-hook 'local-write-file-hooks
  (lambda()
    (untabify (point-min) (point-max)) nil))

;; indentation and tab width
(set (make-local-variable 'indent-tabs-mode) 'nil)
(set (make-local-variable 'tab-width) 2)
(setq truncate-lines t)

;; enable ruby-end
(ruby-end-mode)

;; mode map copied from prog-mode
(define-key enh-ruby-mode-map (kbd "s-/")        'comment-or-uncomment-region-or-line)
(define-key enh-ruby-mode-map (kbd "<C-M-down>") 'move-line-down)
(define-key enh-ruby-mode-map (kbd "<C-M-up>")   'move-line-up)
