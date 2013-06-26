;; untabify the buffer on save
(add-hook 'local-write-file-hooks
  (lambda()
    (untabify (point-min) (point-max)) nil))

;; add the "encoding" local variable to the safe list
(setq safe-local-variable-values (quote ((encoding . utf-8))))

(set (make-local-variable 'indent-tabs-mode) 'nil)
(set (make-local-variable 'tab-width) 2)
(ruby-electric-mode t)
(setq truncate-lines t)
