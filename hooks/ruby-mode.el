(add-hook 'local-write-file-hooks
          '(lambda()
             (save-excursion
               (untabify (point-min) (point-max)))))
(set (make-local-variable 'indent-tabs-mode) 'nil)
(set (make-local-variable 'tab-width) 2)
(ruby-electric-mode t)
(setq truncate-lines t)
