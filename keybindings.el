(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-r") 'ido-recentf)
(global-set-key (kbd "C-x C-d") 'kr-ido-find-project-file)

(global-set-key (kbd "s-s")       'save-buffer)
(global-set-key (kbd "s-<left>")  'move-beginning-of-line)
(global-set-key (kbd "s-<right>") 'move-end-of-line)
(global-set-key (kbd "s-<up>")    'beginning-of-buffer)
(global-set-key (kbd "s-<down>")  'end-of-buffer)

(global-set-key (kbd "C-a")          'kr-mark-line)
(global-set-key (kbd "<C-return>")   'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)

;; navigation
(global-set-key [C-tab] 'other-window)
(global-set-key [M-tab] (lambda () (interactive) (other-window -1)))

;; jekyll
(global-set-key (kbd "C-c b n") 'jekyll-draft-post)
(global-set-key (kbd "C-c b P") 'jekyll-publish-post)
(global-set-key (kbd "C-c b p") (lambda ()
                                  (interactive)
                                  (ido-find-file-in-dir
                                    (concat jekyll-directory "_posts/"))))
(global-set-key (kbd "C-c b d") (lambda ()
                                  (interactive)
                                  (ido-find-file-in-dir
                                    (concat jekyll-directory "_drafts/"))))

;; general prog-mode
(define-key prog-mode-map (kbd "s-/") 'comment-or-uncomment-region-or-line)
(define-key prog-mode-map (kbd "<C-M-down>") 'move-line-down)
(define-key prog-mode-map (kbd "<C-M-up>") 'move-line-up)
