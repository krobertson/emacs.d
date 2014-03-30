(global-set-key (kbd "C-x C-b")   'ibuffer)
(global-set-key (kbd "C-x C-r")   'ido-recentf)
(global-set-key (kbd "C-x C-d")   'kr-ido-find-project-file)

(global-set-key (kbd "s-s")       'save-buffer)
(global-set-key (kbd "s-w")       'delete-frame)
(global-set-key (kbd "s-q")       'save-buffers-kill-emacs)
(global-set-key (kbd "s-n")       'new-frame)
(global-set-key (kbd "s-v")       'yank)
(global-set-key (kbd "s-c")       'kill-ring-save)

(global-set-key (kbd "s-x")       'kill-region)
(global-set-key (kbd "s-a")       'mark-whole-buffer)
(global-set-key (kbd "s-<left>")  'smart-line-beginning)
(global-set-key (kbd "s-<right>") 'move-end-of-line)
(global-set-key (kbd "s-<up>")    'beginning-of-buffer)
(global-set-key (kbd "s-<down>")  'end-of-buffer)
(global-set-key (kbd "<home>")    'smart-line-beginning)

(global-set-key (kbd "S-s-<left>") 'smart-move-line-beginning)

(global-set-key (kbd "C-a")          'kr-mark-line)
(global-set-key (kbd "C-k")          'kr-delete-line)
(global-set-key (kbd "<C-return>")   'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)

;; searching
(global-set-key (kbd "C-f")       'isearch-forward)
(global-set-key (kbd "s-f")       'isearch-repeat-forward)

;; navigation
(global-set-key [C-tab] 'other-window)
(global-set-key [M-tab] (lambda () (interactive) (other-window -1)))

;; jekyll
; (global-set-key (kbd "C-c b n") 'jekyll-draft-post)
; (global-set-key (kbd "C-c b P") 'jekyll-publish-post)
; (global-set-key (kbd "C-c b p") (lambda ()
;                                   (interactive)
;                                   (ido-find-file-in-dir
;                                     (concat jekyll-directory "_posts/"))))
; (global-set-key (kbd "C-c b d") (lambda ()
;                                   (interactive)
;                                   (ido-find-file-in-dir
;                                     (concat jekyll-directory "_drafts/"))))

;; general prog-mode
(define-key prog-mode-map (kbd "s-/")        'comment-or-uncomment-region-or-line)
(define-key prog-mode-map (kbd "<C-M-down>") 'move-line-down)
(define-key prog-mode-map (kbd "<C-M-up>")   'move-line-up)

(global-unset-key (kbd "<S-down-mouse-1>"))
(global-set-key (kbd "<S-mouse-1>") 'mouse-save-then-kill)
