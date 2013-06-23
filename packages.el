;;;; package.el
(require 'package)
(setq package-user-dir "~/.emacs.d/elpa/")
(setq package-archives
      (append package-archives
              '(("melpa" . "http://melpa.milkbox.net/packages/"))
              '(("org" . "http://orgmode.org/elpa/"))))

;; Load the list of packages but don't initialize them.
;; `use-package' will arrange the necessary autoload entries.
(package-initialize nil)

;; If never connected to repositories before, download package descriptions so
;; `use-package' can trigger installation of missing packages.
(unless package-archive-contents
    (message "Refreshing ELPA package archives...")
    (package-refresh-contents))

;; ...but before everything, make sure `use-package' is installed.
(unless (package-installed-p 'use-package)
  (message "`use-package' not found.  Installing...")
  (package-install 'use-package))

(require 'use-package)
(setq use-package-minimum-reported-time 0)


;;; packages

(when is-mac
  (use-package exec-path-from-shell
    :init
    (progn
      (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
        (add-to-list 'exec-path-from-shell-variables var)))
    :config
    (progn
      (exec-path-from-shell-initialize))))

(use-package fiplr
  :ensure t
  :bind ("s-t" . fiplr-find-file)
  :config
  (progn
    (setq fiplr-root-markers '(".git" ".svn" ".hg"))
    (setq fiplr-ignored-globs '((directories (".git" ".svn" ".hg"))
                               (files ("*.jpg" "*.png" "*.zip" "*~"))))))

(use-package flx
  :ensure t
  :defer t
  :init
  (progn
    (flx-ido-mode 1))
  :config
  (progn
    (setq ido-use-faces nil)
    (setq gc-cons-threshold 20000000)))

(use-package gist
  :ensure t
  :bind ("C-c C-g" . gist-region-or-buffer-private))

(use-package go-mode
  :ensure t
  :defer t
  :config
  (progn
    (add-hook 'go-mode-hook
      (lambda ()
        (set (make-local-variable 'indent-tabs-mode) 't)
        (set (make-local-variable 'tab-width) 4)
        (whitespace-mode)
        (add-hook 'before-save-hook 'gofmt-before-save)))))

(use-package markdown-mode
  :ensure t
  :mode ("\\.\\(m\\(ark\\)?down\\|md\\)$" . markdown-mode)
  :config
  (progn
    (add-hook 'markdown-mode-hook
      (lambda ()
        (whitespace-mode)))))

(use-package powerline
  :ensure t
  :config
  (progn
    (custom-set-variables
      '(powerline-default-separator 'arrow))
    (setq powerline-arrow-shape 'arrow14)

    ;; setup colors
    (set-face-background 'mode-line "grey16")
    (set-face-foreground 'powerline-active1 "#8FB28F")
    (set-face-foreground 'powerline-active2 "#AFD8AF")

    ;; modeline items
    (display-time-mode 1)
    (display-battery-mode 1)

    ;; mode-line format
    (setq-default mode-line-format
                  '("%e"
                    (:eval
                     (let* ((active (powerline-selected-window-active))
                            (mode-line (if active 'mode-line 'mode-line-inactive))
                            (face1 (if active 'powerline-active1 'powerline-inactive1))
                            (face2 (if active 'powerline-active2 'powerline-inactive2))
                            (separator-left (intern (format "powerline-%s-%s"
                                                            powerline-default-separator
                                                            (car powerline-default-separator-dir))))
                            (separator-right (intern (format "powerline-%s-%s"
                                                             powerline-default-separator
                                                             (cdr powerline-default-separator-dir))))
                            (lhs (list
                                       (powerline-raw mode-line-modified nil 'l)
                                       (powerline-buffer-id nil 'l)
                                       (when (and (boundp 'which-func-mode) which-func-mode)
                                         (powerline-raw which-func-format nil 'l))
                                       (powerline-raw " ")
                                       (funcall separator-left mode-line face1)
                                       (when (boundp 'erc-modified-channels-object)
                                         (powerline-raw erc-modified-channels-object face1 'l))
                                       (powerline-major-mode face1 'l)
                                       (powerline-process face1)
                                       (powerline-narrow face1 'l)
                                       (powerline-raw " " face1)
                                       (funcall separator-left face1 face2)
                                       (powerline-vc face2 'r)))
                            (rhs (list (powerline-raw global-mode-string face2 'r)
                                       (funcall separator-right face2 face1)
                                       (powerline-raw "%4l" face1 'l)
                                       (powerline-raw ":" face1 'l)
                                       (powerline-raw "%3c" face1 'r)
                                       (funcall separator-right face1 mode-line)
                                       (powerline-raw " ")
                                       (powerline-raw "%6p" nil 'r)
                                       (powerline-hud face2 face1))))
                       (concat (powerline-render lhs)
                               (powerline-fill face2 (powerline-width rhs))
                               (powerline-render rhs))))))))

(use-package smex
  :ensure t
  :defer t
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands))
  :config
  (progn
    (smex-initialize)))

(use-package yaml-mode
  :ensure t
  :defer t)
