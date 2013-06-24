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

(use-package ace-jump-mode
  :ensure t
  :bind ("C-c SPC" . ace-jump-mode)
  :init
  (progn
    (require 'cl)))

(use-package auto-complete
  :ensure t
  :commands auto-complete-mode
  :diminish auto-complete-mode
  :defer t)

(use-package diminish
  :ensure t
  :config
  (progn
    (add-hook 'whitespace-mode-hook (lambda () (diminish 'whitespace-mode)))))

(when is-mac
  (use-package exec-path-from-shell
    :init
    (progn
      (dolist (var '("GOPATH" "SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
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

(use-package edit-server
  :ensure t
  :if window-system
  :init
  (add-hook 'after-init-hook 'edit-server-start)
  :config
  (progn
    (bind-key "C-c C-k" 'edit-server-abort edit-server-edit-mode-map)
    (add-hook 'edit-server-start-hook
              '(lambda ()
                 (auto-fill-mode)
                 (flyspell-mode)
                 (set-fill-column 80)))))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package github-browse-file
  :ensure t
  :commands github-browse-file
            github-browse-file-blame)

(use-package gist
  :ensure t
  :bind ("C-c C-g" . gist-region-or-buffer-private))

(use-package flycheck
  :ensure t
  :defer t)

(use-package go-mode
  :ensure t
  :defer t)

(use-package haml-mode
  :ensure t
  :defer t)

(use-package ido-ubiquitous
  :ensure t
  :init
  ;; Fix ido-ubiquitous for newer packages
  (defmacro ido-ubiquitous-use-new-completing-read (cmd package)
    `(eval-after-load ,package
       '(defadvice ,cmd (around ido-ubiquitous-new activate)
          (let ((ido-ubiquitous-enable-compatibility nil))
            ad-do-it)))))

(use-package markdown-mode
  :ensure t
  :mode ("\\.\\(m\\(ark\\)?down\\|md\\)$" . markdown-mode)
  :config)

(use-package powerline
  :ensure t
  :config
  (progn
    (custom-set-variables
      '(powerline-default-separator 'arrow))
    (setq powerline-arrow-shape 'arrow14)

    ;; setup colors
    (set-face-background 'mode-line "grey16")
    (set-face-background 'mode-line-inactive "grey16")
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
                                       (powerline-minor-modes face1 'l)
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

(use-package ruby-electric
  :ensure t
  :defer t
  :diminish ruby-electric-mode)

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
