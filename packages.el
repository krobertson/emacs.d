;;;; package.el

(require 'use-package)

;;; packages

(use-package ace-jump-mode
  :bind ("C-c SPC" . ace-jump-mode)
  :init
  (progn
    (require 'cl)))

(use-package lua-mode
  :defer t)

(use-package ag
  :commands (ag ag-files ag-regexp ag-project ag-project-files ag-project-regexp)
  :config
  (progn
    (setq ag-highlight-search t
          ag-reuse-buffers t)))

(use-package auto-complete
  :diminish auto-complete-mode
  :config
  (progn
    (use-package go-autocomplete)
    (add-to-list 'ac-dictionary-directories (emacs-d "elpa/auto-complete-20131128.233/dict"))
    (setq ac-use-fuzzy t
          ac-disable-inline t
          ac-use-menu-map t
          ac-auto-show-menu t
          ac-auto-start t
          ac-ignore-case t
          ac-candidate-menu-min 0)
    (add-to-list 'ac-modes 'enh-ruby-mode)
    (add-to-list 'ac-modes 'web-mode)
    (add-to-list 'ac-modes 'go-mode)
    (add-to-list 'ac-modes 'clojure-mode)))

(use-package clojure-mode
  :defer t)

(use-package dockerfile-mode
  :defer t)

(use-package diminish
  :config
  (progn
    (add-hook 'whitespace-mode-hook (lambda () (diminish 'whitespace-mode)))))

(use-package exec-path-from-shell
  :config
  (progn
    (dolist (var '("GOPATH" "SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
      (add-to-list 'exec-path-from-shell-variables var))
    (setq exec-path-from-shell-check-startup-files nil)
    (exec-path-from-shell-initialize)))

(use-package fiplr
  :bind ("s-t" . fiplr-find-file)
  :config
  (progn
    (setq fiplr-root-markers '(".git" ".svn" ".hg"))
    (setq fiplr-ignored-globs '((directories (".git" ".svn" ".hg"))
                               (files ("*.jpg" "*.png" "*.zip" "*~"))))))

(use-package flx
  :defer t
  :config
  (progn
    (flx-ido-mode 1)
    (setq ido-use-faces nil)
    (setq gc-cons-threshold 20000000)))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package github-browse-file
  :commands github-browse-file)

(use-package gist
  :bind ("C-c C-g" . gist-region-or-buffer-private))

(use-package flycheck
  :defer t
  :config
  (progn
    (set-face-underline 'flycheck-error nil)
    (set-face-background 'flycheck-error nil)
    (set-face-underline 'flycheck-warning nil)
    (set-face-background 'flycheck-warning "#D0BF8F")))

(use-package go-mode
  :defer t)

(use-package haml-mode
  :defer t)

(use-package ido-ubiquitous
  :init
  ;; Fix ido-ubiquitous for newer packages
  (defmacro ido-ubiquitous-use-new-completing-read (cmd package)
    `(eval-after-load ,package
       '(defadvice ,cmd (around ido-ubiquitous-new activate)
          (let ((ido-ubiquitous-enable-compatibility nil))
            ad-do-it)))))

(use-package json-mode
  :defer t)

(use-package js2-mode
  :defer t)

(use-package magit
  :init
  (progn
    (use-package magit-blame)
    (bind-key "C-c C-a" 'magit-just-amend magit-mode-map))
  :config
  (progn
    (setq magit-default-tracking-name-function 'magit-default-tracking-name-branch-only)
    (setq magit-set-upstream-on-push t)
    (setq magit-completing-read-function 'magit-ido-completing-read)
    (setq magit-stage-all-confirm nil)
    (setq magit-unstage-all-confirm nil)
    (setq magit-restore-window-configuration t)
    (setq magit-last-seen-setup-instructions "1.4.0"))
  :bind ("C-x g" . magit-status))

(use-package markdown-mode
  :mode ("\\.\\(m\\(ark\\)?down\\|md\\)$" . markdown-mode)
  :config)

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-all-like-this)
         ("C-c C-<" . mc/mark-previous-like-this)))

(use-package ox-reveal
  :defer t
  :init
  (progn
    (setq org-reveal-root (concat "file://" (emacs-d "vendor/reveal.js"))))
 :commands (org-reveal-export-to-html org-reveal-export-to-html-and-browse))

(use-package ox-s5
  :defer t
  :init
  (progn
    (setq org-s5-ui-url (concat "file://" (emacs-d "vendor/s5/ui")))
    (setq org-s5-theme "railscast"))
  :commands (org-s5-export-to-html org-s5-export-as-html))

(use-package pandoc-mode
  :defer t)

(use-package powerline)

(use-package projectile
  ;;:init (projectile-global-mode 1)
  :bind (("s-p" . projectile-find-file)
         ("s-b" . projectile-switch-to-buffer)
         ("s-F" . projectile-ag))
  :config
  (progn
    (setq projectile-enable-caching t)
    (setq projectile-require-project-root nil)
    (setq projectile-completion-system 'grizzl)
    (setq projectile-globally-ignored-files
      (append projectile-globally-ignored-files
      '(
        ;; continuum import files
        "*.cntmp" )))
    (add-to-list 'projectile-globally-ignored-files ".DS_Store")))

(use-package enh-ruby-mode
  :defer t)

(use-package ruby-end
  :diminish ruby-end-mode)

(use-package ruby-electric
  :defer t
  :diminish ruby-electric-mode)

(use-package smex
  :defer t
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands))
  :config
  (progn
    (smex-initialize)))

(use-package undo-tree
  :bind (("s-z" . undo-tree-undo)
         ("s-Z" . undo-tree-redo))
  :init
  (progn
    (defalias 'redo 'undo-tree-redo)
    (defalias 'undo 'undo-tree-undo)
    ))

(use-package yaml-mode
  :defer t)
