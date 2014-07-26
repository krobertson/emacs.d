;;;; init.el

; ;; == package mgmt ==========================================================

(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; window size
(setq default-frame-alist '((font . "Source Code Pro for Powerline-10")
                            (width . 190)
                            (height . 60)))

;; UI
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;; 4px left, and no right right fringe
(if (fboundp 'set-fringe-style) (set-fringe-style '(4 . 0)))
(if window-system (x-focus-frame nil))
(setq default-line-spacing 0)

;; set the theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'zenburn t nil)

;; cursor
(setq-default cursor-type 'bar)
(setq-default cursor-in-non-selected-windows 'hollow)
(blink-cursor-mode t)
(setq-default blink-matching-paren t)

;; mouse
(setq mouse-wheel-scroll-amount '(0.001))

;; cua mode
(cua-mode t)
(setq cua-auto-tabify-rectangles nil
      cua-keep-region-after-copy t)
(transient-mark-mode 1)

(defun emacs-d (filename)
  "Expand FILENAME relative to `user-emacs-directory'."
  (expand-file-name filename user-emacs-directory))

;; *scratch* buffer
(setq initial-scratch-message nil)
(setq initial-major-mode 'text-mode)
;; Never kill, just bury
(defun dont-kill-but-bury-scratch ()
  (if (equal (buffer-name (current-buffer)) "*scratch*")
      (progn (bury-buffer) nil)
    t))
(add-hook 'kill-buffer-query-functions 'dont-kill-but-bury-scratch)

;; annoyances
(setq inhibit-splash-screen t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq ring-bell-function 'ignore)

;; no backup files, no auto-saving
(setq make-backup-files nil)
(setq auto-save-default nil
      auto-save-list-file-prefix nil
      create-lockfiles nil)

;; Do not ask for confirmation
(setq confirm-nonexistent-file-or-buffer nil)

;; Do not show annoying menu-bar tips
(setq suggest-key-bindings nil)

;; ido
(ido-mode 1)
(ido-everywhere 1)
(add-to-list 'ido-ignore-files "\\.DS_Store")
(setq ido-use-virtual-buffers t
      recentf-save-file (emacs-d "var/recentf")
      ido-save-directory-list-file (emacs-d "var/ido-last.el"))

;; Display completions vertically
; (setq ido-decorations (quote ("\n> " "" "\n  " "\n  ..." "[" "]"
;                          " [No Match]" " [Matched]" " [Not Readable]"
;                          " [Too Big]" " [Confirm]")))

(defun ido-disable-line-truncation ()
  (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)

(defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
(add-hook 'ido-setup-hook 'ido-define-keys)

;; os x
(when (string= system-type "darwin")
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super
        mac-allow-anti-aliasing t
        delete-by-moving-to-trash t
        browse-url-browser-function 'browse-url-default-macosx-browser
        trash-directory (expand-file-name ".Trash" (getenv "HOME"))))

;; linux specific
(when (string= system-type "linux")
  (setq browse-url-browser-function 'browse-url-chromium
        browse-url-chromium-program "google-chrome"))

;; whitespace
(setq whitespace-line-column 120)
(setq whitespace-style '(face lines-tail))
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; autofill
(setq-default fill-column 80)

;; highlight matching parenthesis
(setq show-paren-style 'parenthesis)
(show-paren-mode +1)

;; highlight the current line
(global-hl-line-mode +1)

;; Tabs/indentation
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; aquamacs specific
(when (boundp 'aquamacs-version)
  (setq user-emacs-directory "~/.emacs.d")
  (one-buffer-one-frame-mode 0))

;;show a marker in the left fringe for lines not in the buffer
(setq default-indicate-empty-lines t)

;; Set the frame's title. %b is the name of the buffer. %+ indicates the
;; state of the buffer: * if modified, % if read only, or - otherwise.
;; Two of them to emulate the mode line. %f for the file name
;; (absolute path actually).
(setq frame-title-format "Emacs: %b %+%+ %f")

;; recent file list
(recentf-mode 1)
(setq recentf-max-menu-items 25)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

;; configure hooks before we load packages and modes
(load (emacs-d "autohooks"))
(load-autohooks)

;; external packages
(load (emacs-d "packages"))

;; -- load everything from dotfiles-init-dir ---------------------------------
(setq init-file (or load-file-name buffer-file-name))
(setq dotfiles-dir (file-name-directory init-file))
(setq dotfiles-init-dir (expand-file-name "configs" dotfiles-dir))
(if (file-exists-p dotfiles-init-dir)
  (dolist (file (directory-files dotfiles-init-dir t "\\.el$"))
    (load file)))

;; other vendored plugins
(add-to-list 'load-path (emacs-d "vendor"))
(load (emacs-d "vendor/jekyll"))
(load (emacs-d "vendor/linum+"))
;; window numbering
(load (emacs-d "vendor/window_numbering"))
(window-numbering-mode 1)
;; folding mode
(load (emacs-d "vendor/folding"))
(folding-mode-add-find-file-hook)

;; custom functions
(load (emacs-d "functions"))
;; keybindings
(load (emacs-d "keybindings"))

;; configure jekyll blog
(setq jekyll-post-ext ".md"
      jekyll-directory (expand-file-name "~/blog/"))

;; emacs server
(require 'server)
(unless (server-running-p)
  (server-start))
