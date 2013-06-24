;;;; init.el

;; window size
(setq default-frame-alist '(
                (width . 190)
                (height . 60) ))

;; UI
(if window-system
    (progn
      (tool-bar-mode -1)
      (scroll-bar-mode -1)
      ;; 4px left, and no right right fringe
      (set-fringe-style '(4 . 0))))

;; no menu bar
(menu-bar-mode -1)

;; set the theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'zenburn t nil)

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
      auto-save-list-file-prefix nil)

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

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))
(when is-mac
  (setq mouse-wheel-scroll-amount '(0.001)))

;; os x
(when (string= system-type "darwin")
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super
        mac-allow-anti-aliasing t
        delete-by-moving-to-trash t
        trash-directory (expand-file-name ".Trash" (getenv "HOME"))))

;; whitespace
(setq whitespace-line-column 80)
(setq whitespace-style '(face lines-tail))
(add-hook 'before-save-hook 'delete-trailing-whitespace)

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

;; disable local variables
(setq enable-local-variables nil)

;; external packages
(load (emacs-d "packages"))

;; other vendored plugins
(load (emacs-d "vendor/jekyll"))
(load (emacs-d "vendor/linum+"))

;; custom functions
(load (emacs-d "functions"))

;; keybindings
(load (emacs-d "keybindings"))

;; other mode configs
(load (emacs-d "configs/ruby-mode"))

;; enable linum in prog-mode
(add-hook 'prog-mode-hook
  (lambda ()
    (linum-mode)))

;; configure jekyll blog
(setq jekyll-post-ext ".md"
      jekyll-directory (expand-file-name "~/blog/"))

;; emacs server
(require 'server)
(unless (server-running-p)
  (server-start))
