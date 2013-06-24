;; automatically associate hook files with hooks
;; taken from https://github.com/zenspider/elisp/blob/master/misc/autohook.el
;;;###autoload
(defun load-autohooks ()
  (interactive)
  (dolist (path (directory-files (concat user-emacs-directory "hooks") t ".*el$"))
    (let* ((mode       (file-name-nondirectory (file-name-sans-extension path)))
           (hook-name  (intern (concat mode "-hook")))
           (defun-name (intern (concat "auto-" mode "-hook")))
           (lisp       (read-file-to-string path)))
      (eval (read (concat "(defun " (symbol-name defun-name) " () " lisp ")")))
      (and (functionp defun-name)
           (remove-hook hook-name defun-name))
      (add-hook hook-name defun-name))))

;;;###autoload
(defun read-file-to-string (path)
  (with-temp-buffer
    (insert-file-contents-literally path)
    (buffer-string)))
