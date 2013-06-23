(defun ido-recentf ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (let ((home (expand-file-name (getenv "HOME"))))
  (find-file
    (ido-completing-read "Recentf open: "
      (mapcar (lambda (path)
        (replace-regexp-in-string home "~" path))
        recentf-list)
      nil t))))

(defun kr-ido-find-project-file ()
  "Load a file under one of the source projects."
  (interactive)
  (ido-find-file-in-dir
    (concat "~/source/" (ido-completing-read "Project: "
                          (directory-files "~/source/" nil "^[^.]")))))

(defun kr-mark-line ()
  "Sets the mark as the current line."
  (interactive)
  (beginning-of-line)
  (set-mark-command nil)
  (next-line))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))
