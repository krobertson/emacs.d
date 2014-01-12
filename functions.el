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

(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)
        (next-line)))

(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (move-to-column col)))

(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
    (delq (current-buffer)
      (remove-if-not 'buffer-file-name (buffer-list)))))

(defun kr-delete-line ()
  "Deletes the current line, doesn't affect kill-ring or clipboard."
  (interactive)
  (beginning-of-line)
  (set-mark-command nil)
  (next-line)
  (delete-region (region-beginning) (region-end)))

(defun smart-line-beginning ()
  "Move point to the beginning of text on the current line; if that is already
the current position of point, then move it to the beginning of the line."
  (interactive)
  (let ((pt (point)))
    (beginning-of-line-text)
    (when (eq pt (point))
      (beginning-of-line))))

(defun smart-move-line-beginning ()
  "Move point to the beginning of text on the current line; if that is already
the current position of point, then move it to the beginning of the line."
  (interactive)
  (set-mark-command nil)
  (smart-line-beginning))

(defun ad-advised-definition-p (definition)
  "Return non-nil if DEFINITION was generated from advice information."
  (if (or (ad-lambda-p definition)
    (macrop definition)
    (ad-compiled-p definition))
      (let ((docstring (ad-docstring definition)))
  (and (stringp docstring)
      (get-text-property 0 'dynamic-docstring-function docstring)))))

