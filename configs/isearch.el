;; when you find a match and exit isearch, move to the beginning of the match
;; rather than at the last character that was matching
(add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)
(defun my-goto-match-beginning ()
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))
(defadvice isearch-exit (after my-goto-match-beginning activate)
  "Go to beginning of match."
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))

;; enable pasting text into the isearch minibuffer
(define-key isearch-mode-map (kbd "C-v") 'isearch-yank-kill)
