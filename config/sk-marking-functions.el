;; Deactivate mark
(defun sk/remove-mark ()
  "Deactivate the region"
  (interactive)
  (if (region-active-p)
	  (deactivate-mark)))
(bind-key* "M-h" 'sk/remove-mark)

;; select line
(defun sk/select-inside-line ()
  "Select the current line"
  (interactive)
  (sk/smarter-move-beginning-of-line 1)
  (set-mark (line-end-position))
  (exchange-point-and-mark))

(defun sk/select-around-line ()
  "Select line including the newline character"
  (interactive)
  (sk/select-inside-line)
  (next-line 1)
  (sk/smarter-move-beginning-of-line 1))

(defun sk/mark-around-markdown-code-block ()
  "Marks a markdown code-block."
  (interactive)
  (let ((case-fold-search t)
        (re "```.*"))
    (unless (looking-at re)
      (search-backward-regexp re))
    (set-mark (point))
	(move-end-of-line 1)
    (search-forward (concat "```" (match-string 1)))
    (exchange-point-and-mark)))

(defun sk/mark-inside-markdown-code-block ()
  "Marks inside a markdown code-block."
  (interactive)
  (let ((case-fold-search t)
        (re "```.*"))
    (unless (looking-at re)
      (search-backward-regexp re))
	(next-line)
	(move-beginning-of-line 1)
    (set-mark (point))
	(search-forward (concat "```" (match-string 1)))
	(previous-line)
	(move-end-of-line 1)
    (exchange-point-and-mark)))

;; provide this configuration
(provide 'sk-marking-functions)
