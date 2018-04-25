;; clipboard copy function
(defun sk/copy-to-clipboard ()
  "Copies the selection to the system clipboard"
  (interactive)
  (cond
   ((eq system-type 'darwin)
	(if (region-active-p)
		(progn
		  (shell-command-on-region (region-beginning) (region-end) "pbcopy")
		  (message "Yanked region to clipboard!")
		  (deactivate-mark))
	  (message "No region active; can't yank to clipboard!")))
   ((eq system-type 'gnu/linux)
	(if (region-active-p)
		(progn
		  (shell-command-on-region (region-beginning) (region-end) "xsel -i -b")
		  (message "Yanked region to clipboard!")
		  (deactivate-mark))
	  (message "No region active; can't yank to clipboard!")))))

;; clipboard cut function
(defun sk/cut-to-clipboard ()
  "Cuts the selection to the system clipboard"
  (interactive)
  (cond
   ((eq system-type 'darwin)
	(if (region-active-p)
		(progn
		  (shell-command-on-region (region-beginning) (region-end) "pbcopy")
		  (message "Yanked region to clipboard!")
		  (delete-region (region-beginning) (region-end))
		  (deactivate-mark))
	  (message "No region active; can't yank to clipboard!")))
   ((eq system-type 'gnu/linux)
	(if (region-active-p)
		(progn
		  (shell-command-on-region (region-beginning) (region-end) "xsel -i -b")
		  (message "Yanked region to clipboard!")
		  (delete-region (region-beginning) (region-end))
		  (deactivate-mark))
	  (message "No region active; can't yank to clipboard!")))))

;; clipboard paste function
(defun sk/paste-from-clipboard ()
  "pastes the system clipboard contents"
  (interactive)
  (cond ((eq system-type 'darwin)
		 (insert (shell-command-to-string "pbpaste")))
		((eq system-type 'gnu/linux)
		 (insert (shell-command-to-string "xsel -o -b")))))

;; Deactivate mark
(defun sk/remove-mark ()
  "Deactivate the region"
  (interactive)
  (if (region-active-p)
	  (deactivate-mark)))

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

(defun sk/mark-next-line (count)
  "marks to the next `count' lines"
  (interactive "p")
  (beginning-of-line)
  (set-mark (point))
  (next-line count)
  (end-of-line)
  (forward-char))

(defun sk/mark-previous-line (count)
  "marks to the previous `count' lines"
  (interactive "p")
  (end-of-line)
  (set-mark (point))
  (previous-line (+ count 1))
  (next-line)
  (beginning-of-line))

(defun sk/mark-next-char (count)
  "marks to the next `count' chars"
  (interactive "p")
  (set-mark (point))
  (forward-char count))

(defun sk/mark-previous-char (count)
  "marks to the previous `count' chars"
  (interactive "p")
  (set-mark (point))
  (backward-char count))

(defun sk/mark-forward-word (count)
  "marks to the end of word from current position"
  (interactive "p")
  (mark-word)
  (exchange-point-and-mark))

(defun sk/mark-backward-word (count)
  "marks to the beginning of word from current position"
  (interactive "p")
  (set-mark (point))
  (backward-word count))

(defun sk/mark-forward-para (count)
  "marks to the end of word from current position"
  (interactive "p")
  (set-mark (point))
  (forward-paragraph count))

(defun sk/mark-backward-para (count)
  "marks to the beginning of para from current position"
  (interactive "p")
  (set-mark (point))
  (backward-paragraph count))

(defun sk/mark-beginning-buffer ()
  "marks to the beginning of buffer from current position"
  (interactive)
  (set-mark (point))
  (beginning-of-buffer))

(defun sk/mark-end-buffer ()
  "marks to the end of buffer from current position"
  (interactive)
  (set-mark (point))
  (end-of-buffer))

(defun sk/mark-beginning-line ()
  "marks to the beginning of line from current position"
  (interactive)
  (set-mark (point))
  (sk/smarter-move-beginning-of-line 1))

(defun sk/mark-end-line ()
  "marks to the end of line from current position"
  (interactive)
  (set-mark (point))
  (end-of-line))

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

;; alignment functions
(defun sk/align-whitespace ()
  "Align columns by whitespace"
  (interactive)
  (if (region-active-p)
	  (align-regexp (region-beginning) (region-end)
					"\\(\\s-*\\)\\s-" 1 0 t)
	(mark-paragraph)
	(align-regexp (region-beginning) (region-end)
				  "\\(\\s-*\\)\\s-" 1 0 t)))
(defun sk/align-semicolon ()
  "Align columns by semicolon"
  (interactive)
  (if (region-active-p)
	  (align-regexp (region-beginning) (region-end)
					"\\(\\s-*\\)\\s-;" 1 0 t)
	(mark-paragraph)
	(align-regexp (region-beginning) (region-end)
				  "\\(\\s-*\\)\\s-;" 1 0 t)))
(defun sk/align-ampersand ()
  "Align columns by ampersand"
  (interactive)
  (if (region-active-p)
	  (align-regexp (region-beginning) (region-end)
					"\\(\\s-*\\)&" 1 1 t)
	(mark-paragraph)
	(align-regexp (region-beginning) (region-end)
				  "\\(\\s-*\\)&" 1 1 t)))
(defun sk/align-quote-space ()
  "Align columns by quote and space"
  (interactive)
  (if (region-active-p)
	  (align-regexp (region-beginning) (region-end)
					"\\(\\s-*\\).*\\s-\"" 1 0 t)
	(mark-paragraph)
	(align-regexp (region-beginning) (region-end)
				  "\\(\\s-*\\).*\\s-\"" 1 0 t)))
(defun sk/align-equals ()
  "Align columns by equals sign"
  (interactive)
  (if (region-active-p)
	  (align-regexp (region-beginning) (region-end)
					"\\(\\s-*\\)\\s-=\\s-" 1 0 t)
	(mark-paragraph)
	(align-regexp (region-beginning) (region-end)
				  "\\(\\s-*\\)\\s-=\\s-" 1 0 t)))
(defun sk/align-comma ()
  "Align columns by comma"
  (interactive)
  (if (region-active-p)
	  (align-regexp (region-beginning) (region-end)
					"\\(\\s-*\\)\\s-," 1 1 t)
	(mark-paragraph)
	(align-regexp (region-beginning) (region-end)
				  "\\(\\s-*\\)\\s-," 1 1 t)))
(defun sk/align-dot ()
  "Align columns by dot"
  (interactive)
  (if (region-active-p)
	  (align-regexp (region-beginning) (region-end)
					"\\(\\s-*\\)\\\." 1 1 t)
	(mark-paragraph)
	(align-regexp (region-beginning) (region-end)
				  "\\(\\s-*\\)\\\." 1 1 t)))
(defun sk/align-colon ()
  "Align columns by colon"
  (interactive)
  (if (region-active-p)
	  (align-regexp (region-beginning) (region-end)
					"\\(\\s-*\\):" 1 0 t)
	(mark-paragraph)
	(align-regexp (region-beginning) (region-end)
				  "\\(\\s-*\\):" 1 0 t)))
(defun sk/align-percent ()
  "Align columns by percent"
  (interactive)
  (if (region-active-p)
	  (align-regexp (region-beginning) (region-end)
					"\\(\\s-*\\)\\s-%" 1 0 t)
	(mark-paragraph)
	(align-regexp (region-beginning) (region-end)
				  "\\(\\s-*\\)\\s-%" 1 0 t)))
(defun sk/align-hash ()
  "Align columns by percent"
  (interactive)
  (if (region-active-p)
	  (align-regexp (region-beginning) (region-end)
					"\\(\\s-*\\)\\s-#" 1 0 t)
	(mark-paragraph)
	(align-regexp (region-beginning) (region-end)
				  "\\(\\s-*\\)\\s-#" 1 0 t)))

;; Transpose words forward
(defun sk/transpose-words-forward ()
  "Transpose words forward"
  (interactive)
  (forward-word 1)
  (forward-char 1)
  (transpose-words 1)
  (backward-word 1))
;; Transpose words backward
(defun sk/transpose-words-backward ()
  "Transpose words backward"
  (interactive)
  (transpose-words 1)
  (backward-word 2))

;; move lines
(defun sk/move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg)
          (when (and (eval-when-compile
                       '(and (>= emacs-major-version 24)
                             (>= emacs-minor-version 3)))
                     (< arg 0))
            (forward-line -1)))
        (forward-line -1))
      (move-to-column column t)))))
(defun sk/move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (sk/move-text-internal arg))
(defun sk/move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (sk/move-text-internal (- arg))
  (next-line 1))

;; Autocorrect
(defun sk/simple-get-word ()
  (car-safe (save-excursion (ispell-get-word nil))))
(defun sk/ispell-word-then-abbrev (p)
  "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev. Otherwise it will
be global.
If there's nothing wrong with the word at point, keep
looking for a typo until the beginning of buffer. You can
skip typos you don't want to fix with `SPC', and you can
abort completely with `C-g'."
  (interactive "P")
  (let (bef aft)
    (save-excursion
      (while (if (setq bef (sk/simple-get-word))
                 ;; Word was corrected or used quit.
                 (if (ispell-word nil 'quiet)
                     nil ; End the loop.
                   ;; Also end if we reach `bob'.
                   (not (bobp)))
               ;; If there's no word at point, keep looking
               ;; until `bob'.
               (not (bobp)))
        (backward-word)
        (backward-char))
      (setq aft (sk/simple-get-word)))
    (if (and aft bef (not (equal aft bef)))
        (let ((aft (downcase aft))
              (bef (downcase bef)))
          (define-abbrev
            (if p local-abbrev-table global-abbrev-table)
            bef aft)
          (message "\"%s\" now expands to \"%s\" %sally"
                   bef aft (if p "loc" "glob")))
      (user-error "No typo at or before point"))))

;; vi like above and below
(defun sk/open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))
(defun sk/open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

;; put blank lines up and down
(defun sk/blank-line-below ()
  (interactive)
  (save-excursion
	(sk/open-line-below)
	(previous-line)))
(defun sk/blank-line-above ()
  (interactive)
  (save-excursion
	(sk/open-line-above)
	(next-line)))

;; put blank lines forward and backward
(defun sk/blank-char-forward ()
  (interactive)
  (save-excursion
	(insert-char 00A0)
	(backward-char 1)))
(defun sk/blank-char-backward ()
  (interactive)
  (save-excursion
	(forward-char 1)
	(insert-char 00A0)))

;; backward kill word or region
(defun sk/kill-region-or-backward-word ()
  (interactive)
  (if (region-active-p)
	  (kill-region (region-beginning) (region-end))
	(backward-kill-word 1)))

;; kill line or region
(defun sk/kill-region-or-line (arg)
  (interactive "p")
  (if (region-active-p)
	  (kill-region (region-beginning) (region-end))
	(kill-whole-line arg)))

;; save region or current line
(defun sk/copy-whole-lines (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
				  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

(defun sk/copy-region-or-line (arg)
  (interactive "p")
  (if (region-active-p)
	  (kill-ring-save (region-beginning) (region-end))
	(sk/copy-whole-lines arg)))

;; copy to the end of line
(defun sk/copy-to-end-of-line ()
  (interactive)
  (kill-ring-save (point)
                  (line-end-position))
  (message "Copied to end of line"))

;; thanks to https://stackoverflow.com/a/4717026
(defun sk/duplicate-current-line-or-region (&optional n)
  "Duplicate current line, or region if active.
With argument N, make N copies.
With negative N, comment out original line and use the absolute value."
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region        ;Get region if active, otherwise line
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      (if (< 0 (forward-line 1)) ;Go to beginning of next line, or make a new one
                          (newline))))))
        (dotimes (i (abs (or n 1)))     ;Insert N times, or once if not specified
          (insert text))))
    (if use-region nil                  ;Only if we're working with a line (not a region)
      (let ((pos (- (point) (line-beginning-position)))) ;Save column
        (if (> 0 n)                             ;Comment out original with negative arg
            (comment-region (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (forward-char pos)))))

;; toggle case
(defun sk/toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Toggles from 3 cases: UPPER CASE, lower case, Title Case,
in that cyclic order."
  (interactive)
  (let (pos1 pos2 (deactivate-mark nil) (case-fold-search nil))
	(if (and transient-mark-mode mark-active)
		(setq pos1 (region-beginning)
			  pos2 (region-end))
	  (setq pos1 (car (bounds-of-thing-at-point 'word))
			pos2 (cdr (bounds-of-thing-at-point 'word))))

	(when (not (eq last-command this-command))
	  (save-excursion
		(goto-char pos1)
		(cond
		 ((looking-at "[[:lower:]][[:lower:]]") (put this-command 'state
													 "all lower"))
		 ((looking-at "[[:upper:]][[:upper:]]") (put this-command 'state
													 "all caps") )
		 ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'state
													 "init caps") )
		 (t (put this-command 'state "all lower") ))))

	(cond
	 ((string= "all lower" (get this-command 'state))
	  (upcase-initials-region pos1 pos2) (put this-command 'state "init caps"))
	 ((string= "init caps" (get this-command 'state))
	  (upcase-region pos1 pos2) (put this-command 'state "all caps"))
	 ((string= "all caps" (get this-command 'state))
	  (downcase-region pos1 pos2) (put this-command 'state "all lower")))))

;; increase or decrease number at point
(require 's)
(defun sk/incs (s &optional num)
  (let* ((inc (or num 1))
		 (new-number (number-to-string (+ inc (string-to-number s))))
		 (zero-padded? (s-starts-with? "0" s)))
	(if zero-padded?
		(s-pad-left (length s) "0" new-number)
	  new-number)))

(defun sk/change-number-at-point (arg)
  (interactive "p")
  (unless (or (looking-at "[0-9]")
			  (looking-back "[0-9]"))
	(sk/goto-closest-number))
  (save-excursion
	(while (looking-back "[0-9]")
	  (forward-char -1))
	(re-search-forward "[0-9]+" nil)
	(replace-match (sk/incs (match-string 0) arg) nil nil)))

(defun sk/subtract-number-at-point (arg)
  (interactive "p")
  (sk/change-number-at-point (- arg)))

;; unfill region
(defun unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
	logical line.  This is useful, e.g., for use with
	`visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
	(fill-region beg end)))

;; cut region or line to clipboard
(defun sk/cut-region-or-line-to-clipboard ()
  "Cut current line or region to the clipboard"
  (interactive)
  (if (region-active-p)
	  (sk/cut-to-clipboard)
	(sk/select-inside-line)
	(sk/cut-to-clipboard)))

;; copy region or line to clipboard
(defun sk/copy-region-or-line-to-clipboard ()
  "Copy current line or region to the clipboard"
  (interactive)
  (if (region-active-p)
	  (sk/copy-to-clipboard)
	(sk/select-inside-line)
	(sk/copy-to-clipboard)))

;; replace region
(defun sk/replace-region ()
  "replace the region with contents of yank ring"
  (interactive)
  (kill-region (region-beginning) (region-end))
  (yank 2))

;; replace region or line
(defun sk/replace-region-or-line ()
  "replace the region or line with contents of yank ring"
  (interactive)
  (if (region-active-p)
	  (sk/replace-region)
	(sk/select-inside-line)
	(sk/replace-region)))

;; replace region with clipboard
(defun sk/replace-region-with-clipboard ()
  "replace the region with contents of the clipboard"
  (interactive)
  (kill-region (region-beginning) (region-end))
  (sk/paste-from-clipboard))

;; replace region or line with clipboard
(defun sk/replace-region-or-line-with-clipboard ()
  "replace the region or line with contents of the clipboard"
  (interactive)
  (if (region-active-p)
	  (sk/replace-region-with-clipboard)
	(sk/select-inside-line)
	(sk/replace-region-with-clipboard)))

;; clipboard copy function
(defun sk/copy-line-or-region-to-clipboard ()
  "Copies the line or region to the system clipboard"
  (interactive)
  (if (region-active-p)
	  (sk/copy-to-clipboard)
	(sk/select-inside-line)
	(sk/copy-to-clipboard)))

;; shell command on region
(defun sk/shell-command-region ()
  "Asks for a command and executes it in inferior shell with current buffer as input."
  (interactive)
  (let ((current-prefix-arg 4)) ;; emulate C-u
	(call-interactively 'shell-command-on-region)))

;; shell command on region or line
(defun sk/shell-command-region-or-line ()
  "Asks for a command and executes it in inferior shell with current buffer as input."
  (interactive)
  (if (region-active-p)
	  (sk/shell-command-region)
	(sk/select-inside-line)
	(sk/shell-command-region)))

;; provide these functions
(provide 'sk-editing-defuns)
