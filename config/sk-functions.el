;; non-native fullscreen
(defun sk/toggle-frame-fullscreen-non-native ()
  "Toggle full screen non-natively. Uses the `fullboth' frame paramerter
   rather than `fullscreen'. Useful to fullscreen on OSX w/o animations."
  (interactive)
  (modify-frame-parameters
   nil
   `((maximized
      . ,(unless (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
	   (frame-parameter nil 'fullscreen)))
     (fullscreen
      . ,(if (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
	     (if (eq (frame-parameter nil 'maximized) 'maximized)
		 'maximized)
	   'fullboth)))))
(bind-key* "C-z" #'sk/toggle-frame-fullscreen-non-native)

;; smarter start of line
(defun sk/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))
  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'sk/smarter-move-beginning-of-line)

;; Flyspell previous error
(defun sk/flyspell-goto-previous-error (arg)
  "Go to arg previous spelling error."
  (interactive "p")
  (while (not (= 0 arg))
    (let ((pos (point))
          (min (point-min)))
      (if (and (eq (current-buffer) flyspell-old-buffer-error)
               (eq pos flyspell-old-pos-error))
          (progn
            (if (= flyspell-old-pos-error min)
                ;; goto beginning of buffer
                (progn
                  (message "Restarting from end of buffer")
                  (goto-char (point-max)))
              (backward-word 1))
            (setq pos (point))))
      ;; seek the next error
      (while (and (> pos min)
                  (let ((ovs (overlays-at pos))
                        (r '()))
                    (while (and (not r) (consp ovs))
                      (if (flyspell-overlay-p (car ovs))
                          (setq r t)
                        (setq ovs (cdr ovs))))
                    (not r)))
        (backward-word 1)
        (setq pos (point)))
      ;; save the current location for next invocation
      (setq arg (1- arg))
      (setq flyspell-old-pos-error pos)
      (setq flyspell-old-buffer-error (current-buffer))
      (goto-char pos)
      (if (= pos min)
          (progn
            (message "No more miss-spelled word!")
            (setq arg 0))
        (forward-word)))))
;; map the function
(bind-keys*
 ("C-c <" . sk/flyspell-goto-previous-error)
 ("C-c >" . sk/flyspell-goto-next-error))

;; window movements
(defun sk/split-below-and-move ()
  "split the window horizontally and move there"
  (interactive)
  (split-window-below)
  (other-window 1))
(defun sk/split-right-and-move ()
  "split the window vertically and move there"
  (interactive)
  (split-window-right)
  (other-window 1))
;; map these to defaults
(bind-keys*
 ("C-x 3" . sk/split-right-and-move)
 ("C-x C-3" . sk/split-right-and-move)
 ("C-x 2" . sk/split-below-and-move)
 ("C-x C-2" . sk/split-below-and-move))

;; rotate window config
(defun sk/rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))
                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))
                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

;; scroll adjacent windows
(defun sk/other-window-up ()
  "Scrolls up in adjoining window"
  (interactive)
  (other-window 1)
  (scroll-down-command)
  (other-window 1))
(bind-key* "C-x M-v" 'sk/other-window-up)

;; browse the current HTML file in browser
(defun sk/browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
	  (browse-url (concat "file://" file-name)))))
(bind-key* "C-c g B" 'sk/browse-current-file)

;; alignment functions
(defun sk/align-whitespace (start end)
  "Align columns by whitespace"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\s-" 1 0 t))
(defun sk/align-ampersand (start end)
  "Align columns by ampersand"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)&" 1 1 t))
(defun sk/align-quote-space (start end)
  "Align columns by quote and space"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\).*\\s-\"" 1 0 t))
(defun sk/align-equals (start end)
  "Align columns by equals sign"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)=" 1 0 t))
(defun sk/align-comma (start end)
  "Align columns by comma"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)," 1 1 t))
(defun sk/align-dot (start end)
  "Align columns by dot"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\\." 1 1 t))
(defun sk/align-colon (start end)
  "Align columns by equals sign"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\):" 1 0 t))
(bind-keys*
 ("C-c w SPC" . sk/align-whitespace)
 ("C-c w a" . sk/align-ampersand)
 ("C-c w q" . sk/align-quote-space)
 ("C-c w =" . sk/align-equals)
 ("C-c w ," . sk/align-comma)
 ("C-c w :" . sk/align-colon)
 ("C-c w ." . sk/align-dot))

;; rename buffer and file
(defun sk/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

;; delete buffer and file
(defun sk/delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

;; copy the file path
(defun sk/copy-current-file-path ()
  "Add current file path to kill ring. Limits the filename to project root if possible."
  (interactive)
  (kill-new buffer-file-name))

;; map them convenience functions
(bind-keys*
 ("C-c k r" . sk/rename-current-buffer-file)
 ("C-c k d" . sk/delete-current-buffer-file)
 ("C-c k p" . sk/copy-current-file-path))

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

;; Correct the DOuble capitals
(defun sk/dcaps-to-scaps ()
  "Convert word in DOuble CApitals to Single Capitals."
  (interactive)
  (and (= ?w (char-syntax (char-before)))
       (save-excursion
         (and (if (called-interactively-p)
                  (skip-syntax-backward "w")
                (= -3 (skip-syntax-backward "w")))
              (let (case-fold-search)
                (looking-at "\\b[[:upper:]]\\{2\\}[[:lower:]]"))
              (capitalize-word 1)))))
(define-minor-mode sk/dubcaps-mode
  "Toggle `sk/dubcaps-mode'.  Converts words in DOuble CApitals to
Single Capitals as you type."
  :init-value nil
  (if sk/dubcaps-mode
      (add-hook 'post-self-insert-hook #'sk/dcaps-to-scaps nil 'local)
    (remove-hook 'post-self-insert-hook #'sk/dcaps-to-scaps 'local)))
(add-hook 'text-mode-hook #'sk/dubcaps-mode)

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
(bind-key* "C-c =" 'sk/ispell-word-then-abbrev)

;; Set fonts
(cond ((eq system-type 'gnu/linux)                                             ; if system is GNU/Linux
       (set-frame-font "DejaVu Sans Mono"))                                    ; set the font to DejaVu Sans Mono
      ((eq system-type 'darwin)                                                ; if system is macOS
       (set-frame-font "Monaco"))                                              ; set the font to Monaco
      ((eq system-type 'windows-nt)                                            ; if system is Windows
       (set-frame-font "Lucida Sans Typewriter")))                             ; set the font to Lucida Sans Typewriter

;; Some convenience functions for changing fonts
;;;###autoload
(defun sk/courier-font ()
  "Change font to Courier"
  (interactive)
  (set-face-attribute 'default nil :font "Courier")
    (set-frame-width (selected-frame) 97))
;;;###autoload
(defun sk/georgia-font ()
  "Change font to Georgia"
  (interactive)
  (set-face-attribute 'default nil :font "Georgia" :height 160))
;;;###autoload
(defun sk/hack-font ()
  "Change font to Hack"
  (interactive)
  (set-face-attribute 'default nil :font "Hack"))
;;;###autoload
(defun sk/monaco-font ()
  "Change font to Monaco"
  (interactive)
  (set-face-attribute 'default nil :font "Monaco"))
;;;###autoload
(defun sk/consolas-font ()
  "Change font to Consolas"
  (interactive)
  (set-face-attribute 'default nil :font "Consolas"))
;;;###autoload
(defun sk/deja-vu-font ()
  "Change font to DejaVu Sans Mono"
  (interactive)
  (set-face-attribute 'default nil :font "DejaVu Sans Mono"))

;; Some easy functions for font types
;;;###autoload
(defun sk/tiny-type ()
  "Reduce the font size to tiny"
  (interactive)
  (set-face-attribute 'default nil  :height 150))
;;;###autoload
(defun sk/miniscule-type ()
  "Reduce the font size to miniscule"
  (interactive)
  (set-face-attribute 'default nil  :height 140))
;;;###autoload
(defun sk/small-type ()
  "Reduce the font size to small"
  (interactive)
  (set-face-attribute 'default nil  :height 190)
  (set-frame-width (selected-frame) 89))
;;;###autoload
(defun sk/medium-type ()
  "Reduce the font size to medium"
  (interactive)
  (set-face-attribute 'default nil  :height 215)
  (set-frame-width (selected-frame) 89))
;;;###autoload
(defun sk/large-type ()
  "Reduce the font size to large"
  (interactive)
  (set-face-attribute 'default nil  :height 350)
  (set-frame-width (selected-frame) 68))

;; close compilation buffer automatically if no errors
(setq compilation-finish-functions
      (lambda (buf str)
        (if (null (string-match ".*exited abnormally.*" str))
            ;;no errors, make the compilation window go away in a few seconds
            (progn
              (run-at-time "0.4 sec" nil
                           (lambda ()
                             (select-window (get-buffer-window (get-buffer-create "*compilation*")))
                             (switch-to-buffer nil)))
              (message "No Compilation Errors!")))))

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
(bind-keys
 ("C-o" . sk/open-line-above)
 ("M-o" . sk/open-line-below))

;; backward kill word or region
(defun sk/kill-region-or-backward-word ()
  (interactive)
  (if (region-active-p)
	  (kill-region (region-beginning) (region-end))
	(backward-kill-word 1)))
(bind-key* "C-w" 'sk/kill-region-or-backward-word)

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
(bind-key* "M-w" 'sk/copy-region-or-line)

;; duplicate region or line
(defun sk/duplicate-region (&optional num start end)
  "Duplicates the region bounded by START and END NUM times.
If no START and END is provided, the current region-beginning and
region-end is used."
  (interactive "p")
  (save-excursion
	(let* ((start (or start (region-beginning)))
		   (end (or end (region-end)))
		   (region (buffer-substring start end)))
	  (goto-char end)
	  (dotimes (i num)
		(insert region)))))
(defun sk/duplicate-current-line (&optional num)
  "Duplicate the current line NUM times."
  (interactive "p")
  (if (bound-and-true-p paredit-mode)
	  (paredit-sk/duplicate-current-line)
	(save-excursion
	  (when (eq (point-at-eol) (point-max))
		(goto-char (point-max))
		(newline)
		(forward-char -1))
	  (sk/duplicate-region num (point-at-bol) (1+ (point-at-eol))))))
(defun sk/duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated."
  (interactive "p")
  (if (region-active-p)
	  (let ((beg (region-beginning))
			(end (region-end)))
		(sk/duplicate-region arg beg end))
	(sk/duplicate-current-line arg)))
(bind-key* "C-c h" 'sk/duplicate-current-line-or-region)

;; Deactivate mark
(defun sk/remove-mark ()
  "Deactivate the region"
  (interactive)
  (if (region-active-p)
	  (deactivate-mark)))
(bind-key* "M-g" 'sk/remove-mark)

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
(bind-key* "M-c" 'sk/toggle-letter-case)

;; increase or decrease number at point
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

(bind-keys*
 ("C-c +" . sk/change-number-at-point)
 ("C-c -" . sk/subtract-number-at-point))

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

(bind-keys*
 ("C-r i l" . sk/select-inside-line)
 ("C-r a l" . sk/select-around-line)
 ("C-r C-i C-l" . sk/select-inside-line)
 ("C-r C-a C-l" . sk/select-around-line))

;; split ielm
(defun sk/ielm ()
  "open ielm REPL in a split window"
  (interactive)
  (split-window-horizontally)
  (ielm)
  (other-window 1))

;; provide this configuration
(provide 'sk-functions)
