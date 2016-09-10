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
(general-nmap :prefix sk--evil-global-leader                                   ; global leader prefix
	      "z" 'sk/toggle-frame-fullscreen-non-native)                      ; 'SPC z' - fullscreen

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
(general-nmap :prefix "["
	      "s" 'sk/flyspell-goto-previous-error)
(general-nmap :prefix "]"
	      "s" 'flyspell-goto-next-error)

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
(defun sk/other-window-down ()
  "Scrolls down in adjoining window"
  (interactive)
  (other-window 1)
  (scroll-up-command)
  (other-window 1))
(defun sk/other-window-up ()
  "Scrolls up in adjoining window"
  (interactive)
  (other-window 1)
  (scroll-down-command)
  (other-window 1))
(general-nmap "[v" 'sk/other-window-up)
(general-nmap "]v" 'sk/other-window-down)

;; turn the adjoining pdf
(defun sk/other-pdf-next ()
  "Turns the next page in adjoining PDF file"
  (interactive)
  (other-window 1)
  (doc-view-next-page)
  (other-window 1))
(defun sk/other-pdf-previous ()
  "Turns the previous page in adjoining PDF file"
  (interactive)
  (other-window 1)
  (doc-view-previous-page)
  (other-window 1))
(general-nmap "[d" 'sk/other-pdf-previous)
(general-nmap "]d" 'sk/other-pdf-next)

;; browse the current HTML file in browser
(defun sk/browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))
(general-nmap "gB" 'sk/browse-current-file)

;; alignment functions
(defun sk/align-whitespace (start end)
  "Align columns by whitespace"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\s-" 1 0 t))
(general-nvmap "gAs" 'sk/align-whitespace)
(defun sk/align-ampersand (start end)
  "Align columns by ampersand"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)&" 1 1 t))
(general-nvmap "gAa" 'sk/align-ampersand)
(defun sk/align-quote-space (start end)
  "Align columns by quote and space"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\).*\\s-\"" 1 0 t))
(general-nvmap "gAq" 'sk/align-quote-space)
(defun sk/align-equals (start end)
  "Align columns by equals sign"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)=" 1 0 t))
(general-nvmap "gA=" 'sk/align-equals)
(defun sk/align-comma (start end)
  "Align columns by comma"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)," 1 1 t))
(general-nvmap "gA," 'sk/align-comma)
(defun sk/align-dot (start end)
  "Align columns by dot"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\\." 1 1 t))
(general-nvmap "gA." 'sk/align-dot)
(defun sk/align-colon (start end)
  "Align columns by equals sign"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\):" 1 0 t))
(general-nvmap "gA:" 'sk/align-colon)

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
(general-nmap "gR" 'sk/rename-current-buffer-file)

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
(general-nmap :prefix sk--evil-global-leader
	      "K" 'sk/delete-current-buffer-file)

;; copy the file path
(defun sk/copy-current-file-path ()
  "Add current file path to kill ring. Limits the filename to project root if possible."
  (interactive)
  (kill-new buffer-file-name))
(general-nmap "gY" 'sk/copy-current-file-path)

;; Transpose words forward
(defun sk/transpose-words-forward ()
  "Transpose words forward"
  (interactive)
  (forward-word 1)
  (forward-char 1)
  (transpose-words 1)
  (backward-word 1))
(general-nmap "]w" 'sk/transpose-words-forward)
;; Transpose words backward
(defun sk/transpose-words-backward ()
  "Transpose words backward"
  (interactive)
  (transpose-words 1)
  (backward-word 1))
(general-nmap "[w" 'sk/transpose-words-backward)

;; Transpose chars forward
(defun sk/transpose-chars-forward ()
  "Transpose chars forward"
  (interactive)
  (forward-char 1)
  (transpose-chars 1)
  (backward-char 1))
(general-nmap "]c" 'sk/transpose-chars-forward)
;; Transpose chars backward
(defun sk/transpose-chars-backward ()
  "Transpose chars backward"
  (interactive)
  (transpose-chars 1)
  (backward-char 1))
(general-nmap "[c" 'sk/transpose-chars-backward)

;; duplicate line or region
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
  (save-excursion
    (when (eq (point-at-eol) (point-max))
      (goto-char (point-max))
      (newline)
      (forward-char -1))
    (sk/duplicate-region num (point-at-bol) (1+ (point-at-eol)))))
(defun sk/duplicate-line-or-region (&optional num)
  "Duplicate the current line or region if active"
  (interactive "p")
  (if (region-active-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (sk/duplicate-region num beg end)))
  (sk/duplicate-current-line num))
(general-nmap "gD" 'sk/duplicate-line-or-region)

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
(general-nmap "[e" 'sk/move-text-up)
(general-nmap "]e" 'sk/move-text-down)

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
(general-define-key "C-=" 'sk/ispell-word-then-abbrev)

;; Set fonts
(cond ((eq system-type 'gnu/linux)                                             ; if system is GNU/Linux
       (set-frame-font "DejaVu Sans Mono"))                                    ; set the font to DejaVu Sans Mono
      ((eq system-type 'darwin)                                                ; if system is macOS
       (set-frame-font "Monaco"))                                              ; set the font to Monaco
      ((eq system-type 'windows-nt)                                            ; if system is Windows
       (set-frame-font "Lucida Sans Typewriter")))                             ; set the font to Lucida Sans Typewriter

;; Some convenience functions for changing fonts
(defun sk/courier-font ()
  "Change font to Courier"
  (interactive)
  (set-face-attribute 'default nil :font "Courier")
    (set-frame-width (selected-frame) 97))
(defun sk/georgia-font ()
  "Change font to Georgia"
  (interactive)
  (set-face-attribute 'default nil :font "Georgia" :height 160))
(defun sk/hack-font ()
  "Change font to Hack"
  (interactive)
  (set-face-attribute 'default nil :font "Hack"))
(defun sk/monaco-font ()
  "Change font to Monaco"
  (interactive)
  (set-face-attribute 'default nil :font "Monaco"))
(defun sk/consolas-font ()
  "Change font to Consolas"
  (interactive)
  (set-face-attribute 'default nil :font "Consolas"))
(defun sk/deja-vu-font ()
  "Change font to DejaVu Sans Mono"
  (interactive)
  (set-face-attribute 'default nil :font "DejaVu Sans Mono"))

;; Some easy functions for font types
(defun sk/tiny-type ()
  "Reduce the font size to tiny"
  (interactive)
  (set-face-attribute 'default nil  :height 150))
(defun sk/miniscule-type ()
  "Reduce the font size to miniscule"
  (interactive)
  (set-face-attribute 'default nil  :height 140))
(defun sk/small-type ()
  "Reduce the font size to small"
  (interactive)
  (set-face-attribute 'default nil  :height 190)
  (set-frame-width (selected-frame) 89))
(defun sk/medium-type ()
  "Reduce the font size to medium"
  (interactive)
  (set-face-attribute 'default nil  :height 215)
  (set-frame-width (selected-frame) 89))
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

;; blank lines
(defun sk/blank-line-up ()
  "create empty line up"
  (interactive)
  (save-excursion
    (beginning-of-line 1)
    (open-line 1)))
(defun sk/blank-line-down ()
  "create empty line down"
  (interactive)
  (save-excursion
    (end-of-line 1)
    (newline)))
(general-nmap "[o" 'sk/blank-line-up)
(general-nmap "]o" 'sk/blank-line-down)

;; blank chars
(defun sk/blank-char-before ()
  "create empty char before"
  (interactive)
  (save-excursion
    (insert " "))
 (forward-char 1))
(defun sk/blank-char-after ()
  "create empty char after"
  (interactive)
  (save-excursion
    (forward-char 1)
    (insert " ")))
(general-nmap "[b" 'sk/blank-char-before)
(general-nmap "]b" 'sk/blank-char-after)

;; provide this configuration
(provide 'sk-functions)
