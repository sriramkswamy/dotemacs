;; find the init file
(defun sk/find-init ()
  "Find file at point other window"
  (interactive)
  (sk/split-right-and-move)
  (find-file (concat user-emacs-directory "init.el")))

;; open dropbox folder
(defun sk/open-dropbox ()
  "opens dired in the dropbox directory to make a new folder"
  (interactive)
  (dired "~/Dropbox"))

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

;; center the other window
(defun sk/recenter-other-window ()
  "center the other window"
  (interactive)
  (sk/enable-ryo-modal-mode)
  (other-window 1)
  (recenter-top-bottom)
  (other-window 1))

;; scroll adjacent windows
(defun sk/other-window-up ()
  "Scrolls up in adjoining window"
  (interactive)
  (other-window 1)
  (scroll-down-command)
  (other-window 1))

;; jump from one desktop to another
(defun sk/desktop-jump ()
  "Clears the current desktop and prompts for the next one"
  (interactive)
  (desktop-save desktop-dirname)
  (desktop-save-mode -1)
  (delete-other-frames)
  (desktop-clear)
  (make-frame-command)
  (delete-other-frames)
  (call-interactively #'desktop-change-dir))

;; easier way to make desktops
(defun sk/make-desktop ()
  "opens dired in the desktop directory to make a new folder"
  (interactive)
  (dired (concat user-emacs-directory "desktops")))

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

;; goto closest number
(require 's)
(defun sk/goto-closest-number ()
  (interactive)
  (let ((closest-behind (save-excursion (search-backward-regexp "[0-9]" nil t)))
		(closest-ahead (save-excursion (search-forward-regexp "[0-9]" nil t))))
	(push-mark)
	(goto-char
	 (cond
	  ((and (not closest-ahead) (not closest-behind)) (error "No numbers in buffer"))
	  ((and closest-ahead (not closest-behind)) closest-ahead)
	  ((and closest-behind (not closest-ahead)) closest-behind)
	  ((> (- closest-ahead (point)) (- (point) closest-behind)) closest-behind)
	  ((> (- (point) closest-behind) (- closest-ahead (point))) closest-ahead)
	  :else closest-ahead))))

;; jump to frame
(defun sk/frame-jump (args)
  "jump to other frame and provide names when called with universal argument"
  (interactive "P")
  (if args
	  (call-interactively #'select-frame-by-name)
	(other-frame 1)))

;; emulating % - https://superuser.com/a/354236
(defun sk/goto-matching-paren ()
  "If point is sitting on a parenthetic character, jump to its match."
  (interactive)
  (cond ((looking-at "\\s\(") (forward-list 1))
        ((progn
           (backward-char 1)
           (looking-at "\\s\)"))
		 (forward-char 1) (backward-list 1))))

;; provide these functions
(provide 'sk-navigation-defuns)
