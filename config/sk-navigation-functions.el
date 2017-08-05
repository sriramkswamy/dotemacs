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

;;; https://fuco1.github.io/2017-05-06-Enhanced-beginning--and-end-of-buffer-in-special-mode-buffers-(dired-etc.).html
;; smarter move beginning of buffer
(defmacro sk/special-beginning-of-buffer (mode &rest forms)
  "Define a special version of `beginning-of-buffer' in MODE.

The special function is defined such that the point first moves
to `point-min' and then FORMS are evaluated.  If the point did
not change because of the evaluation of FORMS, jump
unconditionally to `point-min'.  This way repeated invocations
toggle between real beginning and logical beginning of the
buffer."
  (declare (indent 1))
  (let ((fname (intern (concat "sk/" (symbol-name mode) "-beginning-of-buffer")))
        (mode-map (intern (concat (symbol-name mode) "-mode-map")))
        (mode-hook (intern (concat (symbol-name mode) "-mode-hook"))))
    `(progn
       (defun ,fname ()
         (interactive)
         (let ((p (point)))
           (goto-char (point-min))
           ,@forms
           (when (= p (point))
             (goto-char (point-min)))))
       (add-hook ',mode-hook
                 (lambda ()
                   (define-key ,mode-map
                     [remap beginning-of-buffer] ',fname))))))

;; smart end of buffer
(defmacro sk/special-end-of-buffer (mode &rest forms)
  "Define a special version of `end-of-buffer' in MODE.

The special function is defined such that the point first moves
to `point-max' and then FORMS are evaluated.  If the point did
not change because of the evaluation of FORMS, jump
unconditionally to `point-max'.  This way repeated invocations
toggle between real end and logical end of the buffer."
  (declare (indent 1))
  (let ((fname (intern (concat "sk/" (symbol-name mode) "-end-of-buffer")))
        (mode-map (intern (concat (symbol-name mode) "-mode-map")))
        (mode-hook (intern (concat (symbol-name mode) "-mode-hook"))))
    `(progn
       (defun ,fname ()
         (interactive)
         (let ((p (point)))
           (goto-char (point-max))
           ,@forms
           (when (= p (point))
             (goto-char (point-max)))))
       (add-hook ',mode-hook
                 (lambda ()
                   (define-key ,mode-map
                     [remap end-of-buffer] ',fname))))))

;;; map the above two functions to various modes

;; dired
(sk/special-beginning-of-buffer
 dired (while (not (ignore-errors (dired-get-filename)))
		 (dired-next-line 1)))
(sk/special-end-of-buffer
 dired (dired-(point)revious-line 1))

;; ibuffer
(sk/special-beginning-of-buffer
 ibuffer (ibuffer-forward-line 1))
(sk/special-end-of-buffer
 ibuffer (ibuffer-backward-line 1))

;; occur
(sk/special-beginning-of-buffer
 occur (occur-next 1))
(sk/special-end-of-buffer
 occur (occur-prev 1))

;; org-agenda
(sk/special-beginning-of-buffer
 org-agenda (org-agenda-next-item 1))
(sk/special-end-of-buffer
 org-agenda (org-agenda-previous-item 1))

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
 ("C-x 3"	. sk/split-right-and-move)
 ("C-x C-3" . sk/split-right-and-move)
 ("C-x 2"	. sk/split-below-and-move)
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

;; find the init file
(defun sk/find-init ()
  "Find file at point other window"
  (interactive)
  (sk/split-right-and-move)
  (find-file (concat user-emacs-directory "init.el")))

;; document in other window
(defun sk/other-doc-down ()
  "Other document next page"
  (interactive)
  (other-window 1)
  (cond
   ((eq system-type 'darwin)
	(if (daemonp)
		(pdf-view-next-page)
	  (doc-view-next-page)))
   ((eq system-type 'gnu/linux)
	(pdf-view-next-page))
   ((eq system-type 'windows-nt)
	(pdf-view-next-page)))
  (other-window 1))
(defun sk/other-doc-up ()
  "Other document prev page"
  (interactive)
  (other-window 1)
  (cond
   ((eq system-type 'darwin)
	(if (daemonp)
		(pdf-view-previous-page)
	  (doc-view-previous-page)))
   ((eq system-type 'gnu/linux)
	(pdf-view-previous-page))
   ((eq system-type 'windows-nt)
	(pdf-view-next-page)))
  (other-window 1))

;; document fit to page in window
(defun sk/doc-fit ()
  "Document next page"
  (interactive)
  (cond
   ((eq system-type 'darwin)
	(if (daemonp)
		(pdf-view-fit-page-to-window)
	  (doc-view-fit-page-to-window)))
   ((eq system-type 'gnu/linux)
	(pdf-view-next-page))
   ((eq system-type 'windows-nt)
	(pdf-view-fit-page-to-window))))

;; document fit to page in other window
(defun sk/other-doc-fit ()
  "Other document next page"
  (interactive)
  (other-window 1)
  (cond
   ((eq system-type 'darwin)
	(if (daemonp)
		(pdf-view-fit-page-to-window)
	  (doc-view-fit-page-to-window)))
   ((eq system-type 'gnu/linux)
	(pdf-view-next-page))
   ((eq system-type 'windows-nt)
	(pdf-view-fit-page-to-window)))
  (other-window 1))

;; add interleave note for the pdf in the other window
(defun sk/interleave-other-window-note ()
  "Add interleaved note for the page in other window"
  (interactive)
  (sk/enable-ryo-modal-mode)
  (other-window 1)
  (interleave-add-note)
  (sk/disable-ryo-modal-mode))

;; add interleave next page in other window
(defun sk/interleave-other-window-next ()
  "Move the next page in the other window when interleaving"
  (interactive)
  (sk/enable-ryo-modal-mode)
  (other-window 1)
  (interleave-go-to-next-page)
  (other-window 1))

;; add interleave previous page in other window
(defun sk/interleave-other-window-previous ()
  "Move the previous page in the other window when interleaving"
  (interactive)
  (sk/enable-ryo-modal-mode)
  (other-window 1)
  (interleave-go-to-previous-page)
  (other-window 1))

;; jump to frame
(defun sk/frame-jump (args)
  "jump to other frame and provide names when called with universal argument"
  (interactive "P")
  (if args
	  (call-interactively #'select-frame-by-name)
	(other-frame 1)))

;; https://fuco1.github.io/2017-05-01-Support-for-imenu-in-dired.html
;; imenu in dired
(defun sk/dired-imenu-prev-index-position (&optional arg)
  "Go to the header line of previous directory."
  (interactive "p")
  (unless (= (line-number-at-pos) 1)
    (call-interactively 'dired-prev-subdir)
    t))

(defun sk/dired-extract-index-name ()
  "Extract name of the current item for imenu."
  (save-excursion
    (back-to-indentation)
    (buffer-substring-no-properties
     (point)
     (1- (re-search-forward ":$")))))

(defun sk/dired-imenu-create-index ()
  "Create `imenu' index for dired."
  (let* ((alist (imenu-default-create-index-function))
         (uniquified (f-uniquify-alist (-map 'car alist))))
    (--remove
     (= 0 (length (car it)))
     (--map (cons (cdr (assoc (car it) uniquified)) (cdr it))
            alist))))

(defun sk/dired-imenu-init ()
  "Initialize `imenu' variables in current buffer."
  (setq-local imenu-prev-index-position-function
              'sk/dired-imenu-prev-index-position)
  (setq-local imenu-extract-index-name-function
              'sk/dired-extract-index-name)
  (setq-local imenu-create-index-function
              'sk/dired-imenu-create-index))
(add-hook 'dired-mode-hook 'sk/dired-imenu-init)

;; go to my personal markdown wiki file index
(defun sk/markdown-wiki-index ()
  "go to my personal markdown wiki index"
  (interactive)
  (find-file "~/Dropbox/notes/index.txt"))

;; go to my personal diary wiki file index
(defun sk/markdown-diary-index ()
  "go to my personal diary"
  (interactive)
  (find-file "~/Dropbox/notes/diary/diary.txt"))

;; provide this configuration
(provide 'sk-navigation-functions)
