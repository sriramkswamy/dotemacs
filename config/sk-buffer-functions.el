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

;; split ielm
(defun sk/ielm ()
  "open ielm REPL in a split window"
  (interactive)
  (split-window-horizontally)
  (ielm)
  (other-window 1))

;; easier way to make desktops
(defun sk/make-desktop ()
  "opens dired in the desktop directory to make a new folder"
  (interactive)
  (dired (concat user-emacs-directory "desktops")))
(bind-key* "C-x g" 'sk/make-desktop)

;; open dropbox folder
(defun sk/open-dropbox ()
  "opens dired in the dropbox directory to make a new folder"
  (interactive)
  (dired "~/Dropbox"))

;; dummy function
(defun sk/nothing ()
  "Sends a message saying nothing here"
  (interactive)
  (message "Nothing here!"))

;; occur at point
(defun sk/occur-at-point ()
  "occur with the thing at point"
  (interactive)
  (occur (thing-at-point 'symbol)))

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

;; kill current buffer or prompt for buffer
(defun sk/kill-buffer (askp)
   "with no prefix, kill the current buffer without prompt
 with prefix, select which buffer to kill"
   (interactive "P")
   (if askp
	   (kill-buffer (read-buffer
						  "Kill buffer: "
						  (mapcar #'buffer-name (buffer-list))))
	 (kill-this-buffer)))
(bind-key* "C-x k" 'sk/kill-buffer)

;; Doc View autofit mode - http://stackoverflow.com/questions/23236555/making-document-view-in-emacs-fit-to-width-of-page

;; set and remove breakpoints
(defun sk/breakpoint-icon-set ()
  "set breakpoint and icon"
  (interactive)
  (require 'gdb-mi)
  (gdb-put-breakpoint-icon t (concat (buffer-file-name) " "
									 (number-to-string (line-number-at-pos)))))

(defun sk/breakpoint-icon-remove ()
  "remove breakpoint and icon"
  (interactive)
  (require 'gdb-mi)
  (gdb-remove-breakpoint-icons (line-beginning-position) (line-end-position)))

;; provide this configuration
(provide 'sk-buffer-functions)
