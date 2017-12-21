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

;; provide these functions
(provide 'sk-editing-defuns)
