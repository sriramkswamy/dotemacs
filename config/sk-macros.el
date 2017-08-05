;;; REPL interaction macros

(defun sk/concat (&rest syms)
  "Concatenate symbols together to create a single symbol."
  (intern (apply #'concatenate 'string (mapcar #'symbol-name syms))))

(defmacro sk/repl-paren-query (lang sending-function repl-query
									&optional tmux switching-function)
  "Generate a macro to create a function that queries the appropriate REPL for the language"
  `(defun ,(sk/concat 'sk/ lang '- repl-query '- tmux) ()
	 (interactive)

	 ;; set a bookmark to jump back here
	 (bookmark-set "repl")

	 ;; if tmux is nil, i.e., not sending to tmux
	 (if ,(not tmux)

		 ;; if comint mode is the underlying repl to send
		 (if (string-equal ,(symbol-name sending-function) "comint")

			 ;; region selected and comint
			 (if (region-active-p)
				 (let* ((sk/buffer-name (buffer-name))
						(sk/string-to-send (concat ,(symbol-name repl-query) "("
												   (buffer-substring
													(region-beginning) (region-end))
												   ")\n")))
				   (,switching-function)
				   (delete-other-windows)
				   (comint-send-string (get-buffer-process (current-buffer)) sk/string-to-send)
				   (split-window-sensibly))

			   ;; region not selected and comint
			   (let* ((sk/buffer-name (buffer-name))
					  (sk/string-to-send (concat ,(symbol-name repl-query) "("
												 (thing-at-point 'symbol) ")\n")))
				 (,switching-function)
				 (delete-other-windows)
				 (comint-send-string (get-buffer-process (current-buffer)) sk/string-to-send)
				 (split-window-sensibly)))

		   ;; region selected and sending-function specified (not comint)
		   (if (region-active-p)
			   (let* ((sk/buffer-name (buffer-name))
					  (sk/string-to-send (concat ,(symbol-name repl-query) "("
												 (buffer-substring
												  (region-beginning) (region-end))
												 ")")))
				 (,sending-function sk/string-to-send))

			 ;; region not selected and sending-function specified
			 (let* ((sk/buffer-name (buffer-name))
					(sk/string-to-send (concat ,(symbol-name repl-query) "("
											   (thing-at-point 'symbol) ")")))
			   (,sending-function sk/string-to-send))))

	   ;; If tmux is true, send it to tmux
	   (if (region-active-p)
		   (let* ((sk/buffer-name (buffer-name))
				  (sk/string-to-send (concat ,(symbol-name repl-query) "("
											 (buffer-substring
											  (region-beginning) (region-end))
											 ")")))
			 (emamux:send-command sk/string-to-send))

		 (let* ((sk/buffer-name (buffer-name))
				(sk/string-to-send (concat ,(symbol-name repl-query) "("
										   (thing-at-point 'symbol) ")")))
		   (emamux:send-command sk/string-to-send))))

	 ;; jump back to the set bookmark
	 (bookmark-jump "repl")))

(defmacro sk/repl-space-query (lang sending-function repl-query
									&optional tmux switching-function)
  "Generate a macro to create a function that queries the appropriate REPL for the language"
  `(defun ,(sk/concat 'sk/ lang '- repl-query '- tmux) ()
	 (interactive)

	 ;; set a bookmark to jump back here
	 (bookmark-set "repl")

	 ;; if tmux is nil, i.e., not sending to tmux
	 (if ,(not tmux)

		 ;; if comint mode is the underlying repl to send
		 (if (string-equal ,(symbol-name sending-function) "comint")

			 ;; region selected and comint
			 (if (region-active-p)
				 (let* ((sk/buffer-name (buffer-name))
						(sk/string-to-send (concat ,(symbol-name repl-query) " "
												   (buffer-substring
													(region-beginning) (region-end))
												   "\n")))
				   (,switching-function)
				   (delete-other-windows)
				   (comint-send-string (get-buffer-process (current-buffer)) sk/string-to-send)
				   (split-window-sensibly))

			   ;; region not selected and comint
			   (let* ((sk/buffer-name (buffer-name))
					  (sk/string-to-send (concat ,(symbol-name repl-query) " "
												 (thing-at-point 'symbol) "\n")))
				 (,switching-function)
				 (delete-other-windows)
				 (comint-send-string (get-buffer-process (current-buffer)) sk/string-to-send)
				 (split-window-sensibly)))

		   ;; region selected and sending-function specified (not comint)
		   (if (region-active-p)
			   (let* ((sk/buffer-name (buffer-name))
					  (sk/string-to-send (concat ,(symbol-name repl-query) " "
												 (buffer-substring
												  (region-beginning) (region-end)))))
				 (,sending-function sk/string-to-send))

			 ;; region not selected and sending-function specified
			 (let* ((sk/buffer-name (buffer-name))
					(sk/string-to-send (concat ,(symbol-name repl-query) " "
											   (thing-at-point 'symbol))))
			   (,sending-function sk/string-to-send))))

	   ;; If tmux is true, send it to tmux
	   (if (region-active-p)
		   (let* ((sk/buffer-name (buffer-name))
				  (sk/string-to-send (concat ,(symbol-name repl-query) " "
											 (buffer-substring
											  (region-beginning) (region-end)))))
			 (emamux:send-command sk/string-to-send))

		 (let* ((sk/buffer-name (buffer-name))
				(sk/string-to-send (concat ,(symbol-name repl-query) " "
										   (thing-at-point 'symbol))))
		   (emamux:send-command sk/string-to-send))))

	 ;; jump back to the set bookmark
	 (bookmark-jump "repl")))

(defmacro sk/repl-dot-query (lang sending-function repl-query
								  &optional tmux switching-function)
  "Generate a macro to create a function that queries the appropriate REPL for the language"
  `(defun ,(sk/concat 'sk/ lang '- repl-query '- tmux) ()
	 (interactive)

	 ;; set a bookmark to jump back here
	 (bookmark-set "repl")

	 ;; if tmux is nil, i.e., not sending to tmux
	 (if ,(not tmux)

		 ;; if comint mode is the underlying repl to send
		 (if (string-equal ,(symbol-name sending-function) "comint")

			 ;; region selected and comint
			 (if (region-active-p)
				 (let* ((sk/buffer-name (buffer-name))
						(sk/string-to-send (concat (buffer-substring
													(region-beginning) (region-end))
												   "." ,(symbol-name repl-query)
												   "\n")))
				   (,switching-function)
				   (delete-other-windows)
				   (comint-send-string (get-buffer-process (current-buffer)) sk/string-to-send)
				   (split-window-sensibly))

			   ;; region not selected and comint
			   (let* ((sk/buffer-name (buffer-name))
					  (sk/string-to-send (concat (thing-at-point 'symbol) "."
												 ,(symbol-name repl-query)
												 "\n")))
				 (,switching-function)
				 (delete-other-windows)
				 (comint-send-string (get-buffer-process (current-buffer)) sk/string-to-send)
				 (split-window-sensibly)))

		   ;; region selected and sending-function specified (not comint)
		   (if (region-active-p)
			   (let* ((sk/buffer-name (buffer-name))
					  (sk/string-to-send (concat (buffer-substring
												  (region-beginning) (region-end))
												 "." ,(symbol-name repl-query))))
				 (,sending-function sk/string-to-send))

			 ;; region not selected and sending-function specified
			 (let* ((sk/buffer-name (buffer-name))
					(sk/string-to-send (concat (thing-at-point 'symbol) "."
											   ,(symbol-name repl-query))))
			   (,sending-function sk/string-to-send))))

	   ;; If tmux is true, send it to tmux
	   (if (region-active-p)
		   (let* ((sk/buffer-name (buffer-name))
				  (sk/string-to-send (concat (buffer-substring
											  (region-beginning) (region-end))
											 "." ,(symbol-name repl-query))))
			 (emamux:send-command sk/string-to-send))

		 (let* ((sk/buffer-name (buffer-name))
				(sk/string-to-send (concat (thing-at-point 'symbol)
										   "." ,(symbol-name repl-query))))
		   (emamux:send-command sk/string-to-send))))

	 ;; jump back to the set bookmark
	 (bookmark-jump "repl")))

(defmacro sk/repl-dot-paren-query (lang sending-function repl-query
										&optional tmux switching-function)
  "Generate a macro to create a function that queries the appropriate REPL for the language"
  `(defun ,(sk/concat 'sk/ lang '- repl-query '- tmux) ()
	 (interactive)

	 ;; set a bookmark to jump back here
	 (bookmark-set "repl")

	 ;; if tmux is nil, i.e., not sending to tmux
	 (if ,(not tmux)

		 ;; if comint mode is the underlying repl to send
		 (if (string-equal ,(symbol-name sending-function) "comint")

			 ;; region selected and comint
			 (if (region-active-p)
				 (let* ((sk/buffer-name (buffer-name))
						(sk/string-to-send (concat (buffer-substring
													(region-beginning) (region-end))
												   "." ,(symbol-name repl-query)
												   "()\n")))
				   (,switching-function)
				   (delete-other-windows)
				   (comint-send-string (get-buffer-process (current-buffer)) sk/string-to-send)
				   (split-window-sensibly))

			   ;; region not selected and comint
			   (let* ((sk/buffer-name (buffer-name))
					  (sk/string-to-send (concat (thing-at-point 'symbol) "."
												 ,(symbol-name repl-query)
												 "()\n")))
				 (,switching-function)
				 (delete-other-windows)
				 (comint-send-string (get-buffer-process (current-buffer)) sk/string-to-send)
				 (split-window-sensibly)))

		   ;; region selected and sending-function specified (not comint)
		   (if (region-active-p)
			   (let* ((sk/buffer-name (buffer-name))
					  (sk/string-to-send (concat (buffer-substring
												  (region-beginning) (region-end))
												 "." ,(symbol-name repl-query) "()")))
				 (,sending-function sk/string-to-send))

			 ;; region not selected and sending-function specified
			 (let* ((sk/buffer-name (buffer-name))
					(sk/string-to-send (concat (thing-at-point 'symbol) "."
											   ,(symbol-name repl-query) "()")))
			   (,sending-function sk/string-to-send))))

	   ;; If tmux is true, send it to tmux
	   (if (region-active-p)
		   (let* ((sk/buffer-name (buffer-name))
				  (sk/string-to-send (concat (buffer-substring
											  (region-beginning) (region-end))
											 "." ,(symbol-name repl-query) "()")))
			 (emamux:send-command sk/string-to-send))

		 (let* ((sk/buffer-name (buffer-name))
				(sk/string-to-send (concat (thing-at-point 'symbol)
										   "." ,(symbol-name repl-query) "()")))
		   (emamux:send-command sk/string-to-send))))

	 ;; jump back to the set bookmark
	 (bookmark-jump "repl")))

(defmacro sk/repl-query (lang sending-function repl-query
							  &optional tmux switching-function)
  "Generate a macro to create a function that queries the appropriate REPL for the language"
  `(defun ,(sk/concat 'sk/ lang '- repl-query '- tmux) ()
	 (interactive)

	 ;; set a bookmark to jump back here
	 (bookmark-set "repl")

	 ;; if tmux is nil, i.e., not sending to tmux
	 (if ,(not tmux)

		 ;; if comint mode is the underlying repl to send
		 (if (string-equal ,(symbol-name sending-function) "comint")

			 (let* ((sk/buffer-name (buffer-name))
					(sk/string-to-send (concat ,(symbol-name repl-query) "\n")))
			   (,switching-function)
			   (delete-other-windows)
			   (comint-send-string (get-buffer-process (current-buffer)) sk/string-to-send)
			   (split-window-sensibly))

		   ;; if comint mode is not the underlying repl to send
		   (let* ((sk/buffer-name (buffer-name))
				  (sk/string-to-send (concat ,(symbol-name repl-query))))
			 (,sending-function sk/string-to-send)))

	   ;; If tmux is true, send it to tmux
	   (let* ((sk/buffer-name (buffer-name))
			  (sk/string-to-send (concat ,(symbol-name repl-query))))
		 (emamux:send-command sk/string-to-send)))

	 ;; jump back to the set bookmark
	 (bookmark-jump "repl")))

(defmacro sk/repl-string-query (lang sending-function query-name repl-query
									 &optional tmux switching-function)
  "Generate a macro to create a function that queries the appropriate REPL for the language"
  `(defun ,(sk/concat 'sk/ lang '- query-name '- tmux) ()
	 (interactive)

	 ;; set a bookmark to jump back here
	 (bookmark-set "repl")

	 ;; if tmux is nil, i.e., not sending to tmux
	 (if ,(not tmux)

		 ;; if comint mode is the underlying repl to send
		 (if (string-equal ,(symbol-name sending-function) "comint")

			 (let* ((sk/buffer-name (buffer-name))
					(sk/string-to-send (concat ,repl-query "\n")))
			   (,switching-function)
			   (delete-other-windows)
			   (comint-send-string (get-buffer-process (current-buffer)) sk/string-to-send)
			   (split-window-sensibly))

		   ;; if comint mode is not the underlying repl to send
		   (let* ((sk/buffer-name (buffer-name))
				  (sk/string-to-send (concat ,repl-query)))
			 (,sending-function sk/string-to-send)))

	   ;; If tmux is true, send it to tmux
	   (let* ((sk/buffer-name (buffer-name))
			  (sk/string-to-send (concat ,repl-query)))
		 (emamux:send-command sk/string-to-send)))

	 ;; jump back to the set bookmark
	 (bookmark-jump "repl")))

(defmacro sk/matlab-debug-query (repl-query &optional tmux)
  "Generate a macro to create a function that queries the appropriate REPL for the language"
  `(defun ,(sk/concat 'sk/matlab- repl-query '- tmux) ()
	 (interactive)

	 ;; set a bookmark to jump back here
	 (bookmark-set "repl")

	 ;; if tmux is nil, i.e., not sending to tmux
	 (if ,(not tmux)

		 (let* ((sk/buffer-name (buffer-name))
				(sk/string-to-send (concat ,(symbol-name repl-query) " at "
										   (number-to-string (line-number-at-pos))
										   " in " sk/buffer-name "\n")))
		   (matlab-show-matlab-shell-buffer)
		   (delete-other-windows)
		   (comint-send-string (get-buffer-process (current-buffer)) sk/string-to-send)
		   (split-window-sensibly))

	   ;; If tmux is true, send it to tmux
	   (let* ((sk/buffer-name (buffer-name))
			  (sk/string-to-send (concat ,(symbol-name repl-query) " at "
										 (number-to-string (line-number-at-pos))
										 " in " sk/buffer-name)))
		 (emamux:send-command sk/string-to-send)))

	 ;; jump back to the set bookmark
	 (bookmark-jump "repl")))

;;; Ryo modal macros

(defmacro sk/ryo-operator-object (op-name prefix-key region-key op-function
										  &optional remove-mark lang-mode)
  "Generates a function for bindings based on operator and textobject schema"

  ;; global bindings
  (if (not lang-mode)

	  ;; if marks are removed automatically by the op-function
	  (if (not remove-mark)

		  (progn 

			`(defun ,(sk/concat 'sk/ryo- op-name '-bindings) ()
			   (interactive)

			   ;; operator/textobject sending
			   ;; region
			   (ryo-modal-key ,(concat prefix-key " " region-key) ',op-function)

			   ;; entire buffer
			   (ryo-modal-key ,(concat prefix-key " " "i a") 'mark-whole-buffer
							  :then '(,op-function))
			   (ryo-modal-key ,(concat prefix-key " " "a a") 'mark-whole-buffer
							  :then '(,op-function))

			   ;; word
			   (ryo-modal-key ,(concat prefix-key " " "i w") 'er/mark-word
							  :then '(,op-function))
			   (ryo-modal-key ,(concat prefix-key " " "a w") 'sk/mark-around-word
							  :then '(,op-function))

			   ;; markdown code block
			   (ryo-modal-key ,(concat prefix-key " " "i k") 'sk/mark-inside-markdown-code-block
							  :then '(,op-function))
			   (ryo-modal-key ,(concat prefix-key " " "a k") 'sk/mark-around-markdown-code-block
							  :then '(,op-function))

			   ;; para
			   (ryo-modal-key ,(concat prefix-key " " "i p") 'er/mark-text-paragraph
							  :then '(,op-function))
			   (ryo-modal-key ,(concat prefix-key " " "a p") 'mark-paragraph
							  :then '(,op-function))

			   ;; indented region
			   (ryo-modal-key ,(concat prefix-key " " "i i") 'indent-tools-select
							  :then '(,op-function))
			   (ryo-modal-key ,(concat prefix-key " " "a i") 'sk/select-indent-tree
							  :then '(,op-function))

			   ;; sentence
			   (ryo-modal-key ,(concat prefix-key " " "i s") 'er/mark-text-sentence
							  :then '(,op-function))
			   (ryo-modal-key ,(concat prefix-key " " "a s") 'er/mark-text-sentence
							  :then '(,op-function))

			   ;; line
			   (ryo-modal-key ,(concat prefix-key " " "i l") 'sk/select-inside-line
							  :then '(,op-function))
			   (ryo-modal-key ,(concat prefix-key " " "a l") 'sk/select-around-line
							  :then '(,op-function))

			   ;; symbol
			   (ryo-modal-key ,(concat prefix-key " " "i y") 'er/mark-symbol
							  :then '(,op-function))
			   (ryo-modal-key ,(concat prefix-key " " "a y") 'sk/mark-around-symbol
							  :then '(,op-function))

			   ;; comment
			   (ryo-modal-key ,(concat prefix-key " " "i c") 'er/mark-comment
							  :then '(,op-function))
			   (ryo-modal-key ,(concat prefix-key " " "a c") 'er/mark-comment
							  :then '(,op-function))

			   ;; function
			   (ryo-modal-key ,(concat prefix-key " " "i f") 'er/mark-defun
							  :then '(,op-function))
			   (ryo-modal-key ,(concat prefix-key " " "a f") 'er/mark-defun
							  :then '(,op-function))

			   ;; diff
			   (ryo-modal-key ,(concat prefix-key " " "i h") 'diff-hl-mark-hunk
							  :then '(,op-function))
			   (ryo-modal-key ,(concat prefix-key " " "a h") 'diff-hl-mark-hunk
							  :then '(,op-function))

			   ;; quotes
			   (ryo-modal-key ,(concat prefix-key " " "i q") 'er/mark-inside-quotes
							  :then '(,op-function))
			   (ryo-modal-key ,(concat prefix-key " " "a q") 'er/mark-outside-quotes
							  :then '(,op-function))

			   ;; pairs (), [], {}
			   (ryo-modal-key ,(concat prefix-key " " "i b") 'er/mark-inside-pairs
							  :then '(,op-function))
			   (ryo-modal-key ,(concat prefix-key " " "a b") 'er/mark-outside-pairs
							  :then '(,op-function))

			   ;; org code block
			   (ryo-modal-key ,(concat prefix-key " " "i o") 'sk/mark-inside-org-code
							  :then '(,op-function))
			   (ryo-modal-key ,(concat prefix-key " " "a o") 'er/mark-org-code-block
							  :then '(,op-function))

			   ;; org subtree
			   (ryo-modal-key ,(concat prefix-key " " "i u") 'sk/mark-inside-subtree
							  :then '(,op-function))
			   (ryo-modal-key ,(concat prefix-key " " "a u") 'org-mark-subtree
							  :then '(,op-function))

			   ;; latex environment
			   (ryo-modal-key ,(concat prefix-key " " "i e") 'er/mark-LaTeX-inside-environment
							  :then '(,op-function))
			   (ryo-modal-key ,(concat prefix-key " " "a e") 'LaTeX-mark-environment
							  :then '(,op-function))

			   ;; method
			   (ryo-modal-key ,(concat prefix-key " " "i r") 'er/mark-method-call
							  :then '(,op-function))
			   (ryo-modal-key ,(concat prefix-key " " "a r") 'er/mark-method-call
							  :then '(,op-function))

			   ;; ruby block
			   (ryo-modal-key ,(concat prefix-key " " "i d") 'sk/mark-inside-ruby-block
							  :then '(,op-function))
			   (ryo-modal-key ,(concat prefix-key " " "a d") 'er/mark-method-call
							  :then '(,op-function))

			   ;; python string
			   (ryo-modal-key ,(concat prefix-key " " "i g") 'er/mark-inside-python-string
							  :then '(,op-function))
			   (ryo-modal-key ,(concat prefix-key " " "a g") 'er/mark-outside-python-string
							  :then '(,op-function))

			   ;; python block
			   (ryo-modal-key ,(concat prefix-key " " "i m") 'sk/mark-inside-python-block
							  :then '(,op-function))
			   (ryo-modal-key ,(concat prefix-key " " "a m") 'er/mark-outer-python-block
							  :then '(,op-function))

			   ;; python statement
			   (ryo-modal-key ,(concat prefix-key " " "i n") 'er/mark-python-statement
							  :then '(,op-function))
			   (ryo-modal-key ,(concat prefix-key " " "a n") 'er/mark-python-block-and-decorator
							  :then '(,op-function))

			   ;; latex math
			   (ryo-modal-key ,(concat prefix-key " " "i $" )'er/mark-LaTeX-math
							  :then '(,op-function))
			   (ryo-modal-key ,(concat prefix-key " " "a $" )'sk/mark-inside-LaTeX-math
							  :then '(,op-function))

			   ;; latex section
			   (ryo-modal-key ,(concat prefix-key " " "i x") 'LaTeX-mark-section
							  :then '(,op-function))
			   (ryo-modal-key ,(concat prefix-key " " "a x") 'LaTeX-mark-section
							  :then '(,op-function))

			   ;; avy jumps
			   (ryo-modal-key ,(concat prefix-key " " ";") 'set-mark-command
							  :then '(avy-goto-line
									  ,op-function))
			   (ryo-modal-key ,(concat prefix-key " " "'") 'set-mark-command
							  :then '(avy-resume
									  ,op-function) :mode ',lang-mode)
			   (ryo-modal-key ,(concat prefix-key " " "f") 'set-mark-command
							  :then '(avy-goto-char-in-line
									  ,op-function))
			   (ryo-modal-key ,(concat prefix-key " " "F") 'set-mark-command
							  :then '(avy-goto-char-2
									  ,op-function))
			   (ryo-modal-key ,(concat prefix-key " " "t") 'set-mark-command
							  :then '(avy-goto-char-2-below
									  ,op-function))
			   (ryo-modal-key ,(concat prefix-key " " "T") 'set-mark-command
							  :then '(avy-goto-char-2-above
									  ,op-function))

			   ;; character
			   (ryo-modal-key ,(concat prefix-key " " "h") 'set-mark-command
							  :then '(backward-char
									  ,op-function))
			   (ryo-modal-key ,(concat prefix-key " " "l") 'set-mark-command
							  :then '(forward-char
									  ,op-function))

			   ;; line
			   (ryo-modal-key ,(concat prefix-key " " "j") 'set-mark-command
							  :then '(next-line
									  move-end-of-line
									  exchange-point-and-mark
									  move-beginning-of-line
									  ,op-function))
			   (ryo-modal-key ,(concat prefix-key " " "k") 'set-mark-command
							  :then '(move-end-of-line
									  exchange-point-and-mark
									  previous-line
									  move-beginning-of-line
									  ,op-function))

			   ;; within line
			   (ryo-modal-key ,(concat prefix-key " " "0" )'set-mark-command
							  :then '(sk/smarter-move-beginning-of-line
									  ,op-function))
			   (ryo-modal-key ,(concat prefix-key " " "$") 'set-mark-command
							  :then '(move-end-of-line
									  ,op-function))

			   ;; within buffer
			   (ryo-modal-key ,(concat prefix-key " " "g g") 'set-mark-command
							  :then '(beginning-of-buffer
									  ,op-function))
			   (ryo-modal-key ,(concat prefix-key " " "G" )'set-mark-command
							  :then '(end-of-buffer
									  ,op-function))

			   ;; within para
			   (ryo-modal-key ,(concat prefix-key " " "{") 'set-mark-command
							  :then '(backward-paragraph
									  ,op-function))
			   (ryo-modal-key ,(concat prefix-key " " "}") 'set-mark-command
							  :then '(forward-paragraph
									  ,op-function))

			   ;; within sentence
			   (ryo-modal-key ,(concat prefix-key " " "(") 'set-mark-command
							  :then '(backward-sentence
									  ,op-function
									  sk/remove-mark) :mode ',lang-mode)
			   (ryo-modal-key ,(concat prefix-key " " ")")  'set-mark-command
							  :then '(forward-sentence
									  ,op-function))

			   ;; within words
			   (ryo-modal-key ,(concat prefix-key " " "e" )'set-mark-command
							  :then '(forward-word
									  ,op-function))
			   (ryo-modal-key ,(concat prefix-key " " "b" )'set-mark-command
							  :then '(backward-word
									  ,op-function))))

		;; if marks are not automatically removed by the op-function
		(progn
		  
		  `(defun ,(sk/concat 'sk/ryo- op-name '-bindings) ()
			 (interactive)

			 ;; operator/textobject sending
			 ;; region
			 (ryo-modal-key ,(concat prefix-key " " region-key) ',op-function
							:then '(sk/remove-mark))

			 ;; entire buffer
			 (ryo-modal-key ,(concat prefix-key " " "i a") 'mark-whole-buffer
							:then '(,op-function
									sk/remove-mark))
			 (ryo-modal-key ,(concat prefix-key " " "a a") 'mark-whole-buffer
							:then '(,op-function
									sk/remove-mark))

			 ;; word
			 (ryo-modal-key ,(concat prefix-key " " "i w") 'er/mark-word
							:then '(,op-function
									sk/remove-mark))
			 (ryo-modal-key ,(concat prefix-key " " "a w") 'sk/mark-around-word
							:then '(,op-function
									sk/remove-mark))

			 ;; markdown code block
			 (ryo-modal-key ,(concat prefix-key " " "i k") 'sk/mark-inside-markdown-code-block
							:then '(,op-function
									sk/remove-mark))
			 (ryo-modal-key ,(concat prefix-key " " "a k") 'sk/mark-around-markdown-code-block
							:then '(,op-function
									sk/remove-mark))

			 ;; para
			 (ryo-modal-key ,(concat prefix-key " " "i p") 'er/mark-text-paragraph
							:then '(,op-function
									sk/remove-mark))
			 (ryo-modal-key ,(concat prefix-key " " "a p") 'mark-paragraph
							:then '(,op-function
									sk/remove-mark))

			 ;; indented region
			 (ryo-modal-key ,(concat prefix-key " " "i i") 'indent-tools-select
							:then '(,op-function
									sk/remove-mark))
			 (ryo-modal-key ,(concat prefix-key " " "a i") 'sk/select-indent-tree
							:then '(,op-function
									sk/remove-mark))

			 ;; sentence
			 (ryo-modal-key ,(concat prefix-key " " "i s") 'er/mark-text-sentence
							:then '(,op-function
									sk/remove-mark))
			 (ryo-modal-key ,(concat prefix-key " " "a s") 'er/mark-text-sentence
							:then '(,op-function
									sk/remove-mark))

			 ;; line
			 (ryo-modal-key ,(concat prefix-key " " "i l") 'sk/select-inside-line
							:then '(,op-function
									sk/remove-mark))
			 (ryo-modal-key ,(concat prefix-key " " "a l") 'sk/select-around-line
							:then '(,op-function
									sk/remove-mark))

			 ;; symbol
			 (ryo-modal-key ,(concat prefix-key " " "i y") 'er/mark-symbol
							:then '(,op-function
									sk/remove-mark))
			 (ryo-modal-key ,(concat prefix-key " " "a y") 'sk/mark-around-symbol
							:then '(,op-function
									sk/remove-mark))

			 ;; comment
			 (ryo-modal-key ,(concat prefix-key " " "i c") 'er/mark-comment
							:then '(,op-function
									sk/remove-mark))
			 (ryo-modal-key ,(concat prefix-key " " "a c") 'er/mark-comment
							:then '(,op-function
									sk/remove-mark))

			 ;; function
			 (ryo-modal-key ,(concat prefix-key " " "i f") 'er/mark-defun
							:then '(,op-function
									sk/remove-mark))
			 (ryo-modal-key ,(concat prefix-key " " "a f") 'er/mark-defun
							:then '(,op-function
									sk/remove-mark))

			 ;; diff
			 (ryo-modal-key ,(concat prefix-key " " "i h") 'diff-hl-mark-hunk
							:then '(,op-function
									sk/remove-mark))
			 (ryo-modal-key ,(concat prefix-key " " "a h") 'diff-hl-mark-hunk
							:then '(,op-function
									sk/remove-mark))

			 ;; quotes
			 (ryo-modal-key ,(concat prefix-key " " "i q") 'er/mark-inside-quotes
							:then '(,op-function
									sk/remove-mark))
			 (ryo-modal-key ,(concat prefix-key " " "a q") 'er/mark-outside-quotes
							:then '(,op-function
									sk/remove-mark))

			 ;; pairs (), [], {}
			 (ryo-modal-key ,(concat prefix-key " " "i b") 'er/mark-inside-pairs
							:then '(,op-function
									sk/remove-mark))
			 (ryo-modal-key ,(concat prefix-key " " "a b") 'er/mark-outside-pairs
							:then '(,op-function
									sk/remove-mark))

			 ;; org code block
			 (ryo-modal-key ,(concat prefix-key " " "i o") 'sk/mark-inside-org-code
							:then '(,op-function
									sk/remove-mark))
			 (ryo-modal-key ,(concat prefix-key " " "a o") 'er/mark-org-code-block
							:then '(,op-function
									sk/remove-mark))

			 ;; org subtree
			 (ryo-modal-key ,(concat prefix-key " " "i u") 'sk/mark-inside-subtree
							:then '(,op-function
									sk/remove-mark))
			 (ryo-modal-key ,(concat prefix-key " " "a u") 'org-mark-subtree
							:then '(,op-function
									sk/remove-mark))

			 ;; latex environment
			 (ryo-modal-key ,(concat prefix-key " " "i e") 'er/mark-LaTeX-inside-environment
							:then '(,op-function
									sk/remove-mark))
			 (ryo-modal-key ,(concat prefix-key " " "a e") 'LaTeX-mark-environment
							:then '(,op-function
									sk/remove-mark))

			 ;; method
			 (ryo-modal-key ,(concat prefix-key " " "i r") 'er/mark-method-call
							:then '(,op-function
									sk/remove-mark))
			 (ryo-modal-key ,(concat prefix-key " " "a r") 'er/mark-method-call
							:then '(,op-function
									sk/remove-mark))

			 ;; ruby block
			 (ryo-modal-key ,(concat prefix-key " " "i d") 'sk/mark-inside-ruby-block
							:then '(,op-function
									sk/remove-mark))
			 (ryo-modal-key ,(concat prefix-key " " "a d") 'er/mark-method-call
							:then '(,op-function
									sk/remove-mark))

			 ;; python string
			 (ryo-modal-key ,(concat prefix-key " " "i g") 'er/mark-inside-python-string
							:then '(,op-function
									sk/remove-mark))
			 (ryo-modal-key ,(concat prefix-key " " "a g") 'er/mark-outside-python-string
							:then '(,op-function
									sk/remove-mark))

			 ;; python block
			 (ryo-modal-key ,(concat prefix-key " " "i m") 'sk/mark-inside-python-block
							:then '(,op-function
									sk/remove-mark))
			 (ryo-modal-key ,(concat prefix-key " " "a m") 'er/mark-outer-python-block
							:then '(,op-function
									sk/remove-mark))

			 ;; python statement
			 (ryo-modal-key ,(concat prefix-key " " "i n") 'er/mark-python-statement
							:then '(,op-function
									sk/remove-mark))
			 (ryo-modal-key ,(concat prefix-key " " "a n") 'er/mark-python-block-and-decorator
							:then '(,op-function
									sk/remove-mark))

			 ;; latex math
			 (ryo-modal-key ,(concat prefix-key " " "i $" )'er/mark-LaTeX-math
							:then '(,op-function
									sk/remove-mark))
			 (ryo-modal-key ,(concat prefix-key " " "a $" )'sk/mark-inside-LaTeX-math
							:then '(,op-function
									sk/remove-mark))

			 ;; latex section
			 (ryo-modal-key ,(concat prefix-key " " "i x") 'LaTeX-mark-section
							:then '(,op-function
									sk/remove-mark))
			 (ryo-modal-key ,(concat prefix-key " " "a x") 'LaTeX-mark-section
							:then '(,op-function
									sk/remove-mark))

			 ;; avy jumps
			 (ryo-modal-key ,(concat prefix-key " " ";") 'set-mark-command
							:then '(avy-goto-line
									,op-function))
			 (ryo-modal-key ,(concat prefix-key " " "'") 'set-mark-command
							:then '(avy-resume
									,op-function
									sk/remove-mark))
			 (ryo-modal-key ,(concat prefix-key " " "f") 'set-mark-command
							:then '(avy-goto-char-in-line
									,op-function
									sk/remove-mark))
			 (ryo-modal-key ,(concat prefix-key " " "F") 'set-mark-command
							:then '(avy-goto-char-2
									,op-function
									sk/remove-mark))
			 (ryo-modal-key ,(concat prefix-key " " "t") 'set-mark-command
							:then '(avy-goto-char-2-below
									,op-function
									sk/remove-mark))
			 (ryo-modal-key ,(concat prefix-key " " "T") 'set-mark-command
							:then '(avy-goto-char-2-above
									,op-function
									sk/remove-mark))

			 ;; character
			 (ryo-modal-key ,(concat prefix-key " " "h") 'set-mark-command
							:then '(backward-char
									,op-function
									sk/remove-mark))
			 (ryo-modal-key ,(concat prefix-key " " "l") 'set-mark-command
							:then '(forward-char
									,op-function
									sk/remove-mark))

			 ;; line
			 (ryo-modal-key ,(concat prefix-key " " "j") 'set-mark-command
							:then '(next-line
									move-end-of-line
									exchange-point-and-mark
									move-beginning-of-line
									,op-function
									sk/remove-mark))
			 (ryo-modal-key ,(concat prefix-key " " "k") 'set-mark-command
							:then '(move-end-of-line
									exchange-point-and-mark
									previous-line
									move-beginning-of-line
									,op-function
									sk/remove-mark))

			 ;; within line
			 (ryo-modal-key ,(concat prefix-key " " "0" )'set-mark-command
							:then '(sk/smarter-move-beginning-of-line
									,op-function
									sk/remove-mark))
			 (ryo-modal-key ,(concat prefix-key " " "$") 'set-mark-command
							:then '(move-end-of-line
									,op-function
									sk/remove-mark))

			 ;; within buffer
			 (ryo-modal-key ,(concat prefix-key " " "g g") 'set-mark-command
							:then '(beginning-of-buffer
									,op-function
									sk/remove-mark))
			 (ryo-modal-key ,(concat prefix-key " " "G" )'set-mark-command
							:then '(end-of-buffer
									,op-function
									sk/remove-mark))

			 ;; within para
			 (ryo-modal-key ,(concat prefix-key " " "{") 'set-mark-command
							:then '(backward-paragraph
									,op-function
									sk/remove-mark))
			 (ryo-modal-key ,(concat prefix-key " " "}") 'set-mark-command
							:then '(forward-paragraph
									,op-function
									sk/remove-mark))

			 ;; within sentence
			 (ryo-modal-key ,(concat prefix-key " " "(") 'set-mark-command
							:then '(backward-sentence
									,op-function
									sk/remove-mark))
			 (ryo-modal-key ,(concat prefix-key " " ")")  'set-mark-command
							:then '(forward-sentence
									,op-function
									sk/remove-mark))

			 ;; within words
			 (ryo-modal-key ,(concat prefix-key " " "e" )'set-mark-command
							:then '(forward-word
									,op-function
									sk/remove-mark))
			 (ryo-modal-key ,(concat prefix-key " " "b" )'set-mark-command
							:then '(backward-word
									,op-function
									sk/remove-mark)))))

	;; local bindings and marks are removed automatically
	(if (not remove-mark)

		(progn

		  `(defun ,(sk/concat 'sk/ryo- op-name '-bindings) ()
			 (interactive)

			 ;; operator/textobject sending
			 ;; region
			 (ryo-modal-key ,(concat prefix-key " " region-key) ',op-function
							:mode ',lang-mode)

			 ;; entire buffer
			 (ryo-modal-key ,(concat prefix-key " " "i a") 'mark-whole-buffer
							:then '(,op-function) :mode ',lang-mode)
			 (ryo-modal-key ,(concat prefix-key " " "a a") 'mark-whole-buffer
							:then '(,op-function) :mode ',lang-mode)

			 ;; word
			 (ryo-modal-key ,(concat prefix-key " " "i w") 'er/mark-word
							:then '(,op-function) :mode ',lang-mode)
			 (ryo-modal-key ,(concat prefix-key " " "a w") 'sk/mark-around-word
							:then '(,op-function) :mode ',lang-mode)

			 ;; markdown code block
			 (ryo-modal-key ,(concat prefix-key " " "i k") 'sk/mark-inside-markdown-code-block
							:then '(,op-function) :mode ',lang-mode)
			 (ryo-modal-key ,(concat prefix-key " " "a k") 'sk/mark-around-markdown-code-block
							:then '(,op-function) :mode ',lang-mode)

			 ;; para
			 (ryo-modal-key ,(concat prefix-key " " "i p") 'er/mark-text-paragraph
							:then '(,op-function) :mode ',lang-mode)
			 (ryo-modal-key ,(concat prefix-key " " "a p") 'mark-paragraph
							:then '(,op-function) :mode ',lang-mode)

			 ;; indented region
			 (ryo-modal-key ,(concat prefix-key " " "i i") 'indent-tools-select
							:then '(,op-function) :mode ',lang-mode)
			 (ryo-modal-key ,(concat prefix-key " " "a i") 'sk/select-indent-tree
							:then '(,op-function) :mode ',lang-mode)

			 ;; sentence
			 (ryo-modal-key ,(concat prefix-key " " "i s") 'er/mark-text-sentence
							:then '(,op-function) :mode ',lang-mode)
			 (ryo-modal-key ,(concat prefix-key " " "a s") 'er/mark-text-sentence
							:then '(,op-function) :mode ',lang-mode)

			 ;; line
			 (ryo-modal-key ,(concat prefix-key " " "i l") 'sk/select-inside-line
							:then '(,op-function) :mode ',lang-mode)
			 (ryo-modal-key ,(concat prefix-key " " "a l") 'sk/select-around-line
							:then '(,op-function) :mode ',lang-mode)

			 ;; symbol
			 (ryo-modal-key ,(concat prefix-key " " "i y") 'er/mark-symbol
							:then '(,op-function) :mode ',lang-mode)
			 (ryo-modal-key ,(concat prefix-key " " "a y") 'sk/mark-around-symbol
							:then '(,op-function) :mode ',lang-mode)

			 ;; comment
			 (ryo-modal-key ,(concat prefix-key " " "i c") 'er/mark-comment
							:then '(,op-function) :mode ',lang-mode)
			 (ryo-modal-key ,(concat prefix-key " " "a c") 'er/mark-comment
							:then '(,op-function) :mode ',lang-mode)

			 ;; function
			 (ryo-modal-key ,(concat prefix-key " " "i f") 'er/mark-defun
							:then '(,op-function) :mode ',lang-mode)
			 (ryo-modal-key ,(concat prefix-key " " "a f") 'er/mark-defun
							:then '(,op-function) :mode ',lang-mode)

			 ;; diff
			 (ryo-modal-key ,(concat prefix-key " " "i h") 'diff-hl-mark-hunk
							:then '(,op-function) :mode ',lang-mode)
			 (ryo-modal-key ,(concat prefix-key " " "a h") 'diff-hl-mark-hunk
							:then '(,op-function) :mode ',lang-mode)

			 ;; quotes
			 (ryo-modal-key ,(concat prefix-key " " "i q") 'er/mark-inside-quotes
							:then '(,op-function) :mode ',lang-mode)
			 (ryo-modal-key ,(concat prefix-key " " "a q") 'er/mark-outside-quotes
							:then '(,op-function) :mode ',lang-mode)

			 ;; pairs (), [], {}
			 (ryo-modal-key ,(concat prefix-key " " "i b") 'er/mark-inside-pairs
							:then '(,op-function) :mode ',lang-mode)
			 (ryo-modal-key ,(concat prefix-key " " "a b") 'er/mark-outside-pairs
							:then '(,op-function) :mode ',lang-mode)

			 ;; org code block
			 (ryo-modal-key ,(concat prefix-key " " "i o") 'sk/mark-inside-org-code
							:then '(,op-function) :mode ',lang-mode)
			 (ryo-modal-key ,(concat prefix-key " " "a o") 'er/mark-org-code-block
							:then '(,op-function) :mode ',lang-mode)

			 ;; org subtree
			 (ryo-modal-key ,(concat prefix-key " " "i u") 'sk/mark-inside-subtree
							:then '(,op-function) :mode ',lang-mode)
			 (ryo-modal-key ,(concat prefix-key " " "a u") 'org-mark-subtree
							:then '(,op-function) :mode ',lang-mode)

			 ;; latex environment
			 (ryo-modal-key ,(concat prefix-key " " "i e") 'er/mark-LaTeX-inside-environment
							:then '(,op-function) :mode ',lang-mode)
			 (ryo-modal-key ,(concat prefix-key " " "a e") 'LaTeX-mark-environment
							:then '(,op-function) :mode ',lang-mode)

			 ;; method
			 (ryo-modal-key ,(concat prefix-key " " "i r") 'er/mark-method-call
							:then '(,op-function) :mode ',lang-mode)
			 (ryo-modal-key ,(concat prefix-key " " "a r") 'er/mark-method-call
							:then '(,op-function) :mode ',lang-mode)

			 ;; ruby block
			 (ryo-modal-key ,(concat prefix-key " " "i d") 'sk/mark-inside-ruby-block
							:then '(,op-function) :mode ',lang-mode)
			 (ryo-modal-key ,(concat prefix-key " " "a d") 'er/mark-method-call
							:then '(,op-function) :mode ',lang-mode)

			 ;; python string
			 (ryo-modal-key ,(concat prefix-key " " "i g") 'er/mark-inside-python-string
							:then '(,op-function) :mode ',lang-mode)
			 (ryo-modal-key ,(concat prefix-key " " "a g") 'er/mark-outside-python-string
							:then '(,op-function) :mode ',lang-mode)

			 ;; python block
			 (ryo-modal-key ,(concat prefix-key " " "i m") 'sk/mark-inside-python-block
							:then '(,op-function) :mode ',lang-mode)
			 (ryo-modal-key ,(concat prefix-key " " "a m") 'er/mark-outer-python-block
							:then '(,op-function) :mode ',lang-mode)

			 ;; python statement
			 (ryo-modal-key ,(concat prefix-key " " "i n") 'er/mark-python-statement
							:then '(,op-function) :mode ',lang-mode)
			 (ryo-modal-key ,(concat prefix-key " " "a n") 'er/mark-python-block-and-decorator
							:then '(,op-function) :mode ',lang-mode)

			 ;; latex math
			 (ryo-modal-key ,(concat prefix-key " " "i $" )'er/mark-LaTeX-math
							:then '(,op-function) :mode ',lang-mode)
			 (ryo-modal-key ,(concat prefix-key " " "a $" )'sk/mark-inside-LaTeX-math
							:then '(,op-function) :mode ',lang-mode)

			 ;; latex section
			 (ryo-modal-key ,(concat prefix-key " " "i x") 'LaTeX-mark-section
							:then '(,op-function) :mode ',lang-mode)
			 (ryo-modal-key ,(concat prefix-key " " "a x") 'LaTeX-mark-section
							:then '(,op-function) :mode ',lang-mode)

			 ;; avy jumps
			 (ryo-modal-key ,(concat prefix-key " " ";") 'set-mark-command
							:then '(avy-goto-line
									,op-function) :mode ',lang-mode)
			 (ryo-modal-key ,(concat prefix-key " " "'") 'set-mark-command
							:then '(avy-resume
									,op-function) :mode ',lang-mode)
			 (ryo-modal-key ,(concat prefix-key " " "f") 'set-mark-command
							:then '(avy-goto-char-in-line
									,op-function) :mode ',lang-mode)
			 (ryo-modal-key ,(concat prefix-key " " "F") 'set-mark-command
							:then '(avy-goto-char-2
									,op-function) :mode ',lang-mode)
			 (ryo-modal-key ,(concat prefix-key " " "t") 'set-mark-command
							:then '(avy-goto-char-2-below
									,op-function) :mode ',lang-mode)
			 (ryo-modal-key ,(concat prefix-key " " "T") 'set-mark-command
							:then '(avy-goto-char-2-above
									,op-function) :mode ',lang-mode)

			 ;; character
			 (ryo-modal-key ,(concat prefix-key " " "h") 'set-mark-command
							:then '(backward-char
									,op-function) :mode ',lang-mode)
			 (ryo-modal-key ,(concat prefix-key " " "l") 'set-mark-command
							:then '(forward-char
									,op-function) :mode ',lang-mode)

			 ;; line
			 (ryo-modal-key ,(concat prefix-key " " "j") 'set-mark-command
							:then '(next-line
									move-end-of-line
									exchange-point-and-mark
									move-beginning-of-line
									,op-function) :mode ',lang-mode)
			 (ryo-modal-key ,(concat prefix-key " " "k") 'set-mark-command
							:then '(move-end-of-line
									exchange-point-and-mark
									previous-line
									move-beginning-of-line
									,op-function) :mode ',lang-mode)

			 ;; within line
			 (ryo-modal-key ,(concat prefix-key " " "0" )'set-mark-command
							:then '(sk/smarter-move-beginning-of-line
									,op-function) :mode ',lang-mode)
			 (ryo-modal-key ,(concat prefix-key " " "$") 'set-mark-command
							:then '(move-end-of-line
									,op-function) :mode ',lang-mode)

			 ;; within buffer
			 (ryo-modal-key ,(concat prefix-key " " "g g") 'set-mark-command
							:then '(beginning-of-buffer
									,op-function) :mode ',lang-mode)
			 (ryo-modal-key ,(concat prefix-key " " "G" )'set-mark-command
							:then '(end-of-buffer
									,op-function) :mode ',lang-mode)

			 ;; within para
			 (ryo-modal-key ,(concat prefix-key " " "{") 'set-mark-command
							:then '(backward-paragraph
									,op-function) :mode ',lang-mode)
			 (ryo-modal-key ,(concat prefix-key " " "}") 'set-mark-command
							:then '(forward-paragraph
									,op-function) :mode ',lang-mode)

			 ;; within sentence
			 (ryo-modal-key ,(concat prefix-key " " "(") 'set-mark-command
							:then '(backward-sentence
									,op-function) :mode ',lang-mode)
			 (ryo-modal-key ,(concat prefix-key " " ")")  'set-mark-command
							:then '(forward-sentence
									,op-function) :mode ',lang-mode)

			 ;; within words
			 (ryo-modal-key ,(concat prefix-key " " "e" )'set-mark-command
							:then '(forward-word
									,op-function) :mode ',lang-mode)
			 (ryo-modal-key ,(concat prefix-key " " "b" )'set-mark-command
							:then '(backward-word
									,op-function) :mode ',lang-mode)))

	  ;; local bindings and marks are not removed automatically
	  (progn

		`(defun ,(sk/concat 'sk/ryo- op-name '-bindings) ()
		   (interactive)

		   ;; operator/textobject sending
		   ;; region
		   (ryo-modal-key ,(concat prefix-key " " region-key) ',op-function
						  :then '(sk/remove-mark) :mode ',lang-mode)

		   ;; entire buffer
		   (ryo-modal-key ,(concat prefix-key " " "i a") 'mark-whole-buffer
						  :then '(,op-function
								  sk/remove-mark) :mode ',lang-mode)
		   (ryo-modal-key ,(concat prefix-key " " "a a") 'mark-whole-buffer
						  :then '(,op-function
								  sk/remove-mark) :mode ',lang-mode)

		   ;; word
		   (ryo-modal-key ,(concat prefix-key " " "i w") 'er/mark-word
						  :then '(,op-function
								  sk/remove-mark) :mode ',lang-mode)
		   (ryo-modal-key ,(concat prefix-key " " "a w") 'sk/mark-around-word
						  :then '(,op-function
								  sk/remove-mark) :mode ',lang-mode)

		   ;; markdown code block
		   (ryo-modal-key ,(concat prefix-key " " "i k") 'sk/mark-inside-markdown-code-block
						  :then '(,op-function
								  sk/remove-mark) :mode ',lang-mode)
		   (ryo-modal-key ,(concat prefix-key " " "a k") 'sk/mark-around-markdown-code-block
						  :then '(,op-function
								  sk/remove-mark) :mode ',lang-mode)

		   ;; para
		   (ryo-modal-key ,(concat prefix-key " " "i p") 'er/mark-text-paragraph
						  :then '(,op-function
								  sk/remove-mark) :mode ',lang-mode)
		   (ryo-modal-key ,(concat prefix-key " " "a p") 'mark-paragraph
						  :then '(,op-function
								  sk/remove-mark) :mode ',lang-mode)

		   ;; indented region
		   (ryo-modal-key ,(concat prefix-key " " "i i") 'indent-tools-select
						  :then '(,op-function
								  sk/remove-mark) :mode ',lang-mode)
		   (ryo-modal-key ,(concat prefix-key " " "a i") 'sk/select-indent-tree
						  :then '(,op-function
								  sk/remove-mark) :mode ',lang-mode)

		   ;; sentence
		   (ryo-modal-key ,(concat prefix-key " " "i s") 'er/mark-text-sentence
						  :then '(,op-function
								  sk/remove-mark) :mode ',lang-mode)
		   (ryo-modal-key ,(concat prefix-key " " "a s") 'er/mark-text-sentence
						  :then '(,op-function
								  sk/remove-mark) :mode ',lang-mode)

		   ;; line
		   (ryo-modal-key ,(concat prefix-key " " "i l") 'sk/select-inside-line
						  :then '(,op-function
								  sk/remove-mark) :mode ',lang-mode)
		   (ryo-modal-key ,(concat prefix-key " " "a l") 'sk/select-around-line
						  :then '(,op-function
								  sk/remove-mark) :mode ',lang-mode)

		   ;; symbol
		   (ryo-modal-key ,(concat prefix-key " " "i y") 'er/mark-symbol
						  :then '(,op-function
								  sk/remove-mark) :mode ',lang-mode)
		   (ryo-modal-key ,(concat prefix-key " " "a y") 'sk/mark-around-symbol
						  :then '(,op-function
								  sk/remove-mark) :mode ',lang-mode)

		   ;; comment
		   (ryo-modal-key ,(concat prefix-key " " "i c") 'er/mark-comment
						  :then '(,op-function
								  sk/remove-mark) :mode ',lang-mode)
		   (ryo-modal-key ,(concat prefix-key " " "a c") 'er/mark-comment
						  :then '(,op-function
								  sk/remove-mark) :mode ',lang-mode)

		   ;; function
		   (ryo-modal-key ,(concat prefix-key " " "i f") 'er/mark-defun
						  :then '(,op-function
								  sk/remove-mark) :mode ',lang-mode)
		   (ryo-modal-key ,(concat prefix-key " " "a f") 'er/mark-defun
						  :then '(,op-function
								  sk/remove-mark) :mode ',lang-mode)

		   ;; diff
		   (ryo-modal-key ,(concat prefix-key " " "i h") 'diff-hl-mark-hunk
						  :then '(,op-function
								  sk/remove-mark) :mode ',lang-mode)
		   (ryo-modal-key ,(concat prefix-key " " "a h") 'diff-hl-mark-hunk
						  :then '(,op-function
								  sk/remove-mark) :mode ',lang-mode)

		   ;; quotes
		   (ryo-modal-key ,(concat prefix-key " " "i q") 'er/mark-inside-quotes
						  :then '(,op-function
								  sk/remove-mark) :mode ',lang-mode)
		   (ryo-modal-key ,(concat prefix-key " " "a q") 'er/mark-outside-quotes
						  :then '(,op-function
								  sk/remove-mark) :mode ',lang-mode)

		   ;; pairs (), [], {}
		   (ryo-modal-key ,(concat prefix-key " " "i b") 'er/mark-inside-pairs
						  :then '(,op-function
								  sk/remove-mark) :mode ',lang-mode)
		   (ryo-modal-key ,(concat prefix-key " " "a b") 'er/mark-outside-pairs
						  :then '(,op-function
								  sk/remove-mark) :mode ',lang-mode)

		   ;; org code block
		   (ryo-modal-key ,(concat prefix-key " " "i o") 'sk/mark-inside-org-code
						  :then '(,op-function
								  sk/remove-mark) :mode ',lang-mode)
		   (ryo-modal-key ,(concat prefix-key " " "a o") 'er/mark-org-code-block
						  :then '(,op-function
								  sk/remove-mark) :mode ',lang-mode)

		   ;; org subtree
		   (ryo-modal-key ,(concat prefix-key " " "i u") 'sk/mark-inside-subtree
						  :then '(,op-function
								  sk/remove-mark) :mode ',lang-mode)
		   (ryo-modal-key ,(concat prefix-key " " "a u") 'org-mark-subtree
						  :then '(,op-function
								  sk/remove-mark) :mode ',lang-mode)

		   ;; latex environment
		   (ryo-modal-key ,(concat prefix-key " " "i e") 'er/mark-LaTeX-inside-environment
						  :then '(,op-function
								  sk/remove-mark) :mode ',lang-mode)
		   (ryo-modal-key ,(concat prefix-key " " "a e") 'LaTeX-mark-environment
						  :then '(,op-function
								  sk/remove-mark) :mode ',lang-mode)

		   ;; method
		   (ryo-modal-key ,(concat prefix-key " " "i r") 'er/mark-method-call
						  :then '(,op-function
								  sk/remove-mark) :mode ',lang-mode)
		   (ryo-modal-key ,(concat prefix-key " " "a r") 'er/mark-method-call
						  :then '(,op-function
								  sk/remove-mark) :mode ',lang-mode)

		   ;; ruby block
		   (ryo-modal-key ,(concat prefix-key " " "i d") 'sk/mark-inside-ruby-block
						  :then '(,op-function
								  sk/remove-mark) :mode ',lang-mode)
		   (ryo-modal-key ,(concat prefix-key " " "a d") 'er/mark-method-call
						  :then '(,op-function
								  sk/remove-mark) :mode ',lang-mode)

		   ;; python string
		   (ryo-modal-key ,(concat prefix-key " " "i g") 'er/mark-inside-python-string
						  :then '(,op-function
								  sk/remove-mark) :mode ',lang-mode)
		   (ryo-modal-key ,(concat prefix-key " " "a g") 'er/mark-outside-python-string
						  :then '(,op-function
								  sk/remove-mark) :mode ',lang-mode)

		   ;; python block
		   (ryo-modal-key ,(concat prefix-key " " "i m") 'sk/mark-inside-python-block
						  :then '(,op-function
								  sk/remove-mark) :mode ',lang-mode)
		   (ryo-modal-key ,(concat prefix-key " " "a m") 'er/mark-outer-python-block
						  :then '(,op-function
								  sk/remove-mark) :mode ',lang-mode)

		   ;; python statement
		   (ryo-modal-key ,(concat prefix-key " " "i n") 'er/mark-python-statement
						  :then '(,op-function
								  sk/remove-mark) :mode ',lang-mode)
		   (ryo-modal-key ,(concat prefix-key " " "a n") 'er/mark-python-block-and-decorator
						  :then '(,op-function
								  sk/remove-mark) :mode ',lang-mode)

		   ;; latex math
		   (ryo-modal-key ,(concat prefix-key " " "i $" )'er/mark-LaTeX-math
						  :then '(,op-function
								  sk/remove-mark) :mode ',lang-mode)
		   (ryo-modal-key ,(concat prefix-key " " "a $" )'sk/mark-inside-LaTeX-math
						  :then '(,op-function
								  sk/remove-mark) :mode ',lang-mode)

		   ;; latex section
		   (ryo-modal-key ,(concat prefix-key " " "i x") 'LaTeX-mark-section
						  :then '(,op-function
								  sk/remove-mark) :mode ',lang-mode)
		   (ryo-modal-key ,(concat prefix-key " " "a x") 'LaTeX-mark-section
						  :then '(,op-function
								  sk/remove-mark) :mode ',lang-mode)

		   ;; avy jumps
		   (ryo-modal-key ,(concat prefix-key " " ";") 'set-mark-command
						  :then '(avy-goto-line
								  ,op-function) :mode ',lang-mode)
		   (ryo-modal-key ,(concat prefix-key " " "'") 'set-mark-command
						  :then '(avy-resume
								  ,op-function
								  sk/remove-mark) :mode ',lang-mode)
		   (ryo-modal-key ,(concat prefix-key " " "f") 'set-mark-command
						  :then '(avy-goto-char-in-line
								  ,op-function
								  sk/remove-mark) :mode ',lang-mode)
		   (ryo-modal-key ,(concat prefix-key " " "F") 'set-mark-command
						  :then '(avy-goto-char-2
								  ,op-function
								  sk/remove-mark) :mode ',lang-mode)
		   (ryo-modal-key ,(concat prefix-key " " "t") 'set-mark-command
						  :then '(avy-goto-char-2-below
								  ,op-function
								  sk/remove-mark) :mode ',lang-mode)
		   (ryo-modal-key ,(concat prefix-key " " "T") 'set-mark-command
						  :then '(avy-goto-char-2-above
								  ,op-function
								  sk/remove-mark) :mode ',lang-mode)

		   ;; character
		   (ryo-modal-key ,(concat prefix-key " " "h") 'set-mark-command
						  :then '(backward-char
								  ,op-function
								  sk/remove-mark) :mode ',lang-mode)
		   (ryo-modal-key ,(concat prefix-key " " "l") 'set-mark-command
						  :then '(forward-char
								  ,op-function
								  sk/remove-mark) :mode ',lang-mode)

		   ;; line
		   (ryo-modal-key ,(concat prefix-key " " "j") 'set-mark-command
						  :then '(next-line
								  move-end-of-line
								  exchange-point-and-mark
								  move-beginning-of-line
								  ,op-function
								  sk/remove-mark) :mode ',lang-mode)
		   (ryo-modal-key ,(concat prefix-key " " "k") 'set-mark-command
						  :then '(move-end-of-line
								  exchange-point-and-mark
								  previous-line
								  move-beginning-of-line
								  ,op-function
								  sk/remove-mark) :mode ',lang-mode)

		   ;; within line
		   (ryo-modal-key ,(concat prefix-key " " "0" )'set-mark-command
						  :then '(sk/smarter-move-beginning-of-line
								  ,op-function
								  sk/remove-mark) :mode ',lang-mode)
		   (ryo-modal-key ,(concat prefix-key " " "$") 'set-mark-command
						  :then '(move-end-of-line
								  ,op-function
								  sk/remove-mark) :mode ',lang-mode)

		   ;; within buffer
		   (ryo-modal-key ,(concat prefix-key " " "g g") 'set-mark-command
						  :then '(beginning-of-buffer
								  ,op-function
								  sk/remove-mark) :mode ',lang-mode)
		   (ryo-modal-key ,(concat prefix-key " " "G" )'set-mark-command
						  :then '(end-of-buffer
								  ,op-function
								  sk/remove-mark) :mode ',lang-mode)

		   ;; within para
		   (ryo-modal-key ,(concat prefix-key " " "{") 'set-mark-command
						  :then '(backward-paragraph
								  ,op-function
								  sk/remove-mark) :mode ',lang-mode)
		   (ryo-modal-key ,(concat prefix-key " " "}") 'set-mark-command
						  :then '(forward-paragraph
								  ,op-function
								  sk/remove-mark) :mode ',lang-mode)

		   ;; within sentence
		   (ryo-modal-key ,(concat prefix-key " " "(") 'set-mark-command
						  :then '(backward-sentence
								  ,op-function
								  sk/remove-mark) :mode ',lang-mode)
		   (ryo-modal-key ,(concat prefix-key " " ")")  'set-mark-command
						  :then '(forward-sentence
								  ,op-function
								  sk/remove-mark) :mode ',lang-mode)

		   ;; within words
		   (ryo-modal-key ,(concat prefix-key " " "e" )'set-mark-command
						  :then '(forward-word
								  ,op-function
								  sk/remove-mark) :mode ',lang-mode)
		   (ryo-modal-key ,(concat prefix-key " " "b" )'set-mark-command
						  :then '(backward-word
								  ,op-function
								  sk/remove-mark) :mode ',lang-mode))))))

;; provide this configuration
(provide 'sk-macros)
