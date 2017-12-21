;; macro to concatenate strings
(defun sk/concat (&rest syms)
  "Concatenate symbols together to create a single symbol."
  (intern (apply #'concatenate 'string (mapcar #'symbol-name syms))))

;; sends a query to the associated REPL of the form "query('<text-obj>')"
(defmacro sk/repl-paren-string-query (lang sending-function repl-query
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
						(sk/string-to-send (concat ,(symbol-name repl-query) "('"
												   (buffer-substring
													(region-beginning) (region-end))
												   "')\n")))
				   (,switching-function)
				   (delete-other-windows)
				   (comint-send-string (get-buffer-process (current-buffer)) sk/string-to-send)
				   (split-window-sensibly))

			   ;; region not selected and comint
			   (let* ((sk/buffer-name (buffer-name))
					  (sk/string-to-send (concat ,(symbol-name repl-query) "('"
												 (thing-at-point 'symbol) "')\n")))
				 (,switching-function)
				 (delete-other-windows)
				 (comint-send-string (get-buffer-process (current-buffer)) sk/string-to-send)
				 (split-window-sensibly)))

		   ;; region selected and sending-function specified (not comint)
		   (if (region-active-p)
			   (let* ((sk/buffer-name (buffer-name))
					  (sk/string-to-send (concat ,(symbol-name repl-query) "('"
												 (buffer-substring
												  (region-beginning) (region-end))
												 "')")))
				 (,sending-function sk/string-to-send))

			 ;; region not selected and sending-function specified
			 (let* ((sk/buffer-name (buffer-name))
					(sk/string-to-send (concat ,(symbol-name repl-query) "('"
											   (thing-at-point 'symbol) "')")))
			   (,sending-function sk/string-to-send))))

	   ;; If tmux is true, send it to tmux
	   (if (region-active-p)
		   (let* ((sk/buffer-name (buffer-name))
				  (sk/string-to-send (concat ,(symbol-name repl-query) "('"
											 (buffer-substring
											  (region-beginning) (region-end))
											 "')")))
			 (emamux:send-command sk/string-to-send))

		 (let* ((sk/buffer-name (buffer-name))
				(sk/string-to-send (concat ,(symbol-name repl-query) "('"
										   (thing-at-point 'symbol) "')")))
		   (emamux:send-command sk/string-to-send))))

	 ;; jump back to the set bookmark
	 (bookmark-jump "repl")))

;; sends a query to the associated REPL of the form "query(<text-obj>)"
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

;; sends a query to the associated REPL of the form "query <text-obj>"
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

;; sends a query to the associated REPL of the form "<text-obj>.query"
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

;; sends a query to the associated REPL of the form "<text-obj>.query()"
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

;; sends a query to the associated REPL of the form "query"
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

;; sends a query to the associated REPL of the form "'query'"
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

;; sends a query to the associated REPL of the form "db* query"
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

;; provide this configuration
(provide 'sk-repl-macros)
