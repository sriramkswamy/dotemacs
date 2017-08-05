;;;;;;;;;;;;;;;;;;;;;;;;
;; Wrapper functions  ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; variable details
(sk/repl-dot-query python python-shell-send-string shape)
(sk/repl-dot-query python python-shell-send-string ndim)
(sk/repl-dot-query python python-shell-send-string size)
(sk/repl-paren-query python python-shell-send-string len)
(sk/repl-paren-query python python-shell-send-string type)
(sk/repl-dot-paren-query python python-shell-send-string sum)
(sk/repl-dot-paren-query python python-shell-send-string cumsum)
(sk/repl-dot-paren-query python python-shell-send-string mean)

;; environment
(sk/repl-string-query python python-shell-send-string locals "locals()")
(sk/repl-string-query python python-shell-send-string globals "globals()")
(sk/repl-string-query python python-shell-send-string dir "dir()")
(sk/repl-string-query python python-shell-send-string whos "whos()")
(sk/repl-string-query python python-shell-send-string clear "%reset -f")

;; plotting
(defun sk/python-plot ()
  "get the plot of the symbol at point"
  (interactive)
  (let* ((sk/buffer-name (buffer-name))
		 (sk/string-to-send (concat "plt.plot(" (thing-at-point 'symbol) "); plt.show()")))
	(python-shell-send-string sk/string-to-send)))

;; debugging
(defun sk/python-set-breakpoint ()
  "set the breakpoint at the current line"
  (interactive)
  (sk/smarter-move-beginning-of-line 1)
  (insert "import pdb; pdb.set_trace()\n"))

(defun sk/python-unset-breakpoint ()
  "unset the breakpoint at the current line"
  (interactive)
  (previous-line)
  (kill-whole-line))

(sk/repl-string-query python python-shell-send-string dbstep "n")
(sk/repl-string-query python python-shell-send-string dbprint "p")
(sk/repl-string-query python python-shell-send-string dblist "l")
(sk/repl-string-query python python-shell-send-string dbwhere "w")

;;; Shell

;; variable details
(sk/repl-dot-query python python-shell-send-string shape tmux)
(sk/repl-dot-query python python-shell-send-string ndim tmux)
(sk/repl-dot-query python python-shell-send-string size tmux)
(sk/repl-paren-query python python-shell-send-string len tmux)
(sk/repl-paren-query python python-shell-send-string type tmux)
(sk/repl-dot-paren-query python python-shell-send-string sum tmux)
(sk/repl-dot-paren-query python python-shell-send-string cumsum tmux)
(sk/repl-dot-paren-query python python-shell-send-string mean tmux)

;; environment
(sk/repl-query python python-shell-send-string ipython tmux)
(sk/repl-string-query python python-shell-send-string locals "locals()" tmux)
(sk/repl-string-query python python-shell-send-string globals "globals()" tmux)
(sk/repl-string-query python python-shell-send-string dir "dir()" tmux)
(sk/repl-string-query python python-shell-send-string whos "whos()" tmux)
(sk/repl-string-query python python-shell-send-string clear "%reset -f" tmux)

(defun sk/ipython-file-tmux ()
  "run the module"
  (interactive)
  (let* ((sk/buffer-name (buffer-name))
		 (sk/string-to-send (concat "run " sk/buffer-name)))
	(emamux:send-command sk/string-to-send)))

;; plotting
(defun sk/python-plot-tmux ()
  "get the plot of the symbol at point"
  (interactive)
  (let* ((sk/buffer-name (buffer-name))
		 (sk/string-to-send (concat "plt.plot(" (thing-at-point 'symbol) "); plt.show()")))
	(emamux:send-command sk/string-to-send)))

;; debugging
(defun sk/python-set-breakpoint-tmux ()
  "set the breakpoint at the current line"
  (interactive)
  (sk/smarter-move-beginning-of-line 1)
  (insert "import pdb; pdb.set_trace()\n"))

(defun sk/python-unset-breakpoint-tmux ()
  "unset the breakpoint at the current line"
  (interactive)
  (previous-line)
  (kill-whole-line))

(sk/repl-query python python-shell-send-string ipdb tmux)
(sk/repl-string-query python python-shell-send-string dbstep "n" tmux)
(sk/repl-string-query python python-shell-send-string dbprint "p" tmux)
(sk/repl-string-query python python-shell-send-string dblist "l" tmux)
(sk/repl-string-query python python-shell-send-string dbwhere "w" tmux)

;;;;;;;;;;;;;
;; Python  ;;
;;;;;;;;;;;;;

;; configure python itself
(use-package python
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :config
  (setq python-shell-interpreter "ipython"
		python-shell-interpreter-args "--simple-prompt -i")
  ;; (setq ansi-color-for-comint-mode t)
  ;; (setq python-shell-interpreter "python3")
  (setq python-shell-native-complete nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters "python3")

  ;; ;; Python auto completion
  ;; (use-package company-jedi
  ;; 	:ensure t
  ;; 	:demand t
  ;; 	:bind* (("C-t j"	. company-jedi)
  ;; 			("C-t C-j"	. company-jedi))
  ;; 	:config
  ;; 	(unless (package-installed-p 'jedi)
  ;; 	  (jedi:install-server)))

  ;; Python auto completion
  (use-package company-anaconda
	:ensure t
	:demand t
	:bind* (("C-t p"	. company-anaconda)
			("C-t C-p"	. company-anaconda)))

  ;; python semantic analyzer
  (use-package anaconda-mode
	:ensure t
    :demand t
	:diminish anaconda-mode
	:diminish anaconda-eldoc-mode
	:commands (anaconda-mode-find-definitions
			   anaconda-mode-find-assignments
			   anaconda-mode-find-references
			   anaconda-mode-find-file
			   anaconda-mode-show-doc
			   anaconda-mode-go-back)
	:config
	(anaconda-mode))

  ;; Format python code
  (use-package py-yapf
	:ensure t
    :demand t
	:commands (py-yapf-buffer
			   py-yapf-region))

  ;; sphinx documentation
  (use-package sphinx-doc
	:ensure t
    :demand t
	:diminish sphinx-doc-mode
	:commands (sphinx-doc)
	:config
	(sphinx-doc-mode))

  ;; virtualenv wrapper
  (use-package virtualenvwrapper
	:ensure t
    :demand t
	:commands (venv-workon
			   venv-deactivate
			   venv-set-location
			   venv-lsvirtualenv
			   venv-cdvirtualenv
			   venv-mkvirtualenv
			   venv-rmvirtualenv
			   venv-cpvirtualenv)
	:config
	;; virtualenv packages integration
	(use-package pyenv-mode
	  :ensure t
      :demand t
	  :commands (pyenv-mode-set
				 pyenv-mode-unset)
	  :config
	  (pyenv-mode))
	(setq eshell-prompt-function
		  (lambda ()
			(concat venv-current-name " $ ")))
	(venv-initialize-interactive-shells)
	(venv-initialize-eshell))

  ;; python testing
  (use-package pytest
	:ensure t
    :demand t
	:commands (pytest-one
			   pytest-all
			   pytest-directory
			   pytest-failed
			   pytest-module
			   pytest-pdb-one
			   pytest-pdb-all
			   pytest-pdb-directory
			   pytest-pdb-failed
			   pytest-pdb-module))

  ;; bindings
  (ryo-modal-key "m r" 'run-python :mode 'python-mode)
  (ryo-modal-key "m z" 'python-shell-switch-to-shell :mode 'python-mode)
  (ryo-modal-key "m m" 'python-shell-send-file :mode 'python-mode)
  (ryo-modal-key "m c" 'python-shell-send-string :mode 'python-mode)

  ;; functions
  (ryo-modal-key "m u" 'sk/python-clear-nil :mode 'python-mode)
  (ryo-modal-key "m i" 'sk/python-shape-nil :mode 'python-mode)
  (ryo-modal-key "m x" 'sk/python-size-nil :mode 'python-mode)
  (ryo-modal-key "m o" 'sk/python-ndim-nil :mode 'python-mode)
  (ryo-modal-key "m l" 'sk/python-len-nil :mode 'python-mode)
  (ryo-modal-key "m w" 'sk/python-type-nil :mode 'python-mode)
  (ryo-modal-key "m =" 'sk/python-sum-nil :mode 'python-mode)
  (ryo-modal-key "m +" 'sk/python-cumsum-nil :mode 'python-mode)
  (ryo-modal-key "m e" 'sk/python-mean-nil :mode 'python-mode)
  (ryo-modal-key "m a l" 'sk/python-locals-nil :mode 'python-mode)
  (ryo-modal-key "m a g" 'sk/python-globals-nil :mode 'python-mode)
  (ryo-modal-key "m a d" 'sk/python-dir-nil :mode 'python-mode)
  (ryo-modal-key "m a a" 'sk/python-whos-nil :mode 'python-mode)
  (ryo-modal-key "m f p" 'sk/python-plot :mode 'python-mode)

  ;; anaconda bindings
  (ryo-modal-key "m b" 'anaconda-mode-go-back :mode 'python-mode)
  (ryo-modal-key "m j j" 'anaconda-mode-find-definitions :mode 'python-mode)
  (ryo-modal-key "m j r" 'anaconda-mode-find-references :mode 'python-mode)
  (ryo-modal-key "m j a" 'anaconda-mode-find-assignments :mode 'python-mode)
  (ryo-modal-key "m d d" 'anaconda-mode-show-doc :mode 'python-mode)

  ;; debugging
  (ryo-modal-key "m g g" 'realgud:ipdb :mode 'python-mode)
  (ryo-modal-key "m g s" 'sk/python-set-breakpoint :mode 'python-mode)
  (ryo-modal-key "m g u" 'sk/python-unset-breakpoint :mode 'python-mode)

  ;; operator/textobject formatting
  (ryo-modal-key "m q" 'py-yapf-buffer :mode 'python-mode)

  ;; sphinx doc
  (ryo-modal-key "m d s" 'sphinx-doc :mode 'python-mode)

  ;; venv bindings
  (ryo-modal-key "m v w" 'venv-workon :mode 'python-mode)
  (ryo-modal-key "m v d" 'venv-deactivate :mode 'python-mode)
  (ryo-modal-key "m v t" 'venv-set-location :mode 'python-mode)
  (ryo-modal-key "m v l" 'venv-lsvirtualenv :mode 'python-mode)
  (ryo-modal-key "m v o" 'venv-cdvirtualenv :mode 'python-mode)
  (ryo-modal-key "m v r" 'venv-rmvirtualenv :mode 'python-mode)
  (ryo-modal-key "m v y" 'venv-cpvirtualenv :mode 'python-mode)
  (ryo-modal-key "m v s" 'pyenv-mode-set :mode 'python-mode)
  (ryo-modal-key "m v u" 'pyenv-mode-unset :mode 'python-mode)

  ;; test bindings
  (ryo-modal-key "m t o" 'pytest-one :mode 'python-mode)
  (ryo-modal-key "m t a" 'pytest-all :mode 'python-mode)
  (ryo-modal-key "m t d" 'pytest-directory :mode 'python-mode)
  (ryo-modal-key "m t f" 'pytest-failed :mode 'python-mode)
  (ryo-modal-key "m t m" 'pytest-module :mode 'python-mode)
  (ryo-modal-key "m p o" 'pytest-one :mode 'python-mode)
  (ryo-modal-key "m p a" 'pytest-all :mode 'python-mode)
  (ryo-modal-key "m p d" 'pytest-directory :mode 'python-mode)
  (ryo-modal-key "m p f" 'pytest-failed :mode 'python-mode)
  (ryo-modal-key "m p m" 'pytest-module :mode 'python-mode)

  ;;; Shell

  ;; bindings
  (ryo-modal-key "r r" 'sk/python-ipython-tmux :mode 'python-mode)
  (ryo-modal-key "r m" 'sk/ipython-file-tmux :mode 'python-mode)

  ;; functions
  (ryo-modal-key "r u" 'sk/python-clear-tmux :mode 'python-mode)
  (ryo-modal-key "r i" 'sk/python-shape-tmux :mode 'python-mode)
  (ryo-modal-key "r x" 'sk/python-size-tmux :mode 'python-mode)
  (ryo-modal-key "r o" 'sk/python-ndim-tmux :mode 'python-mode)
  (ryo-modal-key "r l" 'sk/python-len-tmux :mode 'python-mode)
  (ryo-modal-key "r w" 'sk/python-type-tmux :mode 'python-mode)
  (ryo-modal-key "r =" 'sk/python-sum-tmux :mode 'python-mode)
  (ryo-modal-key "r +" 'sk/python-cumsum-tmux :mode 'python-mode)
  (ryo-modal-key "r e" 'sk/python-mean-tmux :mode 'python-mode)
  (ryo-modal-key "r a l" 'sk/python-locals-tmux :mode 'python-mode)
  (ryo-modal-key "r a g" 'sk/python-globals-tmux :mode 'python-mode)
  (ryo-modal-key "r a d" 'sk/python-dir-tmux :mode 'python-mode)
  (ryo-modal-key "r a a" 'sk/python-whos-tmux :mode 'python-mode)
  (ryo-modal-key "r f p" 'sk/python-plot-tmux :mode 'python-mode)

  ;; debugging
  (ryo-modal-key "r g g" 'sk/python-ipdb-tmux :mode 'python-mode)
  (ryo-modal-key "r g s" 'sk/python-set-breakpoint-tmux :mode 'python-mode)
  (ryo-modal-key "r g u" 'sk/python-unset-breakpoint-tmux :mode 'python-mode))
;; operator/textobject bindings
(add-hook 'python-mode-hook
		  (sk/ryo-operator-object python-send "m s" "s" python-shell-send-region t python-mode))

;; pip packages
(use-package pippel
  :ensure t
  :commands (pippel-list-packages))
(ryo-modal-key "y z" 'pippel-list-packages)
(which-key-add-key-based-replacements
  "y z" "pip packages")

;; ryo major mode
(which-key-add-major-mode-key-based-replacements 'python-mode

  "m j" "jump"
  "m j j" "def"
  "m j r" "refs"
  "m j a" "assignments"
  "m d" "doc"
  "m d d" "find doc"
  "m d s" "sphinx doc"
  "m b" "go back"
  "m r" "run shell"
  "m z" "back to shell"
  "m m" "send file"
  "m c" "run command"

  ;; functions
  "m u" "clear"
  "m a" "list"
  "m f" "plot"
  "m i" "shape"
  "m x" "size"
  "m o" "dimensions"
  "m l" "length"
  "m w" "type"
  "m =" "sum"
  "m +" "cumulative sum"
  "m e" "mean"
  "m a l" "locals"
  "m a g" "globals"
  "m a d" "dir"
  "m a a" "all"
  "m f p" "plot variable"

  ;; virtualenv
  "m v" "environment"
  "m v w" "workon"
  "m v d" "deactivate"
  "m v t" "target location"
  "m v l" "list envs"
  "m v o" "open env"
  "m v r" "remove env"
  "m v y" "copy env"
  "m v s" "set pyvenv"
  "m v u" "unset pyvenv"

  ;; debugging
  "m g" "debug"
  "m g g" "ipdb"

  ;; tests
  "m t" "test"
  "m t o" "one"
  "m t a" "all"
  "m t d" "dir"
  "m t f" "failed"
  "m t m" "module"

  ;; pdb
  "m p" "pdb"
  "m p o" "one"
  "m p a" "all"
  "m p d" "dir"
  "m p f" "failed"
  "m p m" "module"

  ;; send
  "m s" "send"
  "m s s" "region"
  "m s i" "inside"
  "m s a" "around"
  "m s g" "global"
  "m s i a" "all"
  "m s a a" "all"
  "m s i w" "word"
  "m s a w" "word"
  "m s i p" "para"
  "m s a p" "para"
  "m s i s" "sentence"
  "m s a s" "sentence"
  "m s i l" "line"
  "m s a l" "line"
  "m s i y" "symbol"
  "m s a y" "symbol"
  "m s i c" "comment"
  "m s a c" "comment"
  "m s i f" "function"
  "m s a f" "function"
  "m s i q" "quote"
  "m s a q" "quote"
  "m s i b" "block/pairs"
  "m s a b" "block/pairs"
  "m s i h" "diff hunk"
  "m s a h" "diff hunk"
  "m s i x" "latex section"
  "m s a x" "latex section"
  "m s i o" "org code"
  "m s a o" "org code"
  "m s i u" "org subtree"
  "m s a u" "org subtree"
  "m s i e" "latex env"
  "m s a e" "latex env"
  "m s i r" "method call"
  "m s a r" "method call"
  "m s i d" "ruby block"
  "m s a d" "ruby block"
  "m s i g" "python string"
  "m s a g" "python string"
  "m s i m" "python block"
  "m s a m" "python block"
  "m s i n" "python statement"
  "m s a n" "python block and dec"
  "m s i $" "latex math"
  "m s a $" "latex math"
  "m s f" "to char"
  "m s F" "to char back"
  "m s t" "till char"
  "m s T" "till char back"
  "m s ;" "find on screen"
  "m s g ;" "till line"
  "m s h" "prev char"
  "m s j" "next line"
  "m s k" "prev line"
  "m s l" "next char"
  "m s 0" "till start of line"
  "m s $" "till end of line"
  "m s {" "till start of para"
  "m s }" "till end of para"
  "m s (" "till start of sentence"
  "m s )" "till end of sentence"
  "m s e" "end of word"
  "m s b" "start of word"
  "m s g g" "start of buffer"
  "m s G" "end of buffer"

  ;; format
  "m q" "format")

;;; Shell
(which-key-add-major-mode-key-based-replacements 'python-mode

  ;; functions
  "r u" "clear"
  "r a" "list"
  "r f" "plot"
  "r i" "shape"
  "r x" "size"
  "r o" "dimensions"
  "r l" "length"
  "r w" "type"
  "r =" "sum"
  "r +" "cumulative sum"
  "r e" "mean"
  "r a l" "locals"
  "r a g" "globals"
  "r a d" "dir"
  "r a a" "all"
  "r f p" "plot variable"

  ;; debugging
  "r g" "debug"
  "r g g" "ipdb"
  "r g s" "set breakpoint"
  "r g u" "unset breakpoint"

  ;; virtualenv
  "r v" "environment"
  "r v w" "workon"
  "r v d" "deactivate"
  "r v t" "target location"
  "r v l" "list envs"
  "r v o" "open env"
  "r v r" "remove env"
  "r v y" "copy env"
  "r v s" "set pyvenv"
  "r v u" "unset pyvenv"

  ;; tests
  "r t" "test"
  "r t o" "one"
  "r t a" "all"
  "r t d" "dir"
  "r t f" "failed"
  "r t m" "module"

  ;; pdb
  "r p" "pdb"
  "r p o" "one"
  "r p a" "all"
  "r p d" "dir"
  "r p f" "failed"
  "r p m" "module")

;;;;;;;;;;;;;;
;; Ipython  ;;
;;;;;;;;;;;;;;

(use-package ein
  :ensure t
  :commands (ein:notebooklist-open
			 ein:shared-output-eval-string
			 ein:pytools-jump-to-source-command
			 ein:notebook-worksheet-delete
			 ein:pytools-jump-back-command
			 ein:worksheet-next-input-history
			 ein:worksheet-previous-input-history
			 ein:completer-complete
			 ein:worksheet-execute-cell-and-goto-next
			 ein:worksheet-merge-cell
			 ein:worksheet-move-cell-up
			 ein:worksheet-rename-sheet
			 ein:worksheet-move-cell-down
			 ein:worksheet-goto-next-input
			 ein:worksheet-goto-prev-input
			 ein:notebook-scratchsheet-open
			 ein:notebook-worksheet-open-1th
			 ein:notebook-worksheet-open-2th
			 ein:notebook-worksheet-open-3th
			 ein:notebook-worksheet-open-4th
			 ein:notebook-worksheet-open-5th
			 ein:notebook-worksheet-open-6th
			 ein:notebook-worksheet-open-7th
			 ein:notebook-worksheet-open-8th
			 ein:notebook-worksheet-open-last
			 ein:notebook-worksheet-insert-next
			 ein:pytools-jump-to-source-command
			 ein:tb-show
			 ein:console-open
			 ein:notebook-close
			 ein:worksheet-execute-cell-and-insert-below
			 ein:worksheet-kill-cell
			 ein:worksheet-yank-cell
			 ein:worksheet-copy-cell
			 ein:worksheet-execute-cell
			 ein:worksheet-clear-output
			 ein:notebook-rename-command
			 ein:worksheet-toggle-output
			 ein:worksheet-goto-next-input
			 ein:worksheet-goto-prev-input
			 ein:pytools-jump-back-command
			 ein:worksheet-toggle-cell-type
			 ein:worksheet-turn-on-autoexec
			 ein:worksheet-insert-cell-above
			 ein:worksheet-insert-cell-below
			 ein:worksheet-toggle-slide-type
			 ein:notebook-worksheet-move-prev
			 ein:notebook-worksheet-move-next
			 ein:notebook-save-notebook-command
			 ein:notebook-worksheet-insert-prev
			 ein:notebook-restart-kernel-command
			 ein:notebook-kernel-interrupt-command
			 ein:pytools-request-tooltip-or-help
			 ein:worksheet-set-output-visibility-all
			 ein:notebook-kill-kernel-then-close-command
			 ein:shared-output-show-code-cell-at-point
			 ein:worksheet-clear-all-output
			 ein:notebook-switch-kernel)
  :config
  (add-hook 'ein:ipynb-mode-hook 'sk/enable-ryo-modal-mode)
  (add-hook 'ein:ipynb-mode-hook 'visual-line-mode)
  (add-hook 'ein:notebook-multilang-mode-hook 'sk/enable-ryo-modal-mode)
  (add-hook 'ein:notebook-multilang-mode-hook 'visual-line-mode))

;; ryo modal maps
(ryo-modal-keys
 ("g p l" ein:notebooklist-open)
 ("g p V" ein:shared-output-eval-string)
 ("g p j" ein:pytools-jump-to-source-command)
 ("g p k" ein:notebook-worksheet-delete)
 ("g p b" ein:pytools-jump-back-command)
 ("g p n" ein:worksheet-next-input-history)
 ("g p N" ein:worksheet-previous-input-history)
 ("g p TAB" ein:completer-complete)
 ("g p s" ein:worksheet-execute-cell-and-goto-next)
 ("g p m" ein:worksheet-merge-cell)
 ("g p u" ein:worksheet-move-cell-up)
 ("g p R" ein:worksheet-rename-sheet)
 ("g p d" ein:worksheet-move-cell-down)
 ("g p i" ein:worksheet-goto-next-input)
 ("g p I" ein:worksheet-goto-prev-input)
 ("g p RET" ein:notebook-scratchsheet-open)
 ("g p 1" ein:notebook-worksheet-open-1th)
 ("g p 2" ein:notebook-worksheet-open-2th)
 ("g p 3" ein:notebook-worksheet-open-3th)
 ("g p 4" ein:notebook-worksheet-open-4th)
 ("g p 5" ein:notebook-worksheet-open-5th)
 ("g p 5" ein:notebook-worksheet-open-6th)
 ("g p 6" ein:notebook-worksheet-open-7th)
 ("g p 7" ein:notebook-worksheet-open-8th)
 ("g p 8" ein:notebook-worksheet-open-last)
 ("g p 0" ein:notebook-worksheet-insert-next)
 ("g p T" ein:tb-show)
 ("g p e" ein:console-open)
 ("g p c" ein:notebook-close)
 ("g p x" ein:worksheet-execute-cell-and-insert-below)
 ("g p D" ein:worksheet-kill-cell)
 ("g p P" ein:worksheet-yank-cell)
 ("g p Y" ein:worksheet-copy-cell)
 ("g p S" ein:worksheet-execute-cell)
 ("g p C" ein:worksheet-clear-output)
 ("g p r" ein:notebook-rename-command)
 ("g p t" ein:worksheet-toggle-output)
 ("g p y" ein:worksheet-toggle-cell-type)
 ("g p a" ein:worksheet-turn-on-autoexec)
 ("g p O" ein:worksheet-insert-cell-above)
 ("g p o" ein:worksheet-insert-cell-below)
 ("g p Z" ein:worksheet-toggle-slide-type)
 ("g p W" ein:notebook-worksheet-move-prev)
 ("g p w" ein:notebook-worksheet-move-next)
 ("g p p" ein:notebook-save-notebook-command)
 ("g p E" ein:notebook-worksheet-insert-prev)
 ("g p K" ein:notebook-restart-kernel-command)
 ("g p z" ein:notebook-kernel-interrupt-command)
 ("g p h" ein:pytools-request-tooltip-or-help)
 ("g p g" ein:worksheet-set-output-visibility-all)
 ("g p G" ein:notebook-kill-kernel-then-close-command)
 ("g p z" ein:shared-output-show-code-cell-at-point)
 ("g p X" ein:worksheet-clear-all-output)
 ("g p v" ein:notebook-switch-kernel))

;; ipython notebooks
(which-key-add-key-based-replacements
  "g p" "ipython notebook"
  "g p l" "list"
  "g p V" "shared output eval"
  "g p j" "jump to source"
  "g p k" "kill worksheet"
  "g p b" "jump back"
  "g p n" "history next"
  "g p N" "history previous"
  "g p TAB" "complete"
  "g p s" "execute and next"
  "g p m" "merge cell"
  "g p u" "move cell up"
  "g p R" "rename sheet"
  "g p d" "move cell down"
  "g p i" "next input"
  "g p I" "previous input"
  "g p RET" "scratch open"
  "g p 1" "open first sheet"
  "g p 2" "open second sheet"
  "g p 3" "open third sheet"
  "g p 4" "open fourth sheet"
  "g p 5" "open fifth sheet"
  "g p 5" "open sixth sheet"
  "g p 6" "open seventh sheet"
  "g p 7" "open eigth sheet"
  "g p 8" "open ninth sheet"
  "g p 0" "open last sheet"
  "g p T" "show tb"
  "g p e" "open console"
  "g p c" "close notebook"
  "g p x" "execute and insert cell"
  "g p D" "kill cell"
  "g p P" "paste cell"
  "g p Y" "copy cell"
  "g p S" "execute cell"
  "g p C" "clear output"
  "g p r" "rename command"
  "g p t" "toggle output"
  "g p y" "toggle cell type"
  "g p a" "autoexec"
  "g p O" "cell above"
  "g p o" "cell below"
  "g p Z" "slide type"
  "g p W" "worksheet prev"
  "g p w" "worksheet next"
  "g p p" "save notebook"
  "g p E" "insert prev"
  "g p K" "restart kernel"
  "g p z" "kernel interrupt"
  "g p h" "tooltip or help"
  "g p g" "set output visibility"
  "g p G" "kill kernel then close"
  "g p z" "shared o/p at point"
  "g p X" "clear all output"
  "g p v" "switch kernel")

;; provide python configuration
(provide 'sk-python)
