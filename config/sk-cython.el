;;;;;;;;;;;;;;;;;;;;;;;;
;; Wrapper functions  ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun sk/cython-shape ()
  "get the shape of the symbol at point"
  (interactive)
  (let* ((sk/buffer-name (buffer-name))
		 (sk/string-to-send (concat (thing-at-point 'symbol) ".shape")))
	(python-shell-send-string sk/string-to-send)))

(defun sk/cython-ndim ()
  "get the ndim of the symbol at point"
  (interactive)
  (let* ((sk/buffer-name (buffer-name))
		 (sk/string-to-send (concat (thing-at-point 'symbol) ".ndim")))
	(python-shell-send-string sk/string-to-send)))

(defun sk/cython-size ()
  "get the size of the symbol at point"
  (interactive)
  (let* ((sk/buffer-name (buffer-name))
		 (sk/string-to-send (concat (thing-at-point 'symbol) ".size")))
	(python-shell-send-string sk/string-to-send)))

(defun sk/cython-len ()
  "get the len of the symbol at point"
  (interactive)
  (let* ((sk/buffer-name (buffer-name))
		 (sk/string-to-send (concat "len(" (thing-at-point 'symbol) ")")))
	(python-shell-send-string sk/string-to-send)))

(defun sk/cython-type ()
  "get the type of the symbol at point"
  (interactive)
  (let* ((sk/buffer-name (buffer-name))
		 (sk/string-to-send (concat "type(" (thing-at-point 'symbol) ")")))
	(python-shell-send-string sk/string-to-send)))

(defun sk/cython-sum ()
  "get the sum of the symbol at point"
  (interactive)
  (let* ((sk/buffer-name (buffer-name))
		 (sk/string-to-send (concat (thing-at-point 'symbol) ".sum()")))
	(python-shell-send-string sk/string-to-send)))

(defun sk/cython-cumsum ()
  "get the cumsum of the symbol at point"
  (interactive)
  (let* ((sk/buffer-name (buffer-name))
		 (sk/string-to-send (concat (thing-at-point 'symbol) ".cumsum()")))
	(python-shell-send-string sk/string-to-send)))

(defun sk/cython-mean ()
  "get the mean of the symbol at point"
  (interactive)
  (let* ((sk/buffer-name (buffer-name))
		 (sk/string-to-send (concat (thing-at-point 'symbol) ".mean()")))
	(python-shell-send-string sk/string-to-send)))

(defun sk/cython-locals ()
  "get the locals"
  (interactive)
  (let* ((sk/buffer-name (buffer-name))
		 (sk/string-to-send (concat "locals()")))
	(python-shell-send-string sk/string-to-send)))

(defun sk/cython-globals ()
  "get the globals"
  (interactive)
  (let* ((sk/buffer-name (buffer-name))
		 (sk/string-to-send (concat "globals()")))
	(python-shell-send-string sk/string-to-send)))

(defun sk/cython-dir ()
  "get the dir"
  (interactive)
  (let* ((sk/buffer-name (buffer-name))
		 (sk/string-to-send (concat "dir()")))
	(python-shell-send-string sk/string-to-send)))

(defun sk/cython-whos ()
  "get the whos"
  (interactive)
  (let* ((sk/buffer-name (buffer-name))
		 (sk/string-to-send (concat "whos")))
	(python-shell-send-string sk/string-to-send)))

(defun sk/cython-plot ()
  "get the plot of the symbol at point"
  (interactive)
  (let* ((sk/buffer-name (buffer-name))
		 (sk/string-to-send "plt.plot(" (concat (thing-at-point 'symbol) "); plt.show()")))
	(python-shell-send-string sk/string-to-send)))

(defun sk/cython-set-breakpoint ()
  "set the breakpoint at the current line"
  (interactive)
  (sk/smarter-move-beginning-of-line 1)
  (insert "import pdb; pdb.set_trace()\n"))

(defun sk/cython-unset-breakpoint ()
  "unset the breakpoint at the current line"
  (interactive)
  (previous-line)
  (kill-whole-line))

(defun sk/cython-clear ()
  "clear all the variables"
  (interactive)
  (python-shell-send-string "%reset -f"))

;;;;;;;;;;;;;
;; Cython  ;;
;;;;;;;;;;;;;

;; configure python itself
(use-package cython
  :ensure t
  :mode ("\\.cpy\\'" . cython-mode)
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
  (ryo-modal-key "m r" 'run-python :mode 'cython-mode)
  (ryo-modal-key "m z" 'python-shell-switch-to-shell :mode 'cython-mode)
  (ryo-modal-key "m m" 'python-shell-send-file :mode 'cython-mode)
  (ryo-modal-key "m c" 'python-shell-send-string :mode 'cython-mode)

  ;; functions
  (ryo-modal-key "m u" 'sk/cython-clear :mode 'cython-mode)
  (ryo-modal-key "m i" 'sk/cython-shape :mode 'cython-mode)
  (ryo-modal-key "m x" 'sk/cython-size :mode 'cython-mode)
  (ryo-modal-key "m o" 'sk/cython-ndim :mode 'cython-mode)
  (ryo-modal-key "m l" 'sk/cython-len :mode 'cython-mode)
  (ryo-modal-key "m w" 'sk/cython-type :mode 'cython-mode)
  (ryo-modal-key "m =" 'sk/cython-sum :mode 'cython-mode)
  (ryo-modal-key "m +" 'sk/cython-cumsum :mode 'cython-mode)
  (ryo-modal-key "m e" 'sk/cython-mean :mode 'cython-mode)
  (ryo-modal-key "m a l" 'sk/cython-locals :mode 'cython-mode)
  (ryo-modal-key "m a g" 'sk/cython-globals :mode 'cython-mode)
  (ryo-modal-key "m a d" 'sk/cython-dir :mode 'cython-mode)
  (ryo-modal-key "m a a" 'sk/cython-whos :mode 'cython-mode)
  (ryo-modal-key "m f p" 'sk/cython-plot :mode 'cython-mode)

  ;; anaconda bindings
  (ryo-modal-key "m b" 'anaconda-mode-go-back :mode 'cython-mode)
  (ryo-modal-key "m j j" 'anaconda-mode-find-definitions :mode 'cython-mode)
  (ryo-modal-key "m j r" 'anaconda-mode-find-references :mode 'cython-mode)
  (ryo-modal-key "m j a" 'anaconda-mode-find-assignments :mode 'cython-mode)
  (ryo-modal-key "m d d" 'anaconda-mode-show-doc :mode 'cython-mode)

  ;; debugging
  (ryo-modal-key "m g g" 'realgud:ipdb :mode 'cython-mode)
  (ryo-modal-key "m g s" 'sk/cython-set-breakpoint :mode 'cython-mode)
  (ryo-modal-key "m g u" 'sk/cython-unset-breakpoint :mode 'cython-mode)

  ;; operator/textobject formatting
  (ryo-modal-key "m q" 'py-yapf-buffer :mode 'cython-mode)

  ;; sphinx doc
  (ryo-modal-key "m d s" 'sphinx-doc :mode 'cython-mode)

  ;; venv bindings
  (ryo-modal-key "m v w" 'venv-workon :mode 'cython-mode)
  (ryo-modal-key "m v d" 'venv-deactivate :mode 'cython-mode)
  (ryo-modal-key "m v t" 'venv-set-location :mode 'cython-mode)
  (ryo-modal-key "m v l" 'venv-lsvirtualenv :mode 'cython-mode)
  (ryo-modal-key "m v o" 'venv-cdvirtualenv :mode 'cython-mode)
  (ryo-modal-key "m v r" 'venv-rmvirtualenv :mode 'cython-mode)
  (ryo-modal-key "m v y" 'venv-cpvirtualenv :mode 'cython-mode)
  (ryo-modal-key "m v s" 'pyenv-mode-set :mode 'cython-mode)
  (ryo-modal-key "m v u" 'pyenv-mode-unset :mode 'cython-mode)

  ;; test bindings
  (ryo-modal-key "m t o" 'pytest-one :mode 'cython-mode)
  (ryo-modal-key "m t a" 'pytest-all :mode 'cython-mode)
  (ryo-modal-key "m t d" 'pytest-directory :mode 'cython-mode)
  (ryo-modal-key "m t f" 'pytest-failed :mode 'cython-mode)
  (ryo-modal-key "m t m" 'pytest-module :mode 'cython-mode)
  (ryo-modal-key "m p o" 'pytest-one :mode 'cython-mode)
  (ryo-modal-key "m p a" 'pytest-all :mode 'cython-mode)
  (ryo-modal-key "m p d" 'pytest-directory :mode 'cython-mode)
  (ryo-modal-key "m p f" 'pytest-failed :mode 'cython-mode)
  (ryo-modal-key "m p m" 'pytest-module :mode 'cython-mode))
;; operator/textobject bindings
(add-hook 'cython-mode-hook
		  (sk/ryo-operator-object python-send "m s" "s" python-shell-send-region t cython-mode))

;; pip packages
(use-package pippel
  :ensure t
  :commands (pippel-list-packages))
(ryo-modal-key "y z" 'pippel-list-packages)
(which-key-add-key-based-replacements
  "y z" "pip packages")

;; ryo major mode
(which-key-add-major-mode-key-based-replacements 'cython-mode

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
