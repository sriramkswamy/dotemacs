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

;; python operator bindings
(defun sk/python-operator ()
  "operator text object bindings for python"
  (interactive)
  (eval `(ryo-modal-major-mode-keys
		  'python-mode
		  ("m s" ,text-objects :then '(python-shell-send-region)))))

;;;;;;;;;;;;;
;; Python  ;;
;;;;;;;;;;;;;

(use-package python
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :hook ((python-mode . sk/python-operator)
		 (python-mode . sk/company-python))
  :interpreter ("python" . python-mode)
  :commands
  (python-shell-send-string
   python-shell-send-region
   python-shell-switch-to-shell
   python-shell-send-file)
  :init
  ;; (setq python-shell-interpreter "ipython"
  ;; 		python-shell-interpreter-args "--simple-prompt -i")
  ;; (setq ansi-color-for-comint-mode t)
  ;; (add-to-list 'python-shell-completion-native-disabled-interpreters "python3")
  (setq python-shell-native-complete nil))

;; navigation and auto completion
(use-package anaconda-mode
  :ensure t
  :hook (python-mode . anaconda-mode)
  :diminish anaconda-mode
  :diminish anaconda-eldoc-mode
  :commands
  (anaconda-mode-find-definitions
   anaconda-mode-find-assignments
   anaconda-mode-find-references
   anaconda-mode-find-file
   anaconda-mode-show-doc
   anaconda-mode-go-back))

(use-package company-anaconda
  :ensure t
  :after (anaconda-mode)
  :bind* (("C-j p"	. company-anaconda))
  :bind (:map python-mode-map
			  ("C-d" . company-anaconda)))

;; format python code
(use-package py-yapf
  :ensure t
  :commands
  (py-yapf-buffer
   py-yapf-region))

;; sphinx documentation
(use-package sphinx-doc
  :ensure t
  :diminish sphinx-doc-mode
  :commands
  (sphinx-doc)
  :config
  (sphinx-doc-mode))

;; virtualenv wrapper
(use-package virtualenvwrapper
  :ensure t
  :commands
  (venv-workon
   venv-deactivate
   venv-set-location
   venv-lsvirtualenv
   venv-cdvirtualenv
   venv-mkvirtualenv
   venv-mkvirtualenv-using
   venv-rmvirtualenv
   venv-cpvirtualenv)
  :config
  ;; settings
  (setq venv-location "/home/sriramkrish92/.virtualenvs/")
  (setq eshell-prompt-function
		(lambda ()
		  (concat venv-current-name " $ ")))
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell))

;; testing
(use-package pytest
  :ensure t
  :commands
  (pytest-one
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
(ryo-modal-major-mode-keys
 'python-mode
 ;; bindings
 ("m r" run-python :name "run repl")
 ("m z" python-shell-switch-to-shell :name "goto shell")
 ("m m" python-shell-send-file :name "run file")
 ("m c" python-shell-send-string :name "command")

 ;; functions
 ("m u" sk/python-clear-nil :name "clear all")
 ("m i" sk/python-shape-nil :name "shape info")
 ("m x" sk/python-size-nil :name "size")
 ("m o" sk/python-ndim-nil :name "dimensions")
 ("m l" sk/python-len-nil :name "length")
 ("m w" sk/python-type-nil :name "type")
 ("m =" sk/python-sum-nil :name "sum")
 ("m +" sk/python-cumsum-nil :name "cumulative sum")
 ("m e" sk/python-mean-nil :name "mean")
 ("m a l" sk/python-locals-nil :name "locals")
 ("m a g" sk/python-globals-nil :name "globals")
 ("m a d" sk/python-dir-nil :name "dir")
 ("m a a" sk/python-whos-nil :name "all")
 ("m f p" sk/python-plot :name "plot")

 ;; anaconda bindings
 ("m b" anaconda-mode-go-back :name "jump back")
 ("m j j" anaconda-mode-find-definitions :name "definitions")
 ("m j r" anaconda-mode-find-references :name "references")
 ("m j a" anaconda-mode-find-assignments :name "assignments")
 ("m d d" anaconda-mode-show-doc :name "doc")

 ;; debugging
 ("m g g" realgud:pdb :name "debug start")
 ("m g s" sk/gud-break :name "set break")
 ("m g x" sk/gud-remove :name "delete break")
 ("m g l" sk/python-dblist-nil :name "list")
 ("m g w" sk/python-dbwhere-nil :name "where")
 ("m g q" sk/gud-finish :name "quit")
 ("m g n" sk/gud-next :name "next")
 ("m g i" sk/gud-step :name "step in")
 ("m g c" sk/gud-cont :name "continue")
 ("m g u" sk/gud-up :name "up")
 ("m g p" sk/gud-print :name "print")
 ("m g r" sk/gud-refresh :name "refresh")
 ("m g f" sk/gud-find-c-expr :name "find c expression")

 ;; operator/textobject formatting
 ("m q" py-yapf-buffer :name "format")

 ;; sphinx doc
 ("m d s" sphinx-doc :name "sphinx")

 ;; venv bindings
 ("m v w" venv-workon :name "work on")
 ("m v d" venv-deactivate :name "deactivate")
 ("m v t" venv-set-location :name "set location")
 ("m v l" venv-lsvirtualenv :name "list")
 ("m v o" venv-cdvirtualenv :name "open")
 ("m v r" venv-rmvirtualenv :name "remove")
 ("m v y" venv-cpvirtualenv :name "copy")
 ("m v v" venv-mkvirtualenv :name "make")

 ;; test bindings
 ("m t o" pytest-one :name "one")
 ("m t a" pytest-all :name "all")
 ("m t d" pytest-directory :name "directory")
 ("m t f" pytest-failed :name "failed")
 ("m t m" pytest-module :name "module")
 ("m p o" pytest-pdb-one :name "one")
 ("m p a" pytest-pdb-all :name "all")
 ("m p d" pytest-pdb-directory :name "directory")
 ("m p f" pytest-pdb-failed :name "failed")
 ("m p m" pytest-pdb-module :name "module")

 ;;; Shell

 ;; bindings
 ("r r" sk/python-ipython-tmux :name "start repl")
 ("r m" sk/ipython-file-tmux :name "run file")

 ;; functions
 ("r u" sk/python-clear-tmux :name "clear all")
 ("r i" sk/python-shape-tmux :name "shape info")
 ("r x" sk/python-size-tmux :name "size")
 ("r o" sk/python-ndim-tmux :name "dimensions")
 ("r l" sk/python-len-tmux :name "length")
 ("r w" sk/python-type-tmux :name "type")
 ("r =" sk/python-sum-tmux :name "sum")
 ("r +" sk/python-cumsum-tmux :name "cumulative sum")
 ("r e" sk/python-mean-tmux :name "mean")
 ("r a l" sk/python-locals-tmux :name "locals")
 ("r a g" sk/python-globals-tmux :name "globals")
 ("r a d" sk/python-dir-tmux :name "directory")
 ("r a a" sk/python-whos-tmux :name "all")
 ("r f p" sk/python-plot-tmux :name "plot")

 ;; debugging
 ("r g g" sk/python-ipdb-tmux :name "debug start")
 ("r g s" sk/python-set-breakpoint-tmux :name "set breakpoint")
 ("r g u" sk/python-unset-breakpoint-tmux :name "unset breakpoint"))

;; which key hints
(which-key-add-major-mode-key-based-replacements 'python-mode
  "r g" "debug"
  "r f" "figure"
  "r a" "analyse"
  
  "m g" "debug"
  "m f" "figure"
  "m a" "analyse"
  "m j" "jump"
  "m d" "doc"
  "m p" "pdb test"
  "m t" "test"
  "m v" "virtualenv"
  
  "m s" "eval"
  "m s i" "inside"
  "m s a" "around"
  "m s g" "global")

;; provide python configuration
(provide 'sk-python)
