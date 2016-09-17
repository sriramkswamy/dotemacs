;;;;;;;;;;;;;;;;;;;;;;
;;    Emacs Lisp    ;;
;;;;;;;;;;;;;;;;;;;;;;

(use-package macrostep
  :ensure t)
(general-evil-define-key '(normal visual) emacs-lisp-mode-map
  "J" 'find-function-at-point
  "K" 'describe-function)
(general-evil-define-key 'normal emacs-lisp-mode-map :prefix sk--evil-local-leader
			 "e" '(macrostep-expand :which-key "expand macro")
			 "c" '(macrostep-collapse :which-key "collapse macro")
			 "C" '(macrostep-collapse-all :which-key "collapse all")
			 "j" '(macrostep-next-macro :which-key "next macro")
			 "k" '(macrostep-prev-macro :which-key "prev macro")
			 "r" '(ielm :which-key "run ielm")
			 "s" '(eval-defun :which-key "evaluate defun")
			 "S" '(eval-buffer :which-key "evaluate buffer")
			 "x" '(eval-expression :which-key "evaluate expression")
			 "v" '(find-variable :which-key "find variable")
			 "V" '(describe-variable :which-key "describe variable")
			 "l" '(find-library :which-key "find library")
			 "L" '(describe-library :which-key "describe library"))
(general-evil-define-key 'visual emacs-lisp-mode-map :prefix sk--evil-local-leader
			 "s" '(eval-defun :which-key "evaluate defun"))

;; lisp interaction mode
(general-evil-define-key '(normal visual) lisp-interaction-mode-map
  "J" 'find-function-at-point
  "K" 'describe-function)
(general-evil-define-key 'normal lisp-interaction-mode-map :prefix sk--evil-local-leader
			 "e" '(macrostep-expand :which-key "expand macro")
			 "c" '(macrostep-collapse :which-key "collapse macro")
			 "C" '(macrostep-collapse-all :which-key "collapse all")
			 "j" '(macrostep-next-macro :which-key "next macro")
			 "k" '(macrostep-prev-macro :which-key "prev macro")
			 "r" '(ielm :which-key "run ielm")
			 "s" '(eval-defun :which-key "evaluate defun")
			 "S" '(eval-buffer :which-key "evaluate buffer")
			 "x" '(eval-expression :which-key "evaluate expression")
			 "v" '(find-variable :which-key "find variable")
			 "V" '(describe-variable :which-key "describe variable")
			 "l" '(find-library :which-key "find library")
			 "L" '(describe-library :which-key "describe library"))
(general-evil-define-key 'visual lisp-interaction-mode-map :prefix sk--evil-local-leader
			 "s" '(eval-defun :which-key "evaluate defun"))

;;;;;;;;;;;;;;;;;
;;    C/C++    ;;
;;;;;;;;;;;;;;;;;

;; compile functions
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(defun sk/compile-cpp-make ()
  "Compiles the file using the makefile in the current directory"
  (interactive)
  (compile
   (concat "make")))
(defun sk/compile-cpp-make-doc ()
  "Generates the documentation using the makefile in the current directory"
  (interactive)
  (compile
   (concat "make doc")))
(defun sk/compile-cpp-build ()
  "Compiles the file using the makefile in the build directory"
  (interactive)
  (compile
   (concat "make -C build")))
(defun sk/compile-cpp-build-doc ()
  "Generates the documentation using the makefile in the build directory"
  (interactive)
  (compile
   (concat "make -C build doc")))
(defun sk/compile-cpp-omp-math ()
  "Compiles the file with OpenMP and math libraries"
  (interactive)
  (compile
   (concat "g++ -Wall -fopenmp -lgsl -lcblas -llapack -O2 -g -std=c++11 " (buffer-file-name) " -o " (file-name-sans-extension buffer-file-name) ".out")))
(defun sk/compile-cpp-omp-simple ()
  "Compiles the file with OpenMP"
  (interactive)
  (compile
   (concat "g++ -Wall -fopenmp -g -std=c++11 " (buffer-file-name) " -o " (file-name-sans-extension buffer-file-name) ".out")))
(defun sk/compile-cpp-mpi-math ()
  "Compiles the file with MPI and math libraries"
  (interactive)
  (compile
   (concat "/usr/local/openmpi/bin/mpic++ -Wall -lgsl -lcblas -llapack -larmadillo -O2 -g -std=c++11 " (buffer-file-name) " -o " (file-name-sans-extension buffer-file-name) ".out")))
(defun sk/compile-cpp-mpi-simple ()
  "Compiles the file with MPI"
  (interactive)
  (compile
   (concat "/usr/local/openmpi/bin/c++ -Wall -g -std=c++11 " (buffer-file-name) " -o " (file-name-sans-extension buffer-file-name) ".out")))
(defun sk/compile-cpp-hybrid-math ()
  "Compiles the file with OpenMP, MPI and math libraries"
  (interactive)
  (compile
   (concat "/usr/local/openmpi/bin/c++ -Wall -fopenmp -lgsl -lcblas -llapack -larmadillo -O2 -g -std=c++11 " (buffer-file-name) " -o " (file-name-sans-extension buffer-file-name) ".out")))
(defun sk/compile-cpp-hybrid-simple ()
  "Compiles the file with OpenMP and MPI"
  (interactive)
  (compile
   (concat "/usr/local/openmpi/bin/c++ -Wall -fopenmp -g -std=c++11 " (buffer-file-name) " -o " (file-name-sans-extension buffer-file-name) ".out")))
(defun sk/compile-cpp-math ()
  "Compiles the file with math libraries"
  (interactive)
  (compile
   (concat "g++ -Wall -lgsl -lcblas -llapack -larmadillo -O2 -g -std=c++11 " (buffer-file-name) " -o " (file-name-sans-extension buffer-file-name) ".out")))
(defun sk/compile-cpp-simple ()
  "Compiles the file"
  (interactive)
  (compile
   (concat "g++ -Wall -g -std=c++11 " (buffer-file-name) " -o " (file-name-sans-extension buffer-file-name) ".out")))
;; general mappings
(general-evil-define-key '(normal visual) c++-mode-map :prefix sk--evil-local-leader
			 "cc" 'sk/compile-cpp-math
			 "cC" 'sk/compile-cpp-simple
			 "cb" 'sk/compile-cpp-build
			 "cB" 'sk/compile-cpp-build-doc
			 "b" 'sk/compile-cpp-build
			 "B" 'sk/compile-cpp-build-doc
			 "m" 'sk/compile-cpp-make
			 "M" 'sk/compile-cpp-make-doc
			 "cm" 'sk/compile-cpp-mpi-math
			 "cM" 'sk/compile-cpp-mpi-simple
			 "ch" 'sk/compile-cpp-hybrid-math
			 "cH" 'sk/compile-cpp-hybrid-simple)

;; c++ indexer and semantics
(use-package rtags
  :ensure t
  :general
  (general-evil-define-key '(normal visual) c++-mode-map
    "J" 'rtags-find-symbol-at-point
    "K" 'rtags-print-symbol-info)
  (general-evil-define-key '(normal visual) c++-mode-map :prefix sk--evil-local-leader
			   "s" 'rtags-find-symbol-at-point
			   "S" 'rtags-find-symbol
			   "r" 'rtags-find-references-at-point
			   "R" 'rtags-find-references
			   "v" 'rtags-find-virtuals-at-point
			   "d" 'rtags-print-dependencies
			   "i" 'rtags-print-symbol-info
			   "t" 'rtags-symbol-type
			   "e" 'rtags-print-enum-value-at-point
			   "D" 'rtags-diagnostics
			   "]" 'rtags-location-stack-forward
			   "[" 'rtags-location-stack-backward
			   "n" 'rtags-next-match
			   "p" 'rtags-previous-match
			   "F" 'rtags-fixit
			   "P" 'rtags-preprocess-file
			   "N" 'rtags-rename-symbol)
  :init
  (setq rtags-autostart-diagnostics t)
  (setq rtags-completions-enabled t))

;; convert emacs into a c++ ide based on cmake
(use-package cmake-ide
  :ensure t
  :defer 2
  :config
  (cmake-ide-setup))

;;;;;;;;;;;;;;;;;;
;;    Python    ;;
;;;;;;;;;;;;;;;;;;

;; configure python itself
(use-package python
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :general
  (general-evil-define-key 'normal python-mode-map :prefix sk--evil-local-leader
			   "s" 'python-shell-send-defun
			   "S" 'python-shell-send-buffer
			   "r" 'run-python
			   "R" 'python-shell-switch-to-shell)
  (general-evil-define-key 'visual python-mode-map :prefix sk--evil-local-leader
			   "s" 'python-shell-send-region)
  :config
  (setq python-shell-interpreter "ipython"
	python-shell-interpreter-args "--simple-prompt -i")
  ;; (setq ansi-color-for-comint-mode t)
  ;; (setq python-shell-interpreter "python3")
  (setq python-shell-native-complete nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters "python3")
  ;; python semantic analyzer
  (use-package anaconda-mode
    :ensure t
    :diminish anaconda-mode
    :diminish anaconda-eldoc-mode
    :general
    (general-evil-define-key '(normal visual) python-mode-map
      "J" 'anaconda-mode-find-definitions
      "K" 'anaconda-mode-show-doc)
    (general-evil-define-key 'normal python-mode-map :prefix sk--evil-local-leader
			     "d" 'anaconda-mode-find-definitions
			     "a" 'anaconda-mode-find-assignments
			     "f" 'anaconda-mode-find-references
			     "F" 'anaconda-mode-find-file
			     "[" 'anaconda-mode-go-back)
    (general-evil-define-key 'normal anaconda-view-mode-map
      "q" (general-simulate-keys "q" t "quit"))
    :config
    (progn
      (add-hook 'python-mode-hook 'anaconda-mode))))

;; virtualenv packages integration
(use-package pyenv-mode
  :ensure t
  :general
  (general-evil-define-key 'normal python-mode-map :prefix sk--evil-local-leader
			   "e" 'pyenv-mode-set
			   "u" 'pyenv-mode-unset)
  :config
  (pyenv-mode))

;; virtualenv wrapper
(use-package virtualenvwrapper
  :ensure t
  :general
  (general-evil-define-key 'normal python-mode-map :prefix sk--evil-local-leader
			   "w" 'venv-workon
			   "vd" 'venv-deactivate
			   "vl" 'venv-lsvirtualenv
			   "vm" 'venv-mkvirtualenv
			   "vr" 'venv-rmvirtualenv
			   "vc" 'venv-cdvirtualenv
			   "vp" 'venv-cpvirtualenv
			   "l" 'venv-set-location)
  :config
  (setq eshell-prompt-function
	(lambda ()
	  (concat venv-current-name " $ ")))
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell))

;; Format python code
(use-package py-yapf
  :ensure t
  :general
  (general-evil-define-key 'normal python-mode-map :prefix sk--evil-local-leader
			   "y" 'py-yapf-buffer)
  (general-evil-define-key 'visual python-mode-map :prefix sk--evil-local-leader
			   "y" 'py-yapf-region))

;; sphinx documentation
(use-package sphinx-doc
  :ensure t
  :diminish sphinx-doc-mode
  :general
  (general-evil-define-key '(normal visual) python-mode-map :prefix sk--evil-local-leader
			   "x" 'sphinx-doc))

;; python testing
(use-package pytest
  :ensure t
  :general
  (general-evil-define-key '(normal visual) python-mode-map :prefix sk--evil-local-leader
			   "ta" 'pytest-all
			   "td" 'pytest-directory
			   "tf" 'pytest-failed
			   "tm" 'pytest-module
			   "to" 'pytest-one
			   "pa" 'pytest-pdb-all
			   "pd" 'pytest-pdb-directory
			   "pf" 'pytest-pdb-failed
			   "pm" 'pytest-pdb-module
			   "po" 'pytest-pdb-one))

;;;;;;;;;;;;;;;;;
;;    Stats    ;;
;;;;;;;;;;;;;;;;;

;; Emacs Speaks Statistics
(use-package ess
  :ensure t
  :mode (("\\.r$" . R-mode)
         ("\\.R$" . R-mode)
         ("\\.jl$" . julia-mode))
  :general
  (general-evil-define-key 'normal ess-mode-map :prefix sk--evil-local-leader
			   "r" 'R
			   "j" 'julia
			   "R" 'ess-switch-to-ESS
			   "J" 'ess-switch-to-ESS
			   "s" 'ess-eval-function
			   "S" 'ess-eval-buffer
			   "l" 'ess-eval-line)
  (general-evil-define-key 'visual ess-mode-map :prefix sk--evil-local-leader
			   "s" 'ess-eval-region)
  :config
  (require 'ess-site))

;;;;;;;;;;;;;;;;;;
;;    MATLAB    ;;
;;;;;;;;;;;;;;;;;;

;; install matlab from OCIO first
(use-package matlab-mode
  :ensure t
  :mode ("\\.m$" . matlab-mode)
  :bind (:map matlab-shell-mode-map
              ("C-c C-c" . term-interrupt-subjob))
  :init
  (setq matlab-shell-command "/Applications/MATLAB_R2016a.app/bin/matlab"
	matlab-mode-install-path '("~/Dropbox/PhD/codes/ttmat" "/Applications/MATLAB_R2016a.app/toolbox")
        matlab-indent-function t)
  (eval-after-load 'matlab
    '(add-to-list 'matlab-shell-command-switches "-nodesktop -nosplash"))
  :general
  (general-nvmap "J" 'matlab-find-file-on-path
		 "K" 'matlab-shell-describe-command)
  (general-evil-define-key '(normal visual) matlab-mode-map :prefix sk--evil-local-leader
			   "r" 'matlab-shell
			   "R" 'matlab-show-matlab-shell-buffer
			   "s" 'matlab-shell-run-region-or-line
			   "S" 'matlab-shell-run-cell
			   "c" 'matlab-shell-run-command
			   "t" 'matlab-shell-topic-browser
			   "v" 'matlab-shell-describe-variable
			   "l" 'matlab-show-line-info))

;;;;;;;;;;;;;;;
;;    Web    ;;
;;;;;;;;;;;;;;;

;; web mode for html files
(use-package web-mode
  :ensure t
  :mode ("\\.html$" . web-mode)
  :general
  (general-evil-define-key '(normal visual) web-mode-map :prefix sk--evil-local-leader
			   "w" 'httpd-start
			   "W" 'httpd-stop)
  :config
  ;; impatient - refresh HTML immediately
  (use-package impatient-mode
    :ensure t
    :diminish (impatient-mode . " ι")
    :general
    (general-evil-define-key '(normal visual) web-mode-map :prefix sk--evil-local-leader
			     "i" 'impatient-mode))
  ;; js evaluation
  (use-package skewer-mode
    :ensure t
    :diminish skewer-mode
    :general
    (general-evil-define-key '(normal visual) web-mode-map :prefix sk--evil-local-leader
			     "m" 'skewer-mode
			     "r" 'skewer-repl
			     "R" 'run-skewer
			     "h" 'skewer-html-mode
			     "c" 'skewer-css-mode
			     "C" 'list-skewer-clients
			     "s" 'skewer-eval-defun
			     "S" 'skewer-load-buffer
			     "l" 'skewer-eval-last-expression
			     "L" 'skewer-eval-print-last-expression
			     "p" 'skewer-run-phantomjs
			     "P" 'skewer-phantomjs-kill
			     "b" 'skewer-bower-load
			     "B" 'skewer-bower-refresh))
  ;; beautify content
  (use-package web-beautify
    :ensure t
    :general
    (general-evil-define-key 'normal web-mode-map :prefix sk--evil-local-leader
			     "fh" 'web-beautify-html-buffer
			     "fc" 'web-beautify-css-buffer
			     "fj" 'web-beautify-js-buffer)
    (general-evil-define-key 'visual web-mode-map :prefix sk--evil-local-leader
			     "fh" 'web-beautify-html
			     "fc" 'web-beautify-css
			     "fj" 'web-beautify-js)))

;; js3 mode for javascript
(use-package js3-mode
  :ensure t
  :mode ("\\.js$" . js3-mode)
  :config
  ;; JS semantic navigation
  (use-package tern
    :ensure t
    :diminish tern-mode
    :general
    (general-evil-define-key '(normal visual) js3-mode-map
      "J" 'tern-find-definition
      "K" 'tern-get-docs)
    (general-evil-define-key 'normal js3-mode-map :prefix sk--evil-local-leader
			     "t" 'tern-get-type
			     "u" 'tern-use-server
			     "h" 'tern-highlight-refs
			     "N" 'tern-rename-variable)
    (general-evil-define-key '(normal visual) js-mode-map
      "J" 'tern-find-definition
      "K" 'tern-get-docs)
    (general-evil-define-key 'normal js-mode-map :prefix sk--evil-local-leader
			     "t" 'tern-get-type
			     "u" 'tern-use-server
			     "h" 'tern-highlight-refs
			     "N" 'tern-rename-variable)
    :config
    (progn
      (add-hook 'js-mode-hook '(lambda () (tern-mode t)))
      (add-hook 'js3-mode-hook '(lambda () (tern-mode t)))))
  ;; format js code
  (use-package jsfmt
    :ensure t
    :general
    (general-evil-define-key '(normal visual) js3-mode-map :prefix sk--evil-local-leader
			     "j" 'jsfmt
			     "J" 'jsfmt-ast)
    (general-evil-define-key '(normal visual) js-mode-map :prefix sk--evil-local-leader
			     "j" 'jsfmt
			     "J" 'jsfmt-ast))
  ;; nodejs REPL for JS evaluation
  (use-package nodejs-repl
    :ensure t
    :general
    (general-evil-define-key 'normal js3-mode-map :prefix sk--evil-local-leader
			     "r" 'nodejs-repl
			     "R" 'nodejs-repl-switch-to-repl
			     "s" 'nodejs-repl-send-last-sexp
			     "S" 'nodejs-repl-send-buffer
			     "l" 'nodejs-repl-load-file
			     "e" 'nodejs-repl-execute)
    (general-evil-define-key 'visual js3-mode-map :prefix sk--evil-local-leader
			     "s" 'nodejs-repl-send-region)
    (general-evil-define-key 'normal js-mode-map :prefix sk--evil-local-leader
			     "r" 'nodejs-repl
			     "R" 'nodejs-repl-switch-to-repl
			     "s" 'nodejs-repl-send-last-sexp
			     "S" 'nodejs-repl-send-buffer
			     "l" 'nodejs-repl-load-file
			     "e" 'nodejs-repl-execute)
    (general-evil-define-key 'visual js-mode-map :prefix sk--evil-local-leader
			     "s" 'nodejs-repl-send-region)
    :config
    (use-package nvm
      :ensure t
      :general
      (general-evil-define-key '(normal visual) js3-mode-map :prefix sk--evil-local-leader
			       "v" 'nvm-use
			       "V" 'nvm-use-for)
      (general-evil-define-key '(normal visual) js-mode-map :prefix sk--evil-local-leader
			       "v" 'nvm-use
			       "V" 'nvm-use-for))))

;; coffee script syntax highlighting
(use-package coffee-mode
  :ensure t
  :mode "\\.coffee$")

;; SCSS syntax
(use-package scss-mode
  :ensure t
  :mode "\\.scss$")

;; json syntax
(use-package json-mode
  :ensure t
  :mode "\\.json$")

;; Nginx syntax
(use-package nginx-mode
  :ensure t
  :commands (nginx-mode))

;; emmet for easy HTML
(use-package emmet-mode
  :ensure t
  :diminish (emmet-mode . " ε")
  :general
  (general-imap "C-o" 'emmet-expand-line)
  (general-imap "C-y" 'emmet-prev-edit-point)
  (general-imap "C-e" 'emmet-next-edit-point))

;; provide the entire configuration
(provide 'sk-programming)
