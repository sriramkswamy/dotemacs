;;;;;;;;;;;;;;;;;;;;;;
;;    Emacs Lisp    ;;
;;;;;;;;;;;;;;;;;;;;;;

(use-package macrostep
  :ensure t
  :bind (:map emacs-lisp-mode-map
			  ("M-\\" . hydra-elisp/body)
			  ("C-2" . eval-region)
			  ("C-3" . eval-last-sexp)
			  ("C-4" . sk/ielm)
			  ("C-5" . eval-buffer)
			  ("C-6" . eval-defun)
			  ("C-7" . describe-variable)
			  ("C-8" . find-variable)
			  ("C-9" . describe-function)
			  ("C-0" . find-function)))
(bind-keys*
 ("C-\\ e" . hydra-elisp/body)
 ("C-\\ C-e" . hydra-elisp/body))

;; lisp interaction mode
(bind-keys :map lisp-interaction-mode-map
		   ("M-\\" . hydra-elisp/body)
		   ("C-2" . eval-region)
		   ("C-3" . eval-last-sexp)
		   ("C-4" . sk/ielm)
		   ("C-5" . eval-buffer)
		   ("C-6" . eval-defun)
		   ("C-7" . describe-variable)
		   ("C-8" . find-variable)
		   ("C-9" . describe-function)
		   ("C-0" . find-function))

;; hydra for emacs lisp
(defhydra hydra-elisp (:color pink :hint nil)
  "
 ^Macro^                        ^Eval^                          ^Run^            ^Describe^       ^Find^
^^^^^^^^^^-----------------------------------------------------------------------------------------------------------------------
 _e_: expand       _n_: next      _d_: defun   _x_: exp             _r_: run ielm    _k_: function    _f_: variable    _q_: quit
 _c_: collapse     _p_: previous  _b_: buffer  _s_: last sexp                      _v_: variable    _y_: library
 _a_: collapse all              _i_: region  _t_: print last sexp                _l_: library     _j_: function
"
  ("e" macrostep-expand)
  ("c" macrostep-collapse)
  ("a" macrostep-collapse-all)
  ("n" macrostep-next-macro)
  ("p" macrostep-prev-macro)
  ("d" eval-defun :color blue)
  ("b" eval-buffer :color blue)
  ("i" eval-region)
  ("x" eval-expression :color blue)
  ("s" eval-last-sexp)
  ("t" eval-print-last-sexp)
  ("r" sk/ielm :color blue)
  ("k" describe-function :color blue)
  ("v" describe-variable :color blue)
  ("l" describe-library :color blue)
  ("j" find-function :color blue)
  ("f" find-variable :color blue)
  ("y" find-library :color blue)
  ("q" nil :color blue))

;;;;;;;;;;;;;;;;;
;;    C/C++    ;;
;;;;;;;;;;;;;;;;;

;; compile functions
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
;;;###autoload
(defun sk/compile-cpp-make ()
  "Compiles the file using the makefile in the current directory"
  (interactive)
  (compile
   (concat "make")))
;;;###autoload
(defun sk/compile-cpp-make-doc ()
  "Generates the documentation using the makefile in the current directory"
  (interactive)
  (compile
   (concat "make doc")))
;;;###autoload
(defun sk/compile-cpp-build ()
  "Compiles the file using the makefile in the build directory"
  (interactive)
  (compile
   (concat "make -C build")))
;;;###autoload
(defun sk/compile-cpp-build-doc ()
  "Generates the documentation using the makefile in the build directory"
  (interactive)
  (compile
   (concat "make -C build doc")))
;;;###autoload
(defun sk/compile-cpp-omp-math ()
  "Compiles the file with OpenMP and math libraries"
  (interactive)
  (compile
   (concat "g++ -Wall -fopenmp -lgsl -lcblas -llapack -O2 -g -std=c++11 " (buffer-file-name) " -o " (file-name-sans-extension buffer-file-name) ".out")))
;;;###autoload
(defun sk/compile-cpp-omp-simple ()
  "Compiles the file with OpenMP"
  (interactive)
  (compile
   (concat "g++ -Wall -fopenmp -g -std=c++11 " (buffer-file-name) " -o " (file-name-sans-extension buffer-file-name) ".out")))
;;;###autoload
(defun sk/compile-cpp-mpi-math ()
  "Compiles the file with MPI and math libraries"
  (interactive)
  (compile
   (concat "/usr/local/openmpi/bin/mpic++ -Wall -lgsl -lcblas -llapack -larmadillo -O2 -g -std=c++11 " (buffer-file-name) " -o " (file-name-sans-extension buffer-file-name) ".out")))
;;;###autoload
(defun sk/compile-cpp-mpi-simple ()
  "Compiles the file with MPI"
  (interactive)
  (compile
   (concat "/usr/local/openmpi/bin/c++ -Wall -g -std=c++11 " (buffer-file-name) " -o " (file-name-sans-extension buffer-file-name) ".out")))
;;;###autoload
(defun sk/compile-cpp-hybrid-math ()
  "Compiles the file with OpenMP, MPI and math libraries"
  (interactive)
  (compile
   (concat "/usr/local/openmpi/bin/c++ -Wall -fopenmp -lgsl -lcblas -llapack -larmadillo -O2 -g -std=c++11 " (buffer-file-name) " -o " (file-name-sans-extension buffer-file-name) ".out")))
;;;###autoload
(defun sk/compile-cpp-hybrid-simple ()
  "Compiles the file with OpenMP and MPI"
  (interactive)
  (compile
   (concat "/usr/local/openmpi/bin/c++ -Wall -fopenmp -g -std=c++11 " (buffer-file-name) " -o " (file-name-sans-extension buffer-file-name) ".out")))
;;;###autoload
(defun sk/compile-cpp-math ()
  "Compiles the file with math libraries"
  (interactive)
  (compile
   (concat "g++ -Wall -lgsl -lcblas -llapack -larmadillo -O2 -g -std=c++11 " (buffer-file-name) " -o " (file-name-sans-extension buffer-file-name) ".out")))
;;;###autoload
(defun sk/compile-cpp-simple ()
  "Compiles the file"
  (interactive)
  (compile
   (concat "g++ -Wall -g -std=c++11 " (buffer-file-name) " -o " (file-name-sans-extension buffer-file-name) ".out")))

;; c++ indexer and semantics
(use-package rtags
  :ensure t
  :bind* (("C-c y c" . company-rtags)
		  ("C-\\ c" . hydra-cpp/body)
		  ("C-\\ C-c" . hydra-cpp/body))
  :bind (:map c++-mode-map
			  ("M-\\" . hydra-cpp/body)
			  ("C-0" . rtags-find-symbol-at-point)
			  ("C-9" . rtags-print-symbol-info)
			  ("C-2" . rtags-find-references-at-point)
			  ("C-3" . rtags-find-virtuals-at-point)
			  ("C-4" . sk/compile-cpp-build)
			  ("C-5" . rtags-diagnostics)
			  ("C-6" . rtags-preprocess-file)
			  ("C-7" . rtags-print-dependencies)
			  ("C-8" . rtags-symbol-type))
  :init
  (setq rtags-autostart-diagnostics t)
  (setq rtags-completions-enabled t)
  :config
  ;; General C++ completion
  (use-package company-irony
	:ensure t
	:demand t
	:diminish irony-mode
	:bind (("C-c y i" . company-irony))
	:config
	(progn
	  (add-to-list 'company-backends 'company-irony)
	  (irony-mode)))
  ;; C++ header completion
  (use-package company-irony-c-headers
	:ensure t
	:demand t
	:bind (("C-c y h" . company-irony-c-headers))
	:config
	(progn
	  (add-to-list 'company-backends 'company-irony-c-headers))))

;; convert emacs into a c++ ide based on cmake
(use-package cmake-ide
  :ensure t
  :defer 2
  :config
  (cmake-ide-setup))

;; cmake files syntax
(use-package cmake-mode
  :ensure t
  :mode (("\\.cmake\\'" . cmake-mode)
		 ("CMakeLists.txt" . cmake-mode)))

;; hydra for cpp
(defhydra hydra-cpp (:color pink :hint nil)
  "
 ^Semantic Navigation^                                                        ^Compilation^
^^^^^^^^^^---------------------------------------------------------------------------------------------------------------------------------------------------------------
 _j_: symbol           _v_: virtuals      _]_: stack forward    _i_: diagnostics    _cb_: build       _cwc_: compile with math        _cwo_: openmp with math        _q_: quit
 _s_: find symbol      _d_: dependencies  _[_: stack back       _f_: fixit          _cm_: make        _coc_: compile without math     _coo_: openmp without math
 _r_: references       _k_: info          _n_: next match       _o_: preprocess	    _cd_: build doc   _cwb_: hybrid with math         _cwm_: mpi with math
 _a_: find references  _t_: type          _p_: prev match       _m_: rename	    _cl_: make doc    _cob_: hybrid without math      _com_: mpi without math
"
  ("j" rtags-find-symbol-at-point)
  ("s" rtags-find-symbol :color blue)
  ("r" rtags-find-references-at-point)
  ("a" rtags-find-references :color blue)
  ("v" rtags-find-virtuals-at-point)
  ("d" rtags-print-dependencies :color blue)
  ("k" rtags-print-symbol-info :color blue)
  ("t" rtags-symbol-type)
  ("e" rtags-print-enum-value-at-point)
  ("]" rtags-location-stack-forward)
  ("[" rtags-location-stack-backward)
  ("n" rtags-next-match)
  ("p" rtags-previous-match)
  ("i" rtags-diagnostics :color blue)
  ("f" rtags-fixit)
  ("o" rtags-preprocess-file)
  ("m" rtags-rename-symbol :color blue)
  ("cb" sk/compile-cpp-build :color blue)
  ("cm" sk/compile-cpp-make :color blue)
  ("cd" sk/compile-cpp-build-doc :color blue)
  ("cl" sk/compile-cpp-make-doc :color blue)
  ("cwc" sk/compile-cpp-math :color blue)
  ("coc" sk/compile-cpp-simple :color blue)
  ("cwo" sk/compile-cpp-omp-math :color blue)
  ("coo" sk/compile-cpp-omp-simple :color blue)
  ("cwm" sk/compile-cpp-mpi-math :color blue)
  ("com" sk/compile-cpp-mpi-simple :color blue)
  ("cwb" sk/compile-cpp-hybrid-math :color blue)
  ("cob" sk/compile-cpp-hybrid-simple :color blue)
  ("q" nil :color blue))

;;;;;;;;;;;;;;;;;;
;;    Python    ;;
;;;;;;;;;;;;;;;;;;

;; configure python itself
(use-package python
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :bind (:map python-mode-map
			  ("M-\\" . hydra-python/body)
			  ("C-0" . anaconda-mode-find-definitions)
			  ("C-9" . anaconda-mode-show-doc)
			  ("C-2" . python-shell-send-region)
			  ("C-3" . anaconda-mode-find-references)
			  ("C-4" . run-python)
			  ("C-5" . python-shell-send-buffer)
			  ("C-6" . python-shell-send-defun)
			  ("C-7" . python-shell-switch-to-shell)
			  ("C-8" . anaconda-mode-find-assignments))
  :bind* (("C-\\ p" . hydra-python/body)
		  ("C-\\ C-p" . hydra-python/body))
  :config
  (setq python-shell-interpreter "ipython"
		python-shell-interpreter-args "--simple-prompt -i")
  ;; (setq ansi-color-for-comint-mode t)
  ;; (setq python-shell-interpreter "python3")
  (setq python-shell-native-complete nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters "python3"))

;; python semantic analyzer
(use-package anaconda-mode
  :ensure t
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

;; Python auto completion
(use-package company-jedi
  :ensure t
  :demand t
  :bind* (("C-c y j" . company-jedi))
  :config
  (progn
	(add-to-list 'company-backends 'company-jedi)))

;; Format python code
(use-package py-yapf
  :ensure t
  :commands (py-yapf-buffer
			 py-yapf-region))

;; sphinx documentation
(use-package sphinx-doc
  :ensure t
  :diminish sphinx-doc-mode
  :commands (sphinx-doc)
  :config
  (sphinx-doc-mode))

;; virtualenv wrapper
(use-package virtualenvwrapper
  :ensure t
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
	:commands (pyenv-mode-set
			   pyenv-mode-unset)
	:config
	(pyenv-mode))
  (setq eshell-prompt-function
		(lambda ()
		  (concat venv-current-name " $ ")))
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell))
;; hydra for python virtualenv
(defhydra hydra-py-venv (:color blue :hint nil)
  "
 ^Virtualenv^                               ^Pyenv^
^^^^^^^^^^-----------------------------------------------------------
 _w_: workon      _l_: location    _m_: make    _s_: set    _q_: quit
 _d_: deactivate  _v_: list venv   _r_: remove  _u_: unset  _h_: head
 _c_: change dir  _p_: copy
"
  ("w" venv-workon)
  ("d" venv-deactivate)
  ("l" venv-set-location)
  ("v" venv-lsvirtualenv)
  ("c" venv-cdvirtualenv)
  ("m" venv-mkvirtualenv)
  ("r" venv-rmvirtualenv)
  ("p" venv-cpvirtualenv)
  ("s" pyenv-set)
  ("u" pyenv-unset)
  ("h" hydra-python/body :exit t)
  ("q" nil :color blue))

;; python testing
(use-package pytest
  :ensure t
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
;; hydra for python testing
(defhydra hydra-py-test (:color blue :hint nil)
  "
 ^Tests^                ^PDB^
^^^^^^^^^^------------------------------------------------
 _o_: one  _d_: dir       _po_: one  _pd_: dir      _q_: quit
 _a_: all  _f_: failed    _pa_: all  _pf_: failed   _h_: head
 _m_: module            _pm_: module
"
  ("o" pytest-one)
  ("a" pytest-all)
  ("d" pytest-directory)
  ("f" pytest-failed)
  ("m" pytest-module)
  ("po" pytest-pdb-one)
  ("pa" pytest-pdb-all)
  ("pd" pytest-pdb-directory)
  ("pf" pytest-pdb-failed)
  ("pm" pytest-pdb-module)
  ("h" hydra-python/body :exit t)
  ("q" nil :color blue))

;; hydra for python
(defhydra hydra-python (:pre (anaconda-mode) :color pink :hint nil)
  "
 ^Semantic Navigation^         ^Eval^         ^REPL^        ^Miscellaneous^
^^^^^^^^^^---------------------------------------------------------------------------------------------------------
 _j_: definition    _k_: doc     _d_: def       _r_: run      _v_: virtualenv    _x_: sphinx        _q_: quit
 _a_: assignments   _[_: back    _b_: buffer    _]_: switch   _t_: tests         _y_: yapf region
 _e_: references               _i_: region                                 _f_: yapf buffer
"
  ("j" anaconda-mode-find-definitions)
  ("a" anaconda-mode-find-assignments)
  ("e" anaconda-mode-find-references)
  ("k" anaconda-mode-show-doc :color blue)
  ("[" anaconda-mode-go-back)
  ("d" python-shell-send-defun)
  ("b" python-shell-send-buffer :color blue)
  ("i" python-shell-send-region)
  ("r" run-python :color blue)
  ("]" python-shell-switch-to-shell :color blue)
  ("v" hydra-py-venv/body :exit t)
  ("t" hydra-py-test/body :exit t)
  ("x" sphinx-doc :color blue)
  ("y" py-yapf-region)
  ("f" py-yapf-buffer :color blue)
  ("q" nil :color blue))

;; which key replacements
(which-key-add-major-mode-key-based-replacements 'python-mode
  "C-\\ v" "virtualenv"
  "C-\\ C-v" "virtualenv"
  "C-\\ t" "test"
  "C-\\ C-t" "test"
  "C-\\ t p" "pdb"
  "C-\\ C-t C-p" "pdb")

;;;;;;;;;;;;;;;;;
;;    Stats    ;;
;;;;;;;;;;;;;;;;;

;; Emacs Speaks Statistics
(use-package ess
  :ensure t
  :mode (("\\.r\\'" . R-mode)
		 ("\\.R\\'" . R-mode)
		 ("\\.jl\\'" . julia-mode))
  :commands (ess-r-devtools-load-package
			 ess-display-package-index
			 ess-r-devtools-document-package
			 ess-install-library
			 ess-install.packages
			 ess-r-devtools-install-package
			 ess-r-package-set-package
			 ess-r-devtools-unload-package
			 ess-r-devtools-test-package
			 ess-r-devtools-check-package
			 ess-r-devtools-install-github
			 ess-r-devtools-revdep-check-cmd
			 ess-r-devtools-revdep-check-package
			 R
			 julia
			 ess-switch-to-ESS
			 ess-eval-region-or-function-or-paragraph-and-step
			 ess-eval-buffer
			 ess-eval-buffer-from-beg-to-here
			 ess-eval-buffer-from-here-to-end
			 ess-eval-chunk-and-step
			 ess-show-call-stack
			 ess-load-file
			 ess-r-load-file-namespaced
			 ess-watch
			 ess-eval-region-or-line-and-step
			 ess-load-library
			 ess-help
			 ess-describe-object-at-point
			 ess-display-demos
			 ess-display-vignettes
			 ess-help-web-search
			 ess-display-help-apropos)
  :bind (:map ess-mode-map
			  ("M-\\" . hydra-stats/body)
			  ("C-2" . ess-eval-region-or-line-and-step)
			  ("C-3" . ess-eval-chunk-and-step)
			  ("C-4" . sk/R)
			  ("C-5" . ess-eval-buffer)
			  ("C-6" . ess-eval-function-or-paragraph-and-step)
			  ("C-7" . ess-swith-to-ESS)
			  ("C-8" . ess-describe-object-at-point)
			  ("C-9" . ess-help)
			  ("C-0" . ess-dump-object-into-edit-buffer))
  :bind* (("C-\\ s" . hydra-stats/body)
		  ("C-\\ C-s" . hydra-stats/body))
  :init
  (setq ess-use-ido nil)
  :config
  (require 'ess-site)
  (defun sk/R ()
	"open R REPL in other window"
	(interactive)
	(split-window-horizontally)
	(other-window 1)
	(R))
  (defun sk/julia ()
	"open julia REPL in other window"
	(interactive)
	(split-window-horizontally)
	(other-window 1)
	(julia)))

;; hydra for stats package
(defhydra hydra-stats-packages (:color blue :hint nil)
  "
 ^Package^      ^Devtools^                             ^Revdep^
^^^^^^^^^^------------------------------------------------------------------------------
 _x_: index     _i_: install    _s_: set     _w_: github   _c_: check cmd     _q_: quit
 _l_: library   _a_: load       _u_: unload  _e_: check    _k_: check package _h_: head
 _p_: install   _d_: document   _t_: test
"
  ("x" ess-display-package-index)
  ("l" ess-install-library)
  ("p" ess-install.packages)
  ("i" ess-r-devtools-install-package)
  ("a" ess-r-devtools-load-package)
  ("d" ess-r-devtools-document-package)
  ("s" ess-r-package-set-package)
  ("u" ess-r-devtools-unload-package)
  ("t" ess-r-devtools-test-package)
  ("w" ess-r-devtools-install-github)
  ("e" ess-r-devtools-check-package)
  ("c" ess-r-devtools-revdep-check-cmd)
  ("k" ess-r-devtools-revdep-check-package)
  ("h" hydra-stats/body :exit t)
  ("q" nil :color blue))

;; hydra for stats roxy
(defhydra hydra-stats-roxy (:color pink :hint nil)
  "
 ^Roxy Entry^                     ^Preview^
^^^^^^^^^^------------------------------------------------
 _n_: next    _t_: toggle region    _h_: html    _q_: quit
 _p_: prev    _a_: show all         _x_: text    _h_: head
 _u_: update  _c_: cycle            _r_: Rd
"
  ("n" ess-roxy-next-entry)
  ("p" ess-roxy-previous-entry)
  ("u" ess-roxy-update-entry)
  ("t" ess-roxy-toggle-roxy-region)
  ("a" ess-roxy-show-all :color blue)
  ("c" ess-roxy-cycle-example)
  ("p" ess-roxy-preview-HTML :color blue)
  ("x" ess-roxy-preview-text :color blue)
  ("r" ess-roxy-preview-Rd :color blue)
  ("h" hydra-stats/body :exit t)
  ("q" nil :color blue))

;; hydra for stats debug
(defhydra hydra-stats-debug (:color pink :hint nil)
  "
 ^Breakpoint^                               ^Debug^
^^^^^^^^^^------------------------------------------------------------------------------------------------------------
 _s_: set       _l_: logger       _n_: next     _w_: watch      _b_: tracebug help  _i_: goto input     _q_: quit
 _u_: kill      _c_: conditional  _p_: prev     _a_: call stack _f_: flag           _r_: error activate _h_: head
 _k_: kill all  _v_: eval env     _t_: toggle   _e_: tracebug   _d_: unflag         _o_: traceback
"
  ("s" ess-bp-set)
  ("u" ess-bp-kill)
  ("k" ess-bp-kill-all)
  ("l" ess-bp-set-logger)
  ("c" ess-bp-set-conditional)
  ("v" ess-r-set-evaluation-env :color blue)
  ("n" ess-bp-next)
  ("p" ess-bp-previous)
  ("t" ess-bp-toggle-state)
  ("w" ess-watch :color blue)
  ("a" ess-show-call-stack :color blue)
  ("e" ess-toggle-tracebug :color blue)
  ("b" ess-tracebug-show-help :color blue)
  ("f" ess-debug-flag-for-debugging)
  ("d" ess-debug-unflag-for-debugging)
  ("i" ess-debug-goto-input-event-marker :color blue)
  ("r" ess-debug-toggle-error-action)
  ("o" ess-show-traceback :color blue)
  ("h" hydra-stats/body :exit t)
  ("q" nil :color blue))

;; hydra for stats
(defhydra hydra-stats (:color pink :hint nil)
  "
 ^Eval/REPL^                                                    ^Describe/Doc^                  ^Misc^
^^^^^^^^^^-----------------------------------------------------------------------------------------------------------------------
 _r_: R repl          _d_: func/para     _f_: load file             _k_: help       _w_: web search   _p_: packages    _q_: quit
 _j_: julia repl      _i_: region/line   _l_: load library          _o_: obj at pt  _a_: apropos      _x_: execute
 _]_: switch to repl  _b_: buffer        _n_: load file namespace   _e_: demos      _z_: dump obj     _t_: style
 _c_: chunk           _<_: to beg        _>_: to end                _v_: vignettes  _u_: debug        _y_: roxy
"
  ("r" sk/R :color blue)
  ("j" sk/julia :color blue)
  ("]" ess-switch-to-ESS :color blue)
  ("d" ess-eval-function-or-paragraph-and-step)
  ("i" ess-eval-region-or-line-and-step)
  ("b" ess-eval-buffer)
  ("<" ess-eval-buffer-from-beg-to-here)
  (">" ess-eval-buffer-from-here-to-end)
  ("c" ess-eval-chunk-and-step)
  ("f" ess-load-file)
  ("l" ess-load-library)
  ("n" ess-r-load-file-namespaced)
  ("z" ess-dump-object-into-edit-buffer :color blue)
  ("k" ess-help :color blue)
  ("o" ess-describe-object-at-point :color blue)
  ("e" ess-display-demos :color blue)
  ("v" ess-display-vignettes :color blue)
  ("w" ess-help-web-search :color blue)
  ("a" ess-display-help-apropos :color blue)
  ("x" ess-execute :color blue)
  ("t" ess-set-style :color blue)
  ("u" hydra-stats-debug/body :exit t)
  ("p" hydra-stats-packages/body :exit t)
  ("y" hydra-stats-roxy/body :exit t)
  ("q" nil :color blue))

;; which key replacements
(which-key-add-major-mode-key-based-replacements 'ess-mode
  "C-\\ u" "debug"
  "C-\\ C-u" "debug"
  "C-\\ p" "package"
  "C-\\ C-p" "package"
  "C-\\ y" "roxy"
  "C-\\ C-y" "roxy")

;;;;;;;;;;;;;;;;;;
;;    MATLAB    ;;
;;;;;;;;;;;;;;;;;;

;; install matlab from OCIO first
(use-package matlab-mode
  :ensure t
  :mode ("\\.m\\'" . matlab-mode)
  :diminish mlint-minor-mode
  :bind (:map matlab-shell-mode-map
			  ("C-c C-c" . term-interrupt-subjob))
  :bind* (("C-\\ m" . hydra-matlab/body)
		  ("C-\\ C-m" . hydra-matlab/body))
  :init
  (setq matlab-shell-command "/Applications/MATLAB_R2016a.app/bin/matlab"
		matlab-indent-function t)
  (setq matlab-mode-install-path '("~/Dropbox/PhD/codes/ttmat"
								   "/Applications/MATLAB_R2016a.app/toolbox"))
  (eval-after-load 'matlab
	'(add-to-list 'matlab-shell-command-switches "-nodesktop -nosplash"))
  :config
  (require 'matlab-load)
  (require 'mlint))
;; better repl
(defun sk/matlab-shell ()
  "open matlab REPL in a split window"
  (interactive)
  (split-window-horizontally)
  (other-window 1)
  (matlab-shell))
;; matlab mode bindings
(defun sk/matlab-bindings ()
  "bindings for matlab-mode becuase it doesn't get evaluated with use-package for some reason"
  (bind-keys
   ("M-\\" . hydra-matlab/body)
   ("C-0" . matlab-find-file-on-path)
   ("C-9" . matlab-shell-describe-command)
   ("C-2" . matlab-shell-run-region-or-line)
   ("C-3" . mlint-prev-buffer)
   ("C-4" . sk/matlab-shell)
   ("C-5" . matlab-shell-save-and-go)
   ("C-6" . matlab-shell-describe-variable)
   ("C-7" . matlab-show-matlab-shell-buffer)
   ("C-8" . mlint-next-buffer)))
(add-hook 'matlab-mode-hook #'sk/matlab-bindings)
;; setup matlab properly
(defun sk/matlab-setup ()
  "setup matlab mode properly"
  ;; mlint settings
  (setq-default matlab-show-mlint-warnings t)
  (setq-default mlint-verbose t)
  (setq-default mlint-programs '("/Applications/MATLAB_R2016a.app/bin/maci64/mlint"))
  (mlint-minor-mode 1)
  ;; switch on god mode by default
  (god-local-mode))
(add-hook 'matlab-mode-hook #'sk/matlab-setup)
(defun sk/diminish-mlint-minor ()
  (interactive)
  (diminish 'mlint-minor-mode ""))
(add-hook 'mlint-minor-mode-hook 'sk/diminish-mlint-minor)

;; hydra for matlab
(defhydra hydra-matlab (:color pink :hint nil)
  "
 ^Describe^                      ^Run^                ^REPL^
^^^^^^^^^^-------------------------------------------------------------------
 _k_: command    _t_: topic        _i_: region/line     _r_: matlab    _q_: quit
 _v_: variable   _j_: file on path _f_: cell            _]_: switch
 _l_: line                       _c_: command         _b_: buffer
"
  ("k" matlab-shell-describe-command)
  ("v" matlab-shell-describe-variable)
  ("l" matlab-show-line-info)
  ("t" matlab-shell-topic-browser :color blue)
  ("j" matlab-find-file-on-path :color blue)
  ("i" matlab-shell-run-region-or-line)
  ("f" matlab-shell-run-cell)
  ("b" matlab-shell-save-and-go)
  ("c" matlab-shell-run-command :color blue)
  ("r" sk/matlab-shell :color blue)
  ("]" matlab-show-matlab-shell-buffer :color blue)
  ("q" nil :color blue))

;;;;;;;;;;;;;;;
;;    Web    ;;
;;;;;;;;;;;;;;;

;; web mode for html files
(use-package web-mode
  :ensure t
  :mode ("\\.html\\'" . web-mode)
  :commands (httpd-start
			 httpd-stop)
  :bind (:map web-mode-map
			  ("M-\\" . hydra-web/body)
			  ("C-0" . httpd-start)
			  ("C-9" . httpd-stop)
			  ("C-2" . skewer-html-mode)
			  ("C-3" . skewer-css-mode)
			  ("C-4" . skewer-repl)
			  ("C-5" . skewer-mode)
			  ("C-6" . web-beautify-html-buffer)
			  ("C-7" . web-beautify-css-buffer)
			  ("C-8" . web-beautify-js-buffer))
  :bind* (("C-\\ w" . hydra-web/body)
		  ("C-\\ C-w" . hydra-web/body))
  :config
  ;; HTML completion
  (use-package company-web
	:ensure t
	:demand t
	:bind (("C-c y w" . company-web-html))
	:config
	(progn
	  (add-to-list 'company-backends 'company-web-html))))
;; impatient - refresh HTML immediately
(use-package impatient-mode
  :ensure t
  :diminish (impatient-mode . " ι")
  :commands (impatient-mode))
;; js evaluation
(use-package skewer-mode
  :ensure t
  :diminish skewer-mode
  :commands (skewer-mode
			 skewer-html-mode
			 skewer-css-mode
			 run-skewer
			 skewer-repl
			 list-skewer-clients
			 skewer-eval-defun
			 skewer-eval-last-expression
			 skewer-eval-print-last-expression
			 skewer-load-buffer
			 skewer-run-phantomjs
			 skewer-phantomjs-kill
			 skewer-bower-load
			 skewer-bower-refresh))
;; beautify content
(use-package web-beautify
  :ensure t
  :commands (web-beautify-html
			 web-beautify-html-buffer
			 web-beautify-css
			 web-beautify-css-buffer
			 web-beautify-js
			 web-beautify-js-buffer))

;; hydra for web
(defhydra hydra-web (:color pink :hint nil)
  "
 ^Server^          ^Skewer^                                                                               ^Format^
^^^^^^^^^^-------------------------------------------------------------------------------------------------------------------------------------------------------
 _w_: httpd start  _mm_: skewer mode        _r_: skewer repl     _d_: eval defun        _j_: phantomjs start     _fh_: html region    _fj_: js region    _q_: quit
 _z_: httpd stop   _mh_: skewer html mode   _u_: run skewer      _l_: load buffer       _p_: phantomjs stop      _fl_: html buffer    _fb_: js buffer
 _n_: impatient    _mc_: skewer css mode    _i_: list clients    _s_: last sexp         _b_: bower load          _fc_: css region
				 _t_: print last sexp     _e_: bower refresh                                               _fs_: css buffer
"
  ("w" httpd-start)
  ("z" httpd-stop)
  ("n" impatient-mode)
  ("mm" skewer-mode :color blue)
  ("mh" skewer-html-mode :color blue)
  ("mc" skewer-css-mode :color blue)
  ("r" skewer-repl :color blue)
  ("u" run-skewer :color blue)
  ("i" list-skewer-clients)
  ("l" skewer-load-buffer)
  ("d" skewer-eval-defun)
  ("s" skewer-eval-last-expression)
  ("t" skewer-eval-print-last-expression)
  ("j" skewer-run-phantomjs)
  ("p" skewer-phantomjs-kill)
  ("b" skewer-bower-load)
  ("e" skewer-bower-refresh)
  ("fh" web-beautify-html)
  ("fl" web-beautify-html-buffer)
  ("fc" web-beautify-css)
  ("fs" web-beautify-css-buffer)
  ("fj" web-beautify-js)
  ("fb" web-beautify-js-buffer)
  ("q" nil :color blue))

;; js3 mode for javascript
(use-package js3-mode
  :ensure t
  :mode ("\\.js\\'" . js3-mode)
  :bind (:map js3-mode-map
			  ("M-\\" . hydra-javascript/body)
			  ("C-0" . tern-find-definition)
			  ("C-9" . tern-get-docs)
			  ("C-2" . nodejs-repl-send-region)
			  ("C-3" . nodejs-repl-send-last-sexp)
			  ("C-4" . nodejs-repl)
			  ("C-5" . nodejs-repl-send-buffer)
			  ("C-6" . nodejs-repl-load-file)
			  ("C-7" . nodejs-repl-switch-to-repl)
			  ("C-8" . tern-get-type))
  :bind (:map js-mode-map
			  ("M-\\" . hydra-javascript/body)
			  ("C-0" . tern-find-definition)
			  ("C-9" . tern-get-docs)
			  ("C-2" . nodejs-repl-send-region)
			  ("C-3" . nodejs-repl-send-last-sexp)
			  ("C-4" . nodejs-repl)
			  ("C-5" . nodejs-repl-send-buffer)
			  ("C-6" . nodejs-repl-load-file)
			  ("C-7" . nodejs-repl-switch-to-repl)
			  ("C-8" . tern-get-type))
  :bind* (("C-\\ j" . hydra-javascript/body)
		  ("C-\\ C-j" . hydra-javascript/body)))
;; JS semantic navigation
(use-package tern
  :ensure t
  :diminish tern-mode
  :commands (tern-find-definition
			 tern-get-docs
			 tern-get-type
			 tern-use-server
			 tern-highlight-refs
			 tern-rename-variable)
  :config
  ;; Tern for JS
  (use-package company-tern
	:ensure t
	:demand t
	:bind (("C-c y t" . company-tern))
	:init
	(setq company-tern-property-marker "")
	(setq company-tern-meta-as-single-line t)
	:config
	(progn
	  (add-to-list 'company-backends 'company-tern)))
  (progn
	(add-hook 'js-mode-hook '(lambda () (tern-mode t)))
	(add-hook 'js3-mode-hook '(lambda () (tern-mode t)))))
;; format js code
(use-package jsfmt
  :ensure t
  :commands (jsfmt
			 jsfmt-ast))
;; nodejs REPL for JS evaluation
(use-package nodejs-repl
  :ensure t
  :commands (nodejs-repl
			 nodejs-repl-switch-to-repl
			 nodejs-repl-send-last-sexp
			 nodejs-repl-send-buffer
			 nodejs-repl-send-region
			 nodejs-repl-execute
			 nodejs-repl-load-file)
  :config
  (use-package nvm
	:ensure t
	:commands (nvm-use
			   nvm-use-for)))

;; hydra for javascript
(defhydra hydra-javascript (:color pink :hint nil)
  "
 ^Semantic Navigation^                 ^REPL/Eval^                        ^Misc^
^^^^^^^^^^--------------------------------------------------------------------------------------------------
 _t_: type            _j_: definition    _r_: node repl       _s_: last sexp  _v_: nvm use        _q_: quit
 _u_: use server      _k_: docs          _]_: switch to repl  _i_: region     _n_: nvm use for
 _m_: highlight refs                   _l_: load file       _b_: buffer     _f_: format
 _e_: rename                           _x_: execute                       _a_: ast format
"
  ("t" tern-get-type)
  ("u" tern-use-server)
  ("m" tern-highlight-refs)
  ("e" tern-rename-variable)
  ("j" tern-find-definition)
  ("k" tern-get-docs)
  ("r" nodejs-repl :color blue)
  ("]" nodejs-repl-switch-to-repl :color blue)
  ("s" nodejs-repl-send-last-sexp)
  ("i" nodejs-repl-send-region)
  ("b" nodejs-repl-send-buffer :color blue)
  ("x" nodejs-repl-execute :color blue)
  ("l" nodejs-repl-load-file :color blue)
  ("v" nvm-use :color blue)
  ("n" nvm-use-for :color blue)
  ("f" jsfmt)
  ("a" jsfmt-ast)
  ("q" nil :color blue))

;; coffee script syntax highlighting
(use-package coffee-mode
  :ensure t
  :mode "\\.coffee\\'")

;; SCSS syntax
(use-package scss-mode
  :ensure t
  :mode "\\.scss\\'")

;; json syntax
(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

;; Nginx syntax
(use-package nginx-mode
  :ensure t
  :commands (nginx-mode))

;; VimL syntax
(use-package vimrc-mode
  :ensure t
  :mode (("\\.vimrc\\'" . vimrc-mode)
		 ("\\.vim\\'" . vimrc-mode)
		 ("\\vimrc" . vimrc-mode))
  :commands (vimrc-mode))

;; emmet for easy HTML
(use-package emmet-mode
  :ensure t
  :diminish (emmet-mode . " ε")
  :bind* (("C-c i" . emmet-expand-line)
		  ("C-c ]" . emmet-next-edit-point)
		  ("C-c [" . emmet-prev-edit-point)))

;;;;;;;;;;;;;;;
;;    Lua    ;;
;;;;;;;;;;;;;;;

(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'"
  :bind* (("C-\\ u" . hydra-lua/body)
		  ("C-\\ C-u" . hydra-lua/body))
  :bind (:map lua-mode-map
			  ("M-\\" . hydra-lua/body)
			  ("C-0" . ggtags-find-tag-dwim)
			  ("C-9" . lua-search-documentation)
			  ("C-2" . lua-send-region)
			  ("C-3" . lua-send-line)
			  ("C-4" . sk/lua)
			  ("C-5" . lua-send-buffer)
			  ("C-6" . lua-send-defun)
			  ("C-7" . lua-show-process-buffer)
			  ("C-8" . run-lua))
  :config
  (defun sk/lua ()
	"open lua REPL in a split window"
	(interactive)
	(split-window-horizontally)
	(lua-start-process)
	(other-window 1)))

;; hydra for lua
(defhydra hydra-lua (:color pink :hint nil)
  "
 ^Eval^
^^^^^^^^^^--------------------------------------------------------
 _r_: run lua        _i_: send region   _d_: send defun    _q_: quit
 _]_: switch to lua  _b_: send buffer   _l_: send line
 _k_: lua-search-documentation
"
  ("r" sk/lua :color blue)
  ("]" lua-show-process-buffer :color blue)
  ("k" lua-search-documentation :color blue)
  ("i" lua-send-region)
  ("b" lua-send-buffer :color blue)
  ("d" lua-send-defun :color blue)
  ("l" lua-send-current-line :color blue)
  ("q" nil :color blue))

;;;;;;;;;;;;;;;
;;    SML    ;;
;;;;;;;;;;;;;;;

(use-package sml-mode
  :ensure t
  :mode "\\.sml\\'"
  :init
  (setq sml-program-name "sml")
  :bind (:map sml-mode-map
			  ("M-\\" . hydra-sml/body)
			  ("C-0" . ggtags-find-tag-dwim)
			  ("C-9" . woman)
			  ("C-2" . sml-prog-proc-send-region)
			  ("C-3" . sml-prog-proc-compile)
			  ("C-4" . sml-prog-proc-switch-to)
			  ("C-5" . sml-prog-proc-send-buffer)
			  ("C-6" . sml-prog-proc-load-file)
			  ("C-7" . sml-prog-proc-switch-to)
			  ("C-8" . run-sml))
  :bind* (("C-\\ l" . hydra-sml/body)
		  ("C-\\ C-l" . hydra-sml/body)))

;; hydra for sml
(defhydra hydra-sml (:color pink :hint nil)
  "
 ^Eval^
^^^^^^^^^^--------------------------------------------------------
 _r_: run sml        _i_: send region   _c_: compile    _q_: quit
 _]_: switch to sml  _b_: send buffer   _l_: load file
"
  ("r" sml-prog-proc-switch-to :color blue)
  ("]" sml-prog-proc-switch-to :color blue)
  ("i" sml-prog-proc-send-region)
  ("b" sml-prog-proc-send-buffer :color blue)
  ("c" sml-prog-proc-compile :color blue)
  ("l" sml-prog-proc-load-file :color blue)
  ("q" nil :color blue))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Racket/Scheme    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package geiser
  :ensure t
  :ensure racket-mode
  :mode (("\\.scm\\'" . scheme-mode)
		 ("\\.rkt\\'" . racket-mode))
  :diminish geiser-mode
  :commands (run-geiser
			 geiser-mode-switch-to-repl
			 geiser-eval-definition
			 geiser-eval-buffer
			 geiser-eval-last-sexp
			 geiser-eval-region
			 geiser-expand-definition
			 geiser-expand-last-sexp
			 geiser-compile-current-buffer
			 geiser-load-file
			 geiser-edit-symbol-at-point
			 geiser-edit-module
			 geiser-doc-symbol-at-point
			 geiser-doc-module
			 geiser-doc-lookup-manual
			 geiser-xref-callers
			 geiser-xref-callees)
  :bind* (("C-\\ k" . hydra-racket-scheme/body)
		  ("C-\\ C-k" . hydra-racket-scheme/body))
  :bind (:map scheme-mode-map
			  ("M-\\" . hydra-racket-scheme/body)
			  ("C-0" . geiser-edit-symbol-at-point)
			  ("C-9" . geiser-doc-symbol-at-point)
			  ("C-2" . geiser-eval-region)
			  ("C-3" . geiser-eval-last-sexp)
			  ("C-4" . run-geiser)
			  ("C-5" . geiser-eval-buffer)
			  ("C-6" . geiser-eval-definition)
			  ("C-7" . geiser-mode-switch-to-repl)
			  ("C-8" . geiser-load-file))
  :bind (:map racket-mode-map
			  ("M-\\" . hydra-racket-scheme/body)
			  ("C-0" . geiser-edit-symbol-at-point)
			  ("C-9" . geiser-doc-symbol-at-point)
			  ("C-2" . geiser-eval-region)
			  ("C-3" . geiser-eval-last-sexp)
			  ("C-4" . run-geiser)
			  ("C-5" . geiser-eval-buffer)
			  ("C-6" . geiser-eval-definition)
			  ("C-7" . geiser-mode-switch-to-repl)
			  ("C-8" . geiser-load-file)))

;; hydra for racket/scheme
(defhydra hydra-racket-scheme (:color pink :hint nil)
  "
 ^Semantic Navigation^                                         ^Eval^
^^^^^^^^^^-------------------------------------------------------------------------------------------------------------------------------------------------------
 _j_: symbol at point     _e_: edit module    _c_: xref callers    _r_: run geiser      _d_: eval defun     _u_: expand defun     _p_: compile    _q_: quit
 _k_: doc symbol at pt    _m_: doc module     _v_: xref callees    _]_: switch to repl  _b_: eval buffer    _s_: eval last sexp   _f_: load
 _l_: insert lambda       _o_: doc manual     _t_: add to path                        _i_: eval region    _x_: expand last sexp
"
  ("j" geiser-edit-symbol-at-point)
  ("k" geiser-doc-symbol-at-point)
  ("r" run-geiser :color blue)
  ("]" geiser-mode-switch-to-repl :color blue)
  ("i" geiser-eval-region)
  ("d" geiser-eval-definition)
  ("u" geiser-expand-definition)
  ("b" geiser-eval-buffer :color blue)
  ("s" geiser-eval-last-sexp)
  ("x" geiser-expand-last-sexp)
  ("p" geiser-compile-current-buffer :color blue)
  ("f" geiser-load-file :color blue)
  ("l" geiser-insert-lambda :color blue)
  ("e" geiser-edit-module :color blue)
  ("t" geiser-add-to-load-path :color blue)
  ("m" geiser-doc-module :color blue)
  ("o" geiser-doc-lookup-manual :color blue)
  ("c" geiser-xref-callers)
  ("v" geiser-xref-callees)
  ("q" nil :color blue))

;;;;;;;;;;;;;;;;
;;    Ruby    ;;
;;;;;;;;;;;;;;;;

(use-package inf-ruby
  :ensure t
  :mode ("\\.rb" . ruby-mode)
  :commands (inf-ruby
			 ruby-switch-to-inf
			 ruby-send-definition
			 ruby-send-definition-and-go
			 ruby-send-region
			 ruby-send-region-and-go)
  :bind (:map ruby-mode-map
			  ("M-\\" . hydra-ruby/body)
			  ("C-0" . robe-jump)
			  ("C-9" . robe-show-doc)
			  ("C-2" . ruby-send-region)
			  ("C-3" . robe-start)
			  ("C-4" . inf-ruby)
			  ("C-5" . ruby-send-definition)
			  ("C-6" . robe-call-at-point)
			  ("C-7" . ruby-switch-to-inf)
			  ("C-8" . hydra-projectile-rails/body))
  :bind* (("C-\\ r" . hydra-ruby/body)
		  ("C-\\ C-r" . hydra-ruby/body))
  :config
  (add-hook 'ruby-mode-hook 'inf-ruby-mode)
  ;; rails coding
  (use-package projectile-rails
	:ensure t
	:demand t
	:diminish projectile-rails-mode
	:commands (hydra-projectile-rails/body)
	:init
	(setq projectile-rails-expand-snippet nil)
	:config
	(add-hook 'projectile-mode-hook 'projectile-rails-on)
	(projectile-rails-mode)))

;; ruby static code analyzer
(use-package rubocop
  :ensure t
  :commands (rubocop-check-current-file
			 rubocop-autocorrect-current-file
			 rubocop-check-directory
			 rubocop-autocorrect-directory
			 rubocop-check-project
			 rubocop-autocorrect-project)
  :config
  (rubocop-mode t))

;; semantic navigation
(use-package robe
  :ensure t
  :commands (robe-method-def
			 robe-jump-to-module
			 robe-jump
			 robe-doc
			 robe-call-at-point
			 robe-start)
  :config
  (eval-after-load 'company
	'(push 'company-robe company-backends))
  (add-hook 'ruby-mode-hook 'robe-mode))

;; manage ruby environments
(use-package bundler
  :ensure t
  :commands (bundle-show
			 bundle-gemfile
			 bundle-exec
			 bundle-version
			 bundle-outdated
			 bundle-install
			 bundle-check
			 bundle-open
			 bundle-update
			 bundle-console))
;; hydra for bundler
(defhydra hydra-ruby-bundler (:color blue :hint nil)
  "
 ^Bundle^
^^^^^^^^^^---------------------------------------------------------
 _s_: show    _v_: version    _k_: check    _c_: console  _q_: quit
 _g_: gemfile _d_: outdated   _o_: open                 _h_: head
 _e_: exec    _i_: install    _u_: update
"
  ("s" bundle-show)
  ("g" bundle-gemfile)
  ("e" bundle-exec)
  ("v" bundle-version)
  ("d" bundle-outdated)
  ("i" bundle-install)
  ("k" bundle-check)
  ("o" bundle-open)
  ("u" bundle-update)
  ("c" bundle-console)
  ("h" hydra-ruby/body :exit t)
  ("q" nil :color blue))

;; ruby testing
(use-package ruby-test-mode
  :ensure t
  :diminish ruby-test-mode
  :commands (ruby-test-run
			 ruby-test-run-at-point
			 ruby-test-toggle-implementation-and-specification)
  :config
  (ruby-test-mode))
;; rspec for tests
(use-package rspec-mode
  :ensure t
  :diminish rspec-mode
  :commands (rspec-verify
			 rspec-verify-all
			 rspec-verify-matching
			 rspec-verify-method
			 rspec-verify-single
			 rspec-rerun
			 rspec-continue
			 rspec-run-last-failed
			 rspec-find-spec-or-target-find-example-other-window
			 rspec-find-spec-or-target-other-window
			 rspec-toggle-spec-and-target-find-example
			 rspec-toggle-spec-and-target
			 rspec-toggle-example-pendingness)
  :config
  (rspec-mode))
;; hydra for tests
(defhydra hydra-ruby-tests (:color blue :hint nil)
  "
 ^Tests^         ^Spec^
^^^^^^^^^^---------------------------------------------------------
 _t_: run          _v_: verify         _t_: verify matching  _f_: last failed  _a_: target               _p_: pending    _q_: quit
 _i_: run at point _e_: verify all     _r_: rerun            _l_: single       _n_: target other window                _h_: head
 _s_: toggle spec  _m_: verify method  _c_: continue         _x_: example      _o_: example other window
"
  ("t" ruby-test-run)
  ("i" ruby-test-run-at-point)
  ("s" ruby-test-toggle-implementation-and-specification)
  ("v" rspec-verify)
  ("e" rspec-verify-all)
  ("m" rspec-verify-method)
  ("t" rspec-verify-matching)
  ("r" rspec-rerun)
  ("c" rspec-continue)
  ("f" rspec-run-last-failed)
  ("l" rspec-verify-single)
  ("x" rspec-toggle-spec-and-target-find-example)
  ("o" rspec-find-spec-or-target-find-example-other-window)
  ("a" rspec-toggle-spec-and-target)
  ("n" rspec-find-spec-or-target-other-window)
  ("p" rspec-toggle-example-pendingness)
  ("h" hydra-ruby/body :exit t)
  ("q" nil :color blue))

;; hydra for ruby
(defhydra hydra-ruby (:color pink :hint nil)
  "
 ^Semantic Navigation^       ^Static analysis^                                 ^Eval^                                           ^Misc^
^^^^^^^^^^-------------------------------------------------------------------------------------------------------------------------------------------------------
 _m_: method def   _k_: doc    _f_: check file         _p_: check project          _r_: ruby repl        _d_: send defun              _t_: tests     _q_:quit
 _u_: module def   _s_: robe   _af_: autcorrect file   _ap_: autocorrect project   _]_: switch to repl   _}_: send defun and go       _b_: bundler
 _c_: call at pt             _e_: check dir                                                        _i_: send region             _l_: rails
 _j_: jump                   _ae_: autocorrect dir                                                 _>_: send region and go
"
  ("m" robe-method-def)
  ("u" robe-module-def)
  ("c" robe-call-at-point)
  ("j" robe-jump)
  ("k" robe-show-doc)
  ("s" robe-start :color blue)
  ("f" rubocop-check-current-file :color blue)
  ("af" rubocop-autocorrect-current-file :color blue)
  ("e" rubocop-check-directory :color blue)
  ("ae" rubocop-autocorrect-directory :color blue)
  ("p" rubocop-check-project :color blue)
  ("ap" rubocop-autocorrect-project :color blue)
  ("r" inf-ruby :color blue)
  ("]" ruby-switch-to-inf :color blue)
  ("d" ruby-send-definition)
  ("}" ruby-send-definition-and-go :color blue)
  ("i" ruby-send-region)
  (">" ruby-send-region-and-go :color blue)
  ("t" hydra-ruby-tests/body :exit t)
  ("b" hydra-ruby-bundler/body :exit t)
  ("l" hydra-projectile-rails/body :exit t)
  ("q" nil :color blue))

;;;;;;;;;;;;;;
;;    Go    ;;
;;;;;;;;;;;;;;

(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :commands (godef-jump
			 godef-jump-other-window
			 godef-describe
			 gofmt
			 go-import-add
			 go-remove-unused-imports
			 go-goto-imports
			 go-goto-arguments
			 go-goto-docstring
			 go-goto-function
			 go-goto-function-name
			 go-goto-return-values
			 go-goto-method-receiver
			 godoc-at-point
			 go-play-buffer
			 go-play-region
			 go-download-play)
  :bind (:map go-mode-map
			  ("M-\\" . hydra-go/body)
			  ("C-0" . godef-jump)
			  ("C-9" . godoc-at-point)
			  ("C-2" . go-oracle-referrers)
			  ("C-3" . go-goto-imports)
			  ("C-4" . sk/run-go)
			  ("C-5" . sk/build-go)
			  ("C-6" . go-goto-function-name)
			  ("C-7" . sk/test-dir-go)
			  ("C-8" . gofmt))
  :bind* (("C-\\ o" . hydra-go/body)
		  ("C-\\ C-o" . hydra-go/body))
  :config
  ;; Go completion
  (use-package company-go
	:ensure t
	:demand t
	:bind (("C-c y g" . company-go))
	:config
	(progn
	  (add-to-list 'company-backends 'company-go)))
  (defun sk/go-oracle-load ()
	(interactive)
	(load-file (concat
				(getenv "GOPATH") "/src/golang.org/x/tools/cmd/oracle/oracle.el")))
  (add-hook 'go-mode-hook #'sk/go-oracle-load))
;; go helpers
;;;###autoload
(defun sk/build-go ()
  "Builds the go file"
  (interactive)
  (compile
   (concat "go build " (buffer-file-name))))
;;;###autoload
(defun sk/build-dir-go ()
  "build go in the current directory"
  (interactive)
  (compile "go build"))
;;;###autoload
(defun sk/build-project-go ()
  "build the go project - depends on `projectile'"
  (interactive)
  (compile
   (concat "go build " (projectile-project-root))))
;;;###autoload
(defun sk/run-go ()
  "run the go file"
  (interactive)
  (async-shell-command
   (concat "go run " (buffer-file-name))))
;;;###autoload
(defun sk/run-bin-go ()
  "run all the binaries in the project folder - depends on `projectile'"
  (interactive)
  (async-shell-command
   (concat (projectile-project-root) "/bin/*")))
;; go test integration
;;;###autoload
(defun sk/test-dir-go ()
  "run tests in the current directory"
  (interactive)
  (compile "go test -v"))
;;;###autoload
(defun sk/test-project-go ()
  "test the go project - depends on `projectile'"
  (interactive)
  (compile
   (concat "go test -v " (projectile-project-root))))

;; hydra for go
(defhydra hydra-go (:color pink :hint nil)
  "
 ^Semantic Nav^      ^Goto^                          ^Playground^    ^Oracle^                                      ^Build^
^^^^^^^^^^---------------------------------------------------------------------------------------------------------------------------------------------------------
 _j_: jump to def    _e_: arguments  _m_: method rec   _A_: buffer     _s_: scope      _<_: callers     _l_: implements  _c_: current file  _b_: build proj    _q_: quit
 _k_: show doc       _d_: function   _g_: imports      _i_: region     _p_: peers      _>_: callees     _P_: points to   _r_: run file      _B_: build cur dir
 _?_: godef desc     _n_: func name  _u_: rm unused    _W_: download   _G_: callgraph  _f_: definition  _E_: referrers   _R_: run all bin   _t_: test
 _w_: jump other     _v_: return val _a_: add import   _o_: format     _S_: callstack  _F_: freevars    _C_: describe                     _T_: test proj
"
  ("j" godef-jump)
  ("?" godef-describe :color blue)
  ("k" godoc-at-point :color blue)
  ("w" godef-jump-other-window)
  ("e" go-goto-arguments)
  ("d" go-goto-function)
  ("n" go-goto-function-name)
  ("v" go-goto-return-values)
  ("m" go-goto-method-receiver)
  ("g" go-goto-imports)
  ("u" go-remove-unused-imports)
  ("a" go-import-add :color blue)
  ("A" go-play-buffer :color blue)
  ("i" go-play-region :color blue)
  ("W" go-download-play :color blue)
  ("o" gofmt :color blue)
  ("s" go-oracle-set-scope :color blue)
  ("p" go-oracle-peers)
  ("G" go-oracle-callgraph :color blue)
  ("S" go-oracle-callstack :color blue)
  ("<" go-oracle-callers)
  (">" go-oracle-callees)
  ("f" go-oracle-definition)
  ("F" go-oracle-freevars)
  ("l" go-oracle-implements)
  ("P" go-oracle-pointsto)
  ("E" go-oracle-referrers)
  ("C" go-oracle-describe)
  ("c" sk/build-go :color blue)
  ("r" sk/run-go :color blue)
  ("R" sk/run-bin-go :color blue)
  ("b" sk/build-project-go :color blue)
  ("B" sk/build-dir-go :color blue)
  ("t" sk/test-dir-go :color blue)
  ("T" sk/test-project-go :color blue)
  ("q" nil :color blue))

;; ;; links for even more language specific configuration
;; ;; Clojure
;; https://github.com/clojure-emacs/clojure-mode
;; https://github.com/clojure-emacs/cider
;; ;; Scala
;; http://ensime.github.io/
;; ;; Haskell
;; http://commercialhaskell.github.io/intero/

;; provide the entire configuration
(provide 'sk-programming)
