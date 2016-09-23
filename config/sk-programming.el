;;;;;;;;;;;;;;;;;;;;;;
;;    Emacs Lisp    ;;
;;;;;;;;;;;;;;;;;;;;;;

(use-package macrostep
  :ensure t)
(general-evil-define-key '(normal visual) emacs-lisp-mode-map
  "J" '(find-function-at-point :which-key "find definition")
  "K" '(describe-function :which-key "show doc"))
(general-nvmap :prefix sk--evil-local-leader
	       "e" '(hydra-elisp/body :which-key "elisp"))

;; lisp interaction mode
(general-evil-define-key '(normal visual) lisp-interaction-mode-map
  "J" '(find-function-at-point :which-key "find definition")
  "K" '(describe-function :which-key "show doc"))

;; hydra for emacs lisp
(defhydra hydra-elisp (:color pink :hint nil)
  "
 ^Macro^                        ^Eval^                          ^Run^            ^Describe^       ^Find^
^^^^^^^^^^-----------------------------------------------------------------------------------------------------------------------
 _e_: expand       _n_: next      _d_: defun   _x_: exp             _r_: run ielm    _K_: function    _V_: variable    _q_: quit
 _c_: collapse     _p_: previous  _b_: buffer  _s_: last sexp                      _v_: variable    _L_: library
 _C_: collapse all              _i_: region  _S_: print last sexp                _l_: library     _J_: function
"
  ("e" macrostep-expand)
  ("c" macrostep-collapse)
  ("C" macrostep-collapse-all)
  ("n" macrostep-next-macro)
  ("p" macrostep-prev-macro)
  ("d" eval-defun :color blue)
  ("b" eval-buffer :color blue)
  ("i" eval-region)
  ("x" eval-expression :color blue)
  ("s" eval-last-sexp)
  ("S" eval-print-last-sexp)
  ("r" ielm :color blue)
  ("K" describe-function :color blue)
  ("v" describe-variable :color blue)
  ("l" describe-library :color blue)
  ("J" find-function :color blue)
  ("V" find-variable :color blue)
  ("L" find-library :color blue)
  ("q" nil :color blue))

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

;; c++ indexer and semantics
(use-package rtags
  :ensure t
  :general
  (general-evil-define-key '(normal visual) c++-mode-map
    "J" '(rtags-find-symbol-at-point :which-key "find definition")
    "K" '(rtags-print-symbol-info :which-key "show doc"))
  (general-nvmap :prefix sk--evil-local-leader
		 "c" '(hydra-cpp/body :which-key "c++"))
  :init
  (setq rtags-autostart-diagnostics t)
  (setq rtags-completions-enabled t))

;; convert emacs into a c++ ide based on cmake
(use-package cmake-ide
  :ensure t
  :defer 2
  :config
  (cmake-ide-setup))

;; hydra for cpp
(defhydra hydra-cpp (:color pink :hint nil)
  "
 ^Semantic Navigation^                                                        ^Compilation^
^^^^^^^^^^---------------------------------------------------------------------------------------------------------------------------------------------------------------
 _J_: symbol           _v_: virtuals      _]_: stack forward    _i_: diagnostics    _b_: build       _c_: compile with math        _o_: openmp with math        _q_: quit
 _s_: find symbol      _S_: dependencies  _[_: stack back       _f_: fixit          _B_: make        _C_: compile without math     _O_: openmp without math
 _r_: references       _K_: info          _n_: next match       _P_: preprocess	    _d_: build doc   _h_: hybrid with math         _m_: mpi with math
 _R_: find references  _t_: type          _p_: prev match       _N_: rename	    _D_: make doc    _H_: hybrid without math      _M_: mpi without math
"
  ("J" rtags-find-symbol-at-point)
  ("s" rtags-find-symbol :color blue)
  ("r" rtags-find-references-at-point)
  ("R" rtags-find-references :color blue)
  ("v" rtags-find-virtuals-at-point)
  ("S" rtags-print-dependencies :color blue)
  ("K" rtags-print-symbol-info :color blue)
  ("t" rtags-symbol-type)
  ("e" rtags-print-enum-value-at-point)
  ("]" rtags-location-stack-forward)
  ("[" rtags-location-stack-backward)
  ("n" rtags-next-match)
  ("p" rtags-previous-match)
  ("i" rtags-diagnostics :color blue)
  ("f" rtags-fixit)
  ("P" rtags-preprocess-file)
  ("N" rtags-rename-symbol :color blue)
  ("b" sk/compile-cpp-build :color blue)
  ("B" sk/compile-cpp-make :color blue)
  ("d" sk/compile-cpp-build-doc :color blue)
  ("D" sk/compile-cpp-make-doc :color blue)
  ("c" sk/compile-cpp-math :color blue)
  ("C" sk/compile-cpp-simple :color blue)
  ("o" sk/compile-cpp-omp-math :color blue)
  ("O" sk/compile-cpp-omp-simple :color blue)
  ("m" sk/compile-cpp-mpi-math :color blue)
  ("M" sk/compile-cpp-mpi-simple :color blue)
  ("h" sk/compile-cpp-hybrid-math :color blue)
  ("H" sk/compile-cpp-hybrid-simple :color blue)
  ("q" nil :color blue))

;;;;;;;;;;;;;;;;;;
;;    Python    ;;
;;;;;;;;;;;;;;;;;;

;; configure python itself
(use-package python
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :general
  (general-nvmap :prefix sk--evil-local-leader
		 "p" '(hydra-python/body :which-key "python"))
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
    :commands (anaconda-mode-find-definitions
	       anaconda-mode-find-assignments
	       anaconda-mode-find-references
	       anaconda-mode-find-file
	       anaconda-mode-show-doc
	       anaconda-mode-go-back)
    :general
    (general-evil-define-key '(normal visual) python-mode-map
      "J" '(anaconda-mode-find-definitions :which-key "find definition")
      "K" '(anaconda-mode-show-doc :which-key "show doc"))
    (general-evil-define-key 'normal anaconda-view-mode-map
      "q" (general-simulate-keys "q" t "quit"))
    :config
    (progn
      (add-hook 'python-mode-hook 'anaconda-mode))))

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
 _d_: deactivate  _v_: list venv   _r_: remove  _u_: unset
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
 _o_: one  _d_: dir       _O_: one  _D_: dir      _q_: quit
 _a_: all  _f_: failed    _A_: all  _F_: failed
	 _m_: module            _M_: module
"
  ("o" pytest-one)
  ("a" pytest-all)
  ("d" pytest-directory)
  ("f" pytest-failed)
  ("m" pytest-module)
  ("O" pytest-pdb-one)
  ("A" pytest-pdb-all)
  ("D" pytest-pdb-directory)
  ("F" pytest-pdb-failed)
  ("M" pytest-pdb-module)
  ("q" nil :color blue))

;; hydra for python
(defhydra hydra-python (:color pink :hint nil)
  "
 ^Semantic Navigation^         ^Eval^         ^REPL^        ^Miscellaneous^
^^^^^^^^^^---------------------------------------------------------------------------------------------------------
 _J_: definition    _K_: doc     _d_: def       _r_: run      _v_: virtualenv    _x_: sphinx        _q_: quit
 _a_: assignments   _f_: file    _b_: buffer    _R_: switch   _t_: tests         _y_: yapf region
 _e_: references    _[_: back    _i_: region                                 _Y_: yapf buffer
"
  ("J" anaconda-mode-find-definitions)
  ("a" anaconda-mode-find-assignments)
  ("e" anaconda-mode-find-references)
  ("K" anaconda-mode-show-doc :color blue)
  ("f" anaconda-mode-find-file :color blue)
  ("[" anaconda-mode-go-back)
  ("d" python-shell-send-defun)
  ("b" python-shell-send-buffer :color blue)
  ("i" python-shell-send-region)
  ("r" run-python :color blue)
  ("R" python-shell-switch-to-shell :color blue)
  ("v" hydra-py-venv/body :exit t)
  ("t" hydra-py-test/body :exit t)
  ("x" sphinx-doc :color blue)
  ("y" py-yapf-region)
  ("Y" py-yapf-buffer :color blue)
  ("q" nil :color blue))

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
  :general
  (general-evil-define-key '(normal visual) ess-mode-map
    ;; "J" '(nil :which-key "find definition")
    "K" '(ess-display-help-on-object :which-key "show doc"))
  (general-nvmap :prefix sk--evil-local-leader
		 "s" '(hydra-stats/body :which-key "stats"))
  :init
  (setq ess-use-ido nil)
  :config
  (require 'ess-site))

;; hydra for stats package
(defhydra hydra-stats-packages (:color blue :hint nil)
  "
 ^Package^      ^Devtools^                             ^Revdep^
^^^^^^^^^^------------------------------------------------------------------------------
 _i_: index     _I_: install    _s_: set     _g_: github   _C_: check cmd     _q_: quit
 _l_: variable  _L_: load       _u_: unload  _h_: check    _P_: check package
 _p_: install   _d_: document   _t_: test
"
  ("i" ess-display-package-index)
  ("l" ess-install-library)
  ("p" ess-install.packages)
  ("I" ess-r-devtools-install-package)
  ("L" ess-r-devtools-load-package)
  ("d" ess-r-devtools-document-package)
  ("s" ess-r-package-set-package)
  ("u" ess-r-devtools-unload-package)
  ("t" ess-r-devtools-test-package)
  ("g" ess-r-devtools-install-github)
  ("h" ess-r-devtools-check-package)
  ("C" ess-r-devtools-revdep-check-cmd)
  ("P" ess-r-devtools-revdep-check-package)
  ("q" nil :color blue))

;; hydra for stats roxy
(defhydra hydra-stats-roxy (:color pink :hint nil)
  "
 ^Roxy Entry^                     ^Preview^
^^^^^^^^^^------------------------------------------------
 _n_: next    _t_: toggle region    _h_: html    _q_: quit
 _p_: prev    _a_: show all         _x_: text
 _u_: update  _c_: cycle            _r_: Rd
"
  ("n" ess-roxy-next-entry)
  ("p" ess-roxy-previous-entry)
  ("u" ess-roxy-update-entry)
  ("t" ess-roxy-toggle-roxy-region)
  ("a" ess-roxy-show-all :color blue)
  ("c" ess-roxy-cycle-example)
  ("h" ess-roxy-preview-HTML :color blue)
  ("x" ess-roxy-preview-text :color blue)
  ("r" ess-roxy-preview-Rd :color blue)
  ("q" nil :color blue))

;; hydra for stats debug
(defhydra hydra-stats-debug (:color pink :hint nil)
  "
 ^Breakpoint^                               ^Debug^
^^^^^^^^^^------------------------------------------------------------------------------------------------------------
 _s_: set       _l_: logger       _n_: next     _w_: watch      _G_: tracebug help  _i_: goto input     _q_: quit
 _u_: kill      _c_: conditional  _p_: prev     _a_: call stack _f_: flag           _r_: error activate
 _U_: kill all  _e_: eval env     _t_: toggle   _g_: tracebug   _F_: unflag         _b_: traceback
"
  ("s" ess-bp-set)
  ("u" ess-bp-kill)
  ("U" ess-bp-kill-all)
  ("l" ess-bp-set-logger)
  ("c" ess-bp-set-conditional)
  ("e" ess-r-set-evaluation-env :color blue)
  ("n" ess-bp-next)
  ("p" ess-bp-previous)
  ("t" ess-bp-toggle-state)
  ("w" ess-watch :color blue)
  ("a" ess-show-call-stack :color blue)
  ("g" ess-toggle-tracebug :color blue)
  ("G" ess-tracebug-show-help :color blue)
  ("f" ess-debug-flag-for-debugging)
  ("F" ess-debug-unflag-for-debugging)
  ("i" ess-debug-goto-input-event-marker :color blue)
  ("r" ess-debug-toggle-error-action)
  ("b" ess-show-traceback :color blue)
  ("q" nil :color blue))

;; hydra for stats
(defhydra hydra-stats (:color pink :hint nil)
  "
 ^Eval/REPL^                                                    ^Describe/Doc^                  ^Misc^
^^^^^^^^^^-----------------------------------------------------------------------------------------------------------------------
 _r_: R repl          _d_: func/para     _l_: load file             _h_: help       _w_: web search   _p_: packages    _q_: quit
 _u_: julia repl      _i_: region/line   _L_: load library          _o_: obj at pt  _a_: apropos      _x_: execute
 _R_: switch to repl  _b_: buffer        _f_: load file namespace   _D_: demos      _O_: dump obj     _s_: style
 _c_: chunk           _g_: to beg        _G_: to end                _v_: vignettes  _e_: debug        _y_: roxy
"
  ("r" R :color blue)
  ("u" julia :color blue)
  ("R" ess-switch-to-ESS :color blue)
  ("d" ess-eval-function-or-paragraph-and-step)
  ("i" ess-eval-region-or-line-and-step)
  ("b" ess-eval-buffer)
  ("g" ess-eval-buffer-from-beg-to-here)
  ("G" ess-eval-buffer-from-here-to-end)
  ("c" ess-eval-chunk-and-step)
  ("l" ess-load-file)
  ("L" ess-load-library)
  ("f" ess-r-load-file-namespaced)
  ("O" ess-dump-object-into-edit-buffer :color blue)
  ("h" ess-help :color blue)
  ("o" ess-describe-object-at-point :color blue)
  ("D" ess-display-demos :color blue)
  ("v" ess-display-vignettes :color blue)
  ("w" ess-help-web-search :color blue)
  ("a" ess-display-help-apropos :color blue)
  ("e" hydra-stats-debug/body :exit t)
  ("x" ess-execute :color blue)
  ("s" ess-set-style :color blue)
  ("p" hydra-stats-packages/body :exit t)
  ("y" hydra-stats-roxy/body :exit t)
  ("q" nil :color blue))

;;;;;;;;;;;;;;;;;;
;;    MATLAB    ;;
;;;;;;;;;;;;;;;;;;

;; install matlab from OCIO first
(use-package matlab-mode
  :ensure t
  :mode ("\\.m\\'" . matlab-mode)
  :bind (:map matlab-shell-mode-map
	      ("C-c C-c" . term-interrupt-subjob))
  :init
  (setq matlab-shell-command "/Applications/MATLAB_R2016a.app/bin/matlab"
	matlab-indent-function t)
  ;; (setq matlab-mode-install-path '("~/Dropbox/PhD/codes/ttmat"
  ;; 				   "/Applications/MATLAB_R2016a.app/toolbox"))
  (eval-after-load 'matlab
    '(add-to-list 'matlab-shell-command-switches "-nodesktop -nosplash"))
  :commands (matlab-shell
	     matlab-show-matlab-shell-buffer
	     matlab-shell-run-region-or-line
	     matlab-shell-run-cell
	     matlab-shell-run-command
	     matlab-shell-topic-browser
	     matlab-shell-describe-command
	     matlab-shell-describe-variable
	     matlab-show-line-info
	     matlab-find-file-on-path)
  :general
  (general-evil-define-key '(normal visual) matlab-mode-map
    "J" '(matlab-find-file-on-path :which-key "find definition")
    "K" '(matlab-shell-describe-command :which-key "show doc"))
  (general-nvmap :prefix sk--evil-local-leader
		 "m" '(hydra-matlab/body :which-key "matlab")))

;; hydra for matlab
(defhydra hydra-matlab (:color pink :hint nil)
  "
 ^Describe^                      ^Run^                ^REPL^
^^^^^^^^^^-------------------------------------------------------------------
 _K_: command    _t_: topic        _i_: region/line     _r_: matlab    _q_: quit
 _v_: variable   _J_: file on path _f_: cell            _R_: switch
 _l_: line                       _c_: command
"
  ("K" matlab-shell-describe-command)
  ("v" matlab-shell-describe-variable)
  ("l" matlab-show-line-info)
  ("t" matlab-shell-topic-browser :color blue)
  ("J" matlab-find-file-on-path :color blue)
  ("i" matlab-shell-run-region-or-line)
  ("f" matlab-shell-run-cell)
  ("c" matlab-shell-run-command :color blue)
  ("r" matlab-shell :color blue)
  ("R" matlab-show-matlab-shell-buffer :color blue)
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
  :general
  (general-nvmap :prefix sk--evil-local-leader
		 "w" '(hydra-web/body :which-key "web")))
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
 _w_: httpd start  _m_: skewer mode        _r_: skewer repl     _d_: eval defun        _p_: phantomjs start     _h_: html region    _t_: js region    _q_: quit
 _W_: httpd stop   _L_: skewer html mode   _R_: run skewer      _l_: load buffer       _P_: phantomjs stop      _H_: html buffer    _T_: js buffer
 _i_: impatient    _M_: skewer css mode    _I_: list clients    _s_: last sexp         _b_: bower load          _c_: css region
							  _S_: print last sexp   _B_: bower refresh       _C_: css buffer
"
  ("w" httpd-start)
  ("W" httpd-stop)
  ("i" impatient-mode)
  ("m" skewer-mode :color blue)
  ("L" skewer-html-mode :color blue)
  ("M" skewer-css-mode :color blue)
  ("r" skewer-repl :color blue)
  ("R" run-skewer :color blue)
  ("I" list-skewer-clients)
  ("l" skewer-load-buffer)
  ("d" skewer-eval-defun)
  ("s" skewer-eval-last-expression)
  ("S" skewer-eval-print-last-expression)
  ("p" skewer-run-phantomjs)
  ("P" skewer-phantomjs-kill)
  ("b" skewer-bower-load)
  ("B" skewer-bower-refresh)
  ("h" web-beautify-html)
  ("H" web-beautify-html-buffer)
  ("c" web-beautify-css)
  ("C" web-beautify-css-buffer)
  ("t" web-beautify-js)
  ("T" web-beautify-js-buffer)
  ("q" nil :color blue))

;; js3 mode for javascript
(use-package js3-mode
  :ensure t
  :mode ("\\.js\\'" . js3-mode)
  :general
  (general-nvmap :prefix sk--evil-local-leader
		 "j" '(hydra-javascript/body :which-key "javscript")))
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
  :general
  (general-evil-define-key '(normal visual) js3-mode-map
    "J" '(tern-find-definition :which-key "find definition")
    "K" '(tern-get-docs :which-key "show doc"))
  (general-evil-define-key '(normal visual) js-mode-map
    "J" '(tern-find-definition :which-key "find definition")
    "K" '(tern-get-docs :which-key "show doc"))
  :config
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
 _t_: type            _J_: definition    _r_: node repl       _s_: last sexp  _v_: nvm use        _q_: quit
 _u_: use server      _K_: docs          _R_: switch to repl  _i_: region     _V_: nvm use for
 _h_: highlight refs                   _l_: load file       _b_: buffer     _f_: format
 _N_: rename                           _x_: execute                       _F_: ast format
"
  ("t" tern-get-type)
  ("u" tern-use-server)
  ("h" tern-highlight-refs)
  ("N" tern-rename-variable)
  ("J" tern-find-definition)
  ("K" tern-get-docs)
  ("r" nodejs-repl :color blue)
  ("R" nodejs-repl-switch-to-repl :color blue)
  ("s" nodejs-repl-send-last-sexp)
  ("i" nodejs-repl-send-region)
  ("b" nodejs-repl-send-buffer :color blue)
  ("x" nodejs-repl-execute :color blue)
  ("l" nodejs-repl-load-file :color blue)
  ("v" nvm-use :color blue)
  ("V" nvm-use-for :color blue)
  ("f" jsfmt)
  ("F" jsfmt-ast)
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

;; emmet for easy HTML
(use-package emmet-mode
  :ensure t
  :diminish (emmet-mode . " ε")
  :general
  (general-imap "C-o" 'emmet-expand-line)
  (general-imap "C-y" 'emmet-prev-edit-point)
  (general-imap "C-e" 'emmet-next-edit-point))

;;;;;;;;;;;;;;;
;;    Lua    ;;
;;;;;;;;;;;;;;;

(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'"
  :general
  (general-evil-define-key '(normal visual) lua-mode-map
    "K" '(lua-search-documentation :which-key "show doc"))
  (general-nvmap :prefix sk--evil-local-leader
		 "u" '(hydra-lua/body :which-key "lua")))

;; hydra for lua
(defhydra hydra-lua (:color pink :hint nil)
  "
 ^Eval^
^^^^^^^^^^--------------------------------------------------------
 _r_: run lua        _i_: send region   _d_: send defun    _q_: quit
 _R_: switch to lua  _b_: send buffer   _l_: send line
 _K_: lua-search-documentation
"
  ("r" lua-start-process :color blue)
  ("R" lua-show-process-buffer :color blue)
  ("K" lua-search-documentation :color blue)
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
  :general
  (general-nvmap :prefix sk--evil-local-leader
		 "l" '(hydra-sml/body :which-key "sml")))

;; hydra for sml
(defhydra hydra-sml (:color pink :hint nil)
  "
 ^Eval^
^^^^^^^^^^--------------------------------------------------------
 _r_: run sml        _i_: send region   _c_: compile    _q_: quit
 _R_: switch to sml  _b_: send buffer   _l_: load file
"
  ("r" sml-prog-proc-switch-to :color blue)
  ("R" sml-prog-proc-switch-to :color blue)
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
  :general
  (general-nvmap :prefix sk--evil-local-leader
		 "k" '(hydra-racket-scheme/body :which-key "racket/scheme"))
  (general-evil-define-key '(normal visual) scheme-mode-map
    "J" '(geiser-edit-symbol-at-point :which-key "find definition")
    "K" '(geiser-doc-symbol-at-point :which-key "show doc"))
  (general-evil-define-key '(normal visual) racket-mode-map
    "J" '(geiser-edit-symbol-at-point :which-key "find definition")
    "K" '(geiser-doc-symbol-at-point :which-key "show doc")))

;; hydra for racket/scheme
(defhydra hydra-racket-scheme (:color pink :hint nil)
  "
 ^Semantic Navigation^                                         ^Eval^
^^^^^^^^^^-------------------------------------------------------------------------------------------------------------------------------------------------------
 _J_: symbol at point     _e_: edit module    _x_: xref callers    _r_: run geiser      _d_: eval defun     _D_: expand defun     _c_: compile    _q_: quit
 _K_: doc symbol at pt    _m_: doc module     _X_: xref callees    _R_: switch to repl  _b_: eval buffer    _s_: eval last sexp   _l_: load
 _L_: insert lambda       _M_: doc manual                                           _i_: eval region    _S_: expand last sexp
"
  ("J" geiser-edit-symbol-at-point)
  ("K" geiser-doc-symbol-at-point)
  ("r" run-geiser :color blue)
  ("R" geiser-mode-switch-to-repl :color blue)
  ("i" geiser-eval-region)
  ("d" geiser-eval-definition)
  ("D" geiser-expand-definition)
  ("b" geiser-eval-buffer :color blue)
  ("s" geiser-eval-last-sexp)
  ("S" geiser-expand-last-sexp)
  ("c" geiser-compile-current-buffer :color blue)
  ("l" geiser-load-file :color blue)
  ("L" geiser-insert-lambda :color blue)
  ("e" geiser-edit-module :color blue)
  ("a" geiser-add-to-load-path :color blue)
  ("m" geiser-doc-module :color blue)
  ("M" geiser-doc-lookup-manual :color blue)
  ("x" geiser-xref-callers)
  ("X" geiser-xref-callees)
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
  :general
  (general-nvmap :prefix sk--evil-local-leader
		 "r" '(hydra-ruby/body :which-key "ruby"))
  :config
  (add-hook 'ruby-mode-hook 'inf-ruby-mode))

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
  :general
  (general-evil-define-key '(normal visual) ruby-mode-map
    "J" '(robe-jump :which-key "find definition")
    "K" '(robe-doc :which-key "show doc"))
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
 _s_: show    _v_: version    _h_: check    _c_: console  _q_: quit
 _g_: gemfile _d_: outdated   _o_: open
 _e_: exec    _i_: install    _u_: update
"
  ("s" bundle-show)
  ("g" bundle-gemfile)
  ("e" bundle-exec)
  ("v" bundle-version)
  ("d" bundle-outdated)
  ("i" bundle-install)
  ("h" bundle-check)
  ("o" bundle-open)
  ("u" bundle-update)
  ("c" bundle-console)
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
 _t_: run          _v_: verify         _M_: verify matching  _f_: last failed  _a_: target               _p_: pending    _q_: quit
 _T_: run at point _V_: verify all     _r_: rerun            _S_: single       _A_: target other window
 _s_: toggle spec  _m_: verify method  _c_: continue         _e_: example      _E_: example other window
"
  ("t" ruby-test-run)
  ("T" ruby-test-run-at-point)
  ("s" ruby-test-toggle-implementation-and-specification)
  ("v" rspec-verify)
  ("V" rspec-verify-all)
  ("m" rspec-verify-method)
  ("M" rspec-verify-matching)
  ("r" rspec-rerun)
  ("c" rspec-continue)
  ("f" rspec-run-last-failed)
  ("S" rspec-verify-single)
  ("e" rspec-toggle-spec-and-target-find-example)
  ("E" rspec-find-spec-or-target-find-example-other-window)
  ("a" rspec-toggle-spec-and-target)
  ("A" rspec-find-spec-or-target-other-window)
  ("p" rspec-toggle-example-pendingness)
  ("q" nil :color blue))

;; rails coding
  (use-package projectile-rails
    :ensure t
    :commands (hydra-projectile-rails/body)
    :init
    (setq projectile-rails-expand-snippet nil)
    :config
    (add-hook 'projectile-mode-hook 'projectile-rails-on)
    (projectile-rails-mode))

;; hydra for ruby
(defhydra hydra-ruby (:color pink :hint nil)
  "
 ^Semantic Navigation^       ^Static analysis^                               ^Eval^                                           ^Misc^
^^^^^^^^^^-------------------------------------------------------------------------------------------------------------------------------------------------------
 _m_: method def   _K_: doc    _f_: check file        _p_: check project         _r_: ruby repl        _d_: send defun              _t_: tests     _q_:quit
 _M_: module def   _e_: robe   _F_: autcorrect file   _P_: autocorrect project   _R_: switch to repl   _D_: send defun and go       _b_: bundler
 _c_: call at pt             _e_: check dir                                                      _i_: send region             _l_: rails
 _J_: jump                   _E_: autocorrect dir                                                _I_: send region and go
"
  ("m" robe-method-def)
  ("M" robe-module-def)
  ("c" robe-call-at-point)
  ("J" robe-jump)
  ("K" robe-show-doc)
  ("e" robe-start :color blue)
  ("f" rubocop-check-current-file :color blue)
  ("F" rubocop-autocorrect-current-file :color blue)
  ("e" rubocop-check-directory :color blue)
  ("E" rubocop-autocorrect-directory :color blue)
  ("p" rubocop-check-project :color blue)
  ("P" rubocop-autocorrect-project :color blue)
  ("r" inf-ruby :color blue)
  ("R" ruby-switch-to-inf :color blue)
  ("d" ruby-send-definition)
  ("D" ruby-send-definition-and-go :color blue)
  ("i" ruby-send-region)
  ("I" ruby-send-region-and-go :color blue)
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
  :general
  (general-evil-define-key '(normal visual) go-mode-map
    "J" '(godef-jump :which-key "find definition")
    "K" '(godoc-at-point :which-key "show doc"))
  (general-nvmap :prefix sk--evil-local-leader
		 "g" '(hydra-go/body :which-key "go"))
  :config
  (defun sk/go-oracle-load ()
    (interactive)
    (load-file (concat
		(getenv "GOPATH") "/src/golang.org/x/tools/cmd/oracle/oracle.el")))
  (add-hook 'go-mode-hook #'sk/go-oracle-load))

;; hydra for go
(defhydra hydra-go (:color pink :hint nil)
  "
 ^Semantic Nav^      ^Goto^                          ^Playground^    ^Oracle^
^^^^^^^^^^-----------------------------------------------------------------------------------------------------------------
 _J_: jump to def    _e_: arguments  _m_: method rec   _b_: buffer     _s_: scope      _<_: callers     _l_: implements  _q_: quit
 _K_: show doc       _d_: function   _g_: imports      _i_: region     _p_: peers      _>_: callees     _o_: points to
 _D_: godef desc     _n_: func name  _u_: rm unused    _W_: download   _G_: callgraph  _f_: definition  _r_: referrers
 _w_: jump other     _v_: return val _a_: add import   _t_: format     _S_: callstack  _F_: freevars    _c_: describe
"
  ("J" godef-jump)
  ("D" godef-describe :color blue)
  ("K" godoc-at-point :color blue)
  ("w" godef-jump-other-window)
  ("e" go-goto-arguments)
  ("d" go-goto-function)
  ("n" go-goto-function-name)
  ("v" go-goto-return-values)
  ("m" go-goto-method-receiver)
  ("g" go-goto-imports)
  ("u" go-remove-unused-imports)
  ("a" go-import-add :color blue)
  ("b" go-play-buffer :color blue)
  ("i" go-play-region :color blue)
  ("W" go-download-play :color blue)
  ("t" gofmt :color blue)
  ("s" go-oracle-set-scope :color blue)
  ("p" go-oracle-peers)
  ("G" go-oracle-callgraph :color blue)
  ("S" go-oracle-callstack :color blue)
  ("<" go-oracle-callers)
  (">" go-oracle-callees)
  ("f" go-oracle-definition)
  ("F" go-oracle-freevars)
  ("l" go-oracle-implements)
  ("o" go-oracle-pointsto)
  ("r" go-oracle-referrers)
  ("c" go-oracle-describe)
  ("q" nil :color blue))

;; ;; links for even more language specific configuration
;; ;; Clojure
;; https://github.com/clojure-emacs/clojure-mode
;; https://github.com/clojure-emacs/cider
;; ;; Scala
;; http://ensime.github.io/
;; ;; Haskell
;; http://commercialhaskell.github.io/intero/
;; ;; OCaml
;; https://github.com/ocaml/tuareg
;; https://github.com/the-lambda-church/merlin
;; ;; Elixir
;; https://github.com/tonini/alchemist.el

;; provide the entire configuration
(provide 'sk-programming)
