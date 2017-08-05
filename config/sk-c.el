;; compile functions
;; (add-to-list 'auto-mode-alist '("\\.h\\'" . c-mode))

;;;###autoload
(defun sk/compile-c-make ()
  "Compiles the file using the makefile in the current directory"
  (interactive)
  (compile "make"))
;;;###autoload
(defun sk/compile-c-make-doc ()
  "Generates the documentation using the makefile in the current directory"
  (interactive)
  (compile "make doc"))
;;;###autoload
(defun sk/compile-c-build ()
  "Compiles the file using the makefile in the build directory"
  (interactive)
  (compile "make -C build"))
;;;###autoload
(defun sk/compile-c-build-doc ()
  "Generates the documentation using the makefile in the build directory"
  (interactive)
  (compile "make -C build doc"))
;;;###autoload
(defun sk/compile-c-omp-math (arg)
  "Compiles the file with OpenMP and math libraries"
  (interactive "P")
  (if arg
	  (compile
	   (concat "icc -fast -Wall -fopenmp -lgsl -lcblas -llapack -O2 -g " (buffer-name)
			   " -o " (file-name-sans-extension (buffer-name))))
	(compile
	 (concat "gcc -Wall -fopenmp -lgsl -lcblas -llapack -O2 -g " (buffer-name)
			 " -o " (file-name-sans-extension (buffer-name))))))
;;;###autoload
(defun sk/compile-c-omp-simple (arg)
  "Compiles the file with OpenMP"
  (interactive "P")
  (if arg
	  (compile
	   (concat "icc -fast -O3 -Wall -fopenmp -g " (buffer-name)
			   " -o " (file-name-sans-extension (buffer-name))))
	(compile
	 (concat "gcc -O3 -Wall -fopenmp -g " (buffer-name)
			 " -o " (file-name-sans-extension (buffer-name))))))
;;;###autoload
(defun sk/compile-c-mpi-math ()
  "Compiles the file with MPI and math libraries"
  (interactive)
  (compile
   (cond ((eq system-type 'darwin)
		  (concat "/usr/local/openmpi/bin/mpicc -Wall -lgsl -lcblas -llapack -larmadillo -O2 -g "
		   (buffer-name) " -o " (file-name-sans-extension (buffer-name)) ".out"))
		 ((eq system-type 'gnu/linux)
		  (concat "mpicc -Wall -lgsl -lcblas -llapack -larmadillo -O2 -g "
		   (buffer-name) " -o " (file-name-sans-extension (buffer-name)) ".out")))))
;;;###autoload
(defun sk/compile-c-mpi-simple ()
  "Compiles the file with MPI"
  (interactive)
  (compile
   (cond ((eq system-type 'darwin)
		  (concat "/usr/local/openmpi/bin/mpicc -O3 -Wall -g " (buffer-name)
		   " -o " (file-name-sans-extension (buffer-name)) ".out"))
		 ((eq system-type 'gnu/linux)
		  (concat "mpicc -O3 -Wall -g " (buffer-name)
		   " -o " (file-name-sans-extension (buffer-name)) ".out")))))
;;;###autoload
(defun sk/compile-c-hybrid-math ()
  "Compiles the file with OpenMP, MPI and math libraries"
  (interactive)
  (compile
   (cond ((eq system-type 'darwin)
		  (concat
		   "/usr/local/openmpi/bin/mpicc -Wall -fopenmp -lgsl -lcblas -llapack -larmadillo -O2 -g "
		   (buffer-name) " -o " (file-name-sans-extension (buffer-name)) ".out"))
		 ((eq system-type 'gnu/linux)
		  (concat "mpicc -Wall -fopenmp -lgsl -lcblas -llapack -larmadillo -O2 -g "
		   (buffer-name) " -o " (file-name-sans-extension (buffer-name)) ".out")))))
;;;###autoload
(defun sk/compile-c-hybrid-simple ()
  "Compiles the file with OpenMP and MPI"
  (interactive)
  (compile
   (cond ((eq system-type 'darwin)
		  (concat "/usr/local/openmpi/bin/mpicc -O3 -Wall -fopenmp -g " (buffer-name)
		   " -o " (file-name-sans-extension (buffer-name)) ".out"))
		 ((eq system-type 'gnu/linux)
		  (concat "mpicc -O3 -Wall -fopenmp -g " (buffer-name)
		   " -o " (file-name-sans-extension (buffer-name)) ".out")))))
;;;###autoload
(defun sk/compile-c-math (arg)
  "Compiles the file with math libraries"
  (interactive "P")
  (if arg
	  (compile
	   (concat "icc -fast -Wall -lgsl -lcblas -llapack -larmadillo -O2 -g " (buffer-name)
			   " -o " (file-name-sans-extension (buffer-name))))
	(compile
	 (concat "gcc -Wall -lgsl -lcblas -llapack -larmadillo -O2 -g " (buffer-name)
			 " -o " (file-name-sans-extension (buffer-name))))))
;;;###autoload
(defun sk/compile-c-simple (arg)
  "Compiles the file"
  (interactive "P")
  (if arg
	  (compile
	   (concat "icc -fast -O3 -Wall -g " (buffer-name)
			   " -o " (file-name-sans-extension (buffer-name))))
	(compile
	 (concat "gcc -O3 -Wall -g " (buffer-name)
			 " -o " (file-name-sans-extension (buffer-name))))))
;;;###autoload
(defun sk/compile-c-run (arg)
  "Runs the compiled file"
  (interactive
   (list
	(read-string "Enter a command line argument (if any): ")))
  (async-shell-command
   (concat "./" (file-name-sans-extension (buffer-name)) " " arg)))
;;;###autoload
(defun sk/compile-c-mpirun (arg)
  "Runs the compiled file"
  (interactive
   (list
	(read-string "Enter the number of processes (if necessary): ")))
  (cond ((eq system-type 'darwin)
		 (async-shell-command
		  (concat "/usr/local/openmpi/bin/mpirun -np " arg " ./"
				  (file-name-sans-extension (buffer-name)) ".out")))
		((eq system-type 'gnu/linux)
		 (async-shell-command
		  (concat "mpirun -np " arg " ./" (file-name-sans-extension (buffer-name)) ".out")))))

;;; Shell interaction with tmux
;;;###autoload
(defun sk/tmux-compile-c-make ()
  "Compiles the file using the makefile in the current directory"
  (interactive)
  (emamux:send-command "make"))
;;;###autoload
(defun sk/tmux-compile-c-make-doc ()
  "Generates the documentation using the makefile in the current directory"
  (interactive)
  (emamux:send-command "make doc"))
;;;###autoload
(defun sk/tmux-compile-c-build ()
  "Compiles the file using the makefile in the build directory"
  (interactive)
  (emamux:send-command "make -C build"))
;;;###autoload
(defun sk/tmux-compile-c-build-doc ()
  "Generates the documentation using the makefile in the build directory"
  (interactive)
  (emamux:send-command "make -C build doc"))
;;;###autoload
(defun sk/tmux-compile-c-omp-math (arg)
  "Compiles the file with OpenMP and math libraries"
  (interactive "P")
  (if arg
	  (emamux:send-command
	   (concat "icc -fast -Wall -fopenmp -lgsl -lcblas -llapack -O2 -g " (buffer-name)
			   " -o " (file-name-sans-extension (buffer-name))))
	(emamux:send-command
	 (concat "gcc -Wall -fopenmp -lgsl -lcblas -llapack -O2 -g " (buffer-name)
			 " -o " (file-name-sans-extension (buffer-name))))))
;;;###autoload
(defun sk/tmux-compile-c-omp-simple (arg)
  "Compiles the file with OpenMP"
  (interactive "P")
  (if arg
	  (emamux:send-command
	   (concat "icc -fast -O3 -Wall -fopenmp -g " (buffer-name)
			   " -o " (file-name-sans-extension (buffer-name))))
	(emamux:send-command
	 (concat "gcc -O3 -Wall -fopenmp -g " (buffer-name)
			 " -o " (file-name-sans-extension (buffer-name))))))
;;;###autoload
(defun sk/tmux-compile-c-mpi-math ()
  "Compiles the file with MPI and math libraries"
  (interactive)
  (emamux:send-command
   (cond ((eq system-type 'darwin)
		  (concat "/usr/local/openmpi/bin/mpicc -Wall -lgsl -lcblas -llapack -larmadillo -O2 -g "
		   (buffer-name) " -o " (file-name-sans-extension (buffer-name)) ".out"))
		 ((eq system-type 'gnu/linux)
		  (concat "mpicc -Wall -lgsl -lcblas -llapack -larmadillo -O2 -g "
		   (buffer-name) " -o " (file-name-sans-extension (buffer-name)) ".out")))))
;;;###autoload
(defun sk/tmux-compile-c-mpi-simple ()
  "Compiles the file with MPI"
  (interactive)
  (emamux:send-command
   (cond ((eq system-type 'darwin)
		  (concat "/usr/local/openmpi/bin/mpicc -O3 -Wall -g " (buffer-name)
		   " -o " (file-name-sans-extension (buffer-name)) ".out"))
		 ((eq system-type 'gnu/linux)
		  (concat "mpicc -O3 -Wall -g " (buffer-name)
		   " -o " (file-name-sans-extension (buffer-name)) ".out")))))
;;;###autoload
(defun sk/tmux-compile-c-hybrid-math ()
  "Compiles the file with OpenMP, MPI and math libraries"
  (interactive)
  (emamux:send-command
   (cond ((eq system-type 'darwin)
		  (concat
		   "/usr/local/openmpi/bin/mpicc -Wall -fopenmp -lgsl -lcblas -llapack -larmadillo -O2 -g "
		   (buffer-name) " -o " (file-name-sans-extension (buffer-name)) ".out"))
		 ((eq system-type 'gnu/linux)
		  (concat "mpicc -Wall -fopenmp -lgsl -lcblas -llapack -larmadillo -O2 -g "
		   (buffer-name) " -o " (file-name-sans-extension (buffer-name)) ".out")))))
;;;###autoload
(defun sk/tmux-compile-c-hybrid-simple ()
  "Compiles the file with OpenMP and MPI"
  (interactive)
  (emamux:send-command
   (cond ((eq system-type 'darwin)
		  (concat "/usr/local/openmpi/bin/mpicc -O3 -Wall -fopenmp -g " (buffer-name)
		   " -o " (file-name-sans-extension (buffer-name)) ".out"))
		 ((eq system-type 'gnu/linux)
		  (concat "mpicc -O3 -Wall -fopenmp -g " (buffer-name)
		   " -o " (file-name-sans-extension (buffer-name)) ".out")))))
;;;###autoload
(defun sk/tmux-compile-c-math (arg)
  "Compiles the file with math libraries"
  (interactive "P")
  (if arg
	  (emamux:send-command
	   (concat "icc -fast -Wall -lgsl -lcblas -llapack -larmadillo -O2 -g " (buffer-name)
			   " -o " (file-name-sans-extension (buffer-name))))
	(emamux:send-command
	 (concat "gcc -Wall -lgsl -lcblas -llapack -larmadillo -O2 -g " (buffer-name)
			 " -o " (file-name-sans-extension (buffer-name))))))
;;;###autoload
(defun sk/tmux-compile-c-simple (arg)
  "Compiles the file"
  (interactive "P")
  (if arg
	  (emamux:send-command
	   (concat "icc -fast -O3 -Wall -g " (buffer-name)
			   " -o " (file-name-sans-extension (buffer-name))))
	(emamux:send-command
	 (concat "gcc -O3 -Wall -g " (buffer-name)
			 " -o " (file-name-sans-extension (buffer-name))))))
;;;###autoload
(defun sk/tmux-compile-c-run (arg)
  "Runs the compiled file"
  (interactive
   (list
	(read-string "Enter a command line argument (if any): ")))
  (emamux:send-command
   (concat "clear && ./" (file-name-sans-extension (buffer-name)) " " arg)))
;;;###autoload
(defun sk/tmux-compile-c-mpirun (arg)
  "Runs the compiled file"
  (interactive
   (list
	(read-string "Enter the number of processes (if necessary): ")))
  (cond ((eq system-type 'darwin)
		 (emamux:send-command
		  (concat "clear && /usr/local/openmpi/bin/mpirun -np " arg
				  " ./" (file-name-sans-extension (buffer-name)) ".out")))
		((eq system-type 'gnu/linux)
		 (concat "clear && mpirun -np " arg
				 " ./" (file-name-sans-extension (buffer-name)) ".out"))))

;; setup c++ mode
(use-package cc-mode
  :mode (("\\.cpp\\'"	. c++-mode)
		 ("\\.h\\'"		. c-mode)
		 ("\\.c\\'"		. c-mode)
		 ("\\.java\\'"	. java-mode))

  :config
  ;; c++ indexer and semantics
  (use-package rtags
	:ensure t
	:demand t
	:bind* (("C-t r"	. company-rtags)
			("C-t C-r"	. company-rtags))
	:init
	(setq rtags-autostart-diagnostics t)
    (setq rtags-rc-log-enabled t)
	(setq rtags-completions-enabled t)
	:config
	(require 'company-rtags)

	;; ryo bindings
	(ryo-modal-key "m j j" 'rtags-find-symbol-at-point :mode 'c-mode)
	(ryo-modal-key "m j a" 'rtags-find-references :mode 'c-mode)
	(ryo-modal-key "m j r" 'rtags-find-references-at-point :mode 'c-mode)
	(ryo-modal-key "m j s" 'rtags-find-symbol :mode 'c-mode)
	(ryo-modal-key "m j v" 'rtags-find-virtuals-at-point :mode 'c-mode)
	(ryo-modal-key "m b" 'rtags-location-stack-back :mode 'c-mode)
	(ryo-modal-key "m d" 'rtags-print-symbol-info :mode 'c-mode)
	(ryo-modal-key "m e" 'rtags-print-enum-value-at-point :mode 'c-mode)
	(ryo-modal-key "m f" 'rtags-location-stack-forward :mode 'c-mode)
	(ryo-modal-key "m i" 'rtags-print-dependencies :mode 'c-mode)
	(ryo-modal-key "m l" 'rtags-diagnostics :mode 'c-mode)
	(ryo-modal-key "m n" 'rtags-next-match :mode 'c-mode)
	(ryo-modal-key "m p" 'rtags-previous-match :mode 'c-mode)
	(ryo-modal-key "m w" 'rtags-symbol-type :mode 'c-mode)
	(ryo-modal-key "m h" 'rtags-fixit :mode 'c-mode)
	(ryo-modal-key "m k" 'rtags-rename-symbol :mode 'c-mode)
	(ryo-modal-key "m z" 'rtags-preprocess-file :mode 'c-mode))

  ;; c++ navigation based on clang
  (use-package irony
	:ensure t
	:config
	(unless (package-installed-p 'irony)
	  (irony-mode)
	  (irony-install-server))

	;; bindings
	(ryo-modal-key "m v" 'irony-get-type :mode 'c++-mode))

  ;; General C++ completion
  (use-package company-irony
	:ensure t
	:demand t
	:diminish irony-mode
	:bind* (("C-t o"	. company-irony)
			("C-t C-o"	. company-irony))
	:config
	(unless (package-installed-p 'irony)
	  (irony-mode)
	  (irony-install-server))
	(irony-mode))

  ;; C++ header completion
  (use-package company-irony-c-headers
	:ensure t
	:demand t
	:bind* (("C-t i" . company-irony-c-headers)
			("C-t C-i" . company-irony-c-headers))
	:config
	(unless (package-installed-p 'irony)
	  (irony-mode)
	  (irony-install-server)))

  ;; convert emacs into a c ide based on cmake
  (use-package cmake-ide
  	:ensure t
  	:demand t
  	:config
  	(cmake-ide-setup)
  	;; ryo bindings
  	(ryo-modal-key "m c c" 'cmake-ide-compile :mode 'c-mode)
  	(ryo-modal-key "m c f" 'cmake-ide-delete-file :mode 'c-mode)
  	(ryo-modal-key "m c m" 'cmake-ide-maybe-run-cmake :mode 'c-mode)
  	(ryo-modal-key "m c r" 'cmake-ide-maybe-start-rdm :mode 'c-mode)
  	(ryo-modal-key "m c d" 'cmake-ide-load-db :mode 'c-mode))

  ;; compile bindings
  (ryo-modal-key "m r" 'sk/compile-c-run :mode 'c-mode)
  (ryo-modal-key "m x" 'sk/compile-c-mpirun :mode 'c-mode)
  (ryo-modal-key "m m b" 'sk/compile-c-build :mode 'c-mode)
  (ryo-modal-key "m m m" 'sk/compile-c-make :mode 'c-mode)
  (ryo-modal-key "m m d" 'sk/compile-c-build-doc :mode 'c-mode)
  (ryo-modal-key "m m D" 'sk/compile-c-make-doc :mode 'c-mode)
  (ryo-modal-key "m m C" 'sk/compile-c-math :mode 'c-mode)
  (ryo-modal-key "m m H" 'sk/compile-c-hybrid-math :mode 'c-mode)
  (ryo-modal-key "m m I" 'sk/compile-c-mpi-math :mode 'c-mode)
  (ryo-modal-key "m m O" 'sk/compile-c-omp-math :mode 'c-mode)
  (ryo-modal-key "m m c" 'sk/compile-c-simple :mode 'c-mode)
  (ryo-modal-key "m m h" 'sk/compile-c-hybrid-simple :mode 'c-mode)
  (ryo-modal-key "m m i" 'sk/compile-c-mpi-simple :mode 'c-mode)
  (ryo-modal-key "m m o" 'sk/compile-c-omp-simple :mode 'c-mode)

  ;; tmux compile bindings
  (ryo-modal-key "SPC m r" 'sk/tmux-compile-c-run :mode 'c-mode)
  (ryo-modal-key "SPC m m b" 'sk/tmux-compile-c-build :mode 'c-mode)
  (ryo-modal-key "SPC m m m" 'sk/tmux-compile-c-make :mode 'c-mode)
  (ryo-modal-key "SPC m m d" 'sk/tmux-compile-c-build-doc :mode 'c-mode)
  (ryo-modal-key "SPC m m D" 'sk/tmux-compile-c-make-doc :mode 'c-mode)
  (ryo-modal-key "SPC m m C" 'sk/tmux-compile-c-math :mode 'c-mode)
  (ryo-modal-key "SPC m m H" 'sk/tmux-compile-c-hybrid-math :mode 'c-mode)
  (ryo-modal-key "SPC m m I" 'sk/tmux-compile-c-mpi-math :mode 'c-mode)
  (ryo-modal-key "SPC m m O" 'sk/tmux-compile-c-omp-math :mode 'c-mode)
  (ryo-modal-key "SPC m m c" 'sk/tmux-compile-c-simple :mode 'c-mode)
  (ryo-modal-key "SPC m m h" 'sk/tmux-compile-c-hybrid-simple :mode 'c-mode)
  (ryo-modal-key "SPC m m i" 'sk/tmux-compile-c-mpi-simple :mode 'c-mode)
  (ryo-modal-key "SPC m m o" 'sk/tmux-compile-c-omp-simple :mode 'c-mode)

  ;; disassemble code
  (use-package disaster
	:ensure t
	:commands (disaster)
	:demand t
	:config
	(ryo-modal-key "m a" 'disaster :mode 'c-mode))

  ;; clang format
  (use-package clang-format
	:ensure t
	:commands (clang-format)
	:demand t
	:config
	(ryo-modal-key "m q" 'clang-format-region :mode 'c-mode)))

;; cmake files syntax
(use-package cmake-mode
  :ensure t
  :mode (("\\.cmake\\'"	    . cmake-mode)
		 ("CMakeLists.txt"	. cmake-mode)))

;; major mode bindings
(which-key-add-major-mode-key-based-replacements 'c-mode
  "m j" "jump"
  "m j j" "jump to symbol"
  "m j s" "find symbol"
  "m j a" "find references"
  "m j r" "reference at point"
  "m j v" "virtual at point"
  "m d" "symbol info"
  "m i" "print dependencies"
  "m w" "type"
  "m e" "enum value"
  "m f" "forward stack"
  "m b" "backward stack"
  "m n" "next match"
  "m p" "previous match"
  "m l" "list diagnostics"
  "m h" "fixit"
  "m k" "rename symbol"
  "m z" "preprocess file"
  "m a" "dissasemble code"
  "m r" "run binary"
  "m x" "mpi exec"
  "m q" "format region"

  ;; cmake bindings
  "m c" "cmake"
  "m c c" "compile"
  "m c f" "delete file"
  "m c m" "cmake run"
  "m c r" "rdm start"
  "m c d" "load db"

  ;; compile bindings
  "m m" "make/build"
  "m m b" "build"
  "m m m" "make"
  "m m d" "build doc"
  "m m l" "make doc"
  "m m c" "simple"
  "m m h" "hybrid"
  "m m i" "mpi"
  "m m o" "omp"
  "m m C" "simple w/ math"
  "m m H" "hybrid w/ math"
  "m m I" "mpi w/ math"
  "m m O" "omp w/ math")

;; major mode bindings
(which-key-add-major-mode-key-based-replacements 'c-mode
  ;; compile bindings
  "SPC m r" "run"
  "SPC m m" "make/build"
  "SPC m m b" "build"
  "SPC m m m" "make"
  "SPC m m d" "build doc"
  "SPC m m l" "make doc"
  "SPC m m c" "simple"
  "SPC m m h" "hybrid"
  "SPC m m i" "mpi"
  "SPC m m o" "omp"
  "SPC m m C" "simple w/ math"
  "SPC m m H" "hybrid w/ math"
  "SPC m m I" "mpi w/ math"
  "SPC m m O" "omp w/ math")

;; provide the C configuration
(provide 'sk-c)
