;;;###autoload
(defun sk/compile-cpp-make ()
  "Compiles the file using the makefile in the current directory"
  (interactive)
  (compile "make"))
;;;###autoload
(defun sk/compile-cpp-make-doc ()
  "Generates the documentation using the makefile in the current directory"
  (interactive)
  (compile "make doc"))
;;;###autoload
(defun sk/compile-cpp-build ()
  "Compiles the file using the makefile in the build directory"
  (interactive)
  (compile "make -C build"))
;;;###autoload
(defun sk/compile-cpp-build-doc ()
  "Generates the documentation using the makefile in the build directory"
  (interactive)
  (compile "make -C build doc"))
;;;###autoload
(defun sk/compile-cpp-omp-math (arg)
  "Compiles the file with OpenMP and math libraries"
  (interactive "P")
  (if arg
	  (compile
	   (concat "icc -fast -Wall -Wextra -pedantic -fopenmp -lgsl -lcblas -llapack -O3 -g "
			   (buffer-name) " -o " (file-name-sans-extension (buffer-name))))
	(compile
	 (concat "g++ -std=c++14 -Wall -Wextra -pedantic -fopenmp -lgsl -lcblas -llapack -O3 -g "
			 (buffer-name) " -o " (file-name-sans-extension (buffer-name))))))
;;;###autoload
(defun sk/compile-cpp-omp-simple (arg)
  "Compiles the file with OpenMP"
  (interactive "P")
  (if arg
	  (compile
	   (concat "icc -fast -O3 -Wall -Wextra -pedantic -fopenmp -g " (buffer-name)
			   " -o " (file-name-sans-extension (buffer-name))))
	(compile
	 (concat "g++ -std=c++14 -O3 -Wall -Wextra -pedantic -fopenmp -g " (buffer-name)
			 " -o " (file-name-sans-extension (buffer-name))))))
;;;###autoload
(defun sk/compile-cpp-mpi-math ()
  "Compiles the file with MPI and math libraries"
  (interactive)
  (compile
   (cond ((eq system-type 'darwin)
		  (concat
		   "/usr/local/openmpi/bin/mpic++ -Wall -Wextra -pedantic -lgsl -lcblas -llapack -larmadillo -O3 -g "
		   (buffer-name) " -o " (file-name-sans-extension (buffer-name)) ".out"))
		 ((eq system-type 'gnu/linux)
		  (concat "mpic++ -Wall -Wextra -pedantic -lgsl -lcblas -llapack -larmadillo -O3 -g "
				  (buffer-name) " -o " (file-name-sans-extension (buffer-name)) ".out")))))
;;;###autoload
(defun sk/compile-cpp-mpi-simple ()
  "Compiles the file with MPI"
  (interactive)
  (compile
   (cond
	((eq system-type 'darwin)
	 (concat "/usr/local/openmpi/bin/mpic++ -O3 -Wall -Wextra -pedantic -g " (buffer-name)
			 " -o " (file-name-sans-extension (buffer-name)) ".out"))
	((eq system-type 'gnu/linux)
	 (concat "mpic++ -O3 -Wall -Wextra -pedantic -g " (buffer-name)
			 " -o " (file-name-sans-extension (buffer-name)) ".out")))))
;;;###autoload
(defun sk/compile-cpp-hybrid-math ()
  "Compiles the file with OpenMP, MPI and math libraries"
  (interactive)
  (compile
   (cond ((eq system-type 'darwin)
		  (concat
		   "/usr/local/openmpi/bin/mpic++ -Wall -Wextra -pedantic -fopenmp -lgsl -lcblas -llapack -larmadillo -O3 -g "
		   (buffer-name) " -o " (file-name-sans-extension (buffer-name)) ".out"))
		 ((eq system-type 'gnu/linux)
		  (concat
		   "mpic++ -Wall -Wextra -pedantic -fopenmp -lgsl -lcblas -llapack -larmadillo -O3 -g "
		   (buffer-name) " -o " (file-name-sans-extension (buffer-name)) ".out")))))
;;;###autoload
(defun sk/compile-cpp-hybrid-simple ()
  "Compiles the file with OpenMP and MPI"
  (interactive)
  (compile
   (cond ((eq system-type 'darwin)
		  (concat "/usr/local/openmpi/bin/mpic++ -O3 -Wall -Wextra -pedantic -fopenmp -g "
				  (buffer-name) " -o " (file-name-sans-extension (buffer-name)) ".out"))
		 ((eq system-type 'gnu/linux)
		  (concat "mpic++ -O3 -Wall -Wextra -pedantic -fopenmp -g " (buffer-name)
				  " -o " (file-name-sans-extension (buffer-name)) ".out")))))
;;;###autoload
(defun sk/compile-cpp-math (arg)
  "Compiles the file with math libraries"
  (interactive "P")
  (if arg
	  (compile
	   (concat "icc -fast -Wall -Wextra -pedantic -lgsl -lcblas -llapack -larmadillo -O3 -g "
			   (buffer-name) " -o " (file-name-sans-extension (buffer-name))))
	(compile
	 (concat "g++ -std=c++14 -Wall -Wextra -pedantic -lgsl -lcblas -llapack -larmadillo -O3 -g "
			 (buffer-name) " -o " (file-name-sans-extension (buffer-name))))))
;;;###autoload
(defun sk/compile-cpp-simple (arg)
  "Compiles the file"
  (interactive "P")
  (if arg
	  (compile
	   (concat "icc -fast -O3 -Wall -Wextra -pedantic -g " (buffer-name)
			   " -o " (file-name-sans-extension (buffer-name))))
	(compile
	 (concat "g++ -std=c++14 -O3 -Wall -Wextra -pedantic -g " (buffer-name)
			 " -o " (file-name-sans-extension (buffer-name))))))
;;;###autoload
(defun sk/compile-cpp-run (arg)
  "Runs the compiled file"
  (interactive
   (list
	(read-string "Enter a command line argument (if any): ")))
  (async-shell-command
   (concat "./" (file-name-sans-extension (buffer-name)) " " arg)))
;;;###autoload
(defun sk/compile-cpp-mpirun (arg)
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
;; compile functions
;;;###autoload
(defun sk/tmux-compile-cpp-make ()
  "Compiles the file using the makefile in the current directory"
  (interactive)
  (emamux:send-command "make"))
;;;###autoload
(defun sk/tmux-compile-cpp-make-doc ()
  "Generates the documentation using the makefile in the current directory"
  (interactive)
  (emamux:send-command "make doc"))
;;;###autoload
(defun sk/tmux-compile-cpp-build ()
  "Compiles the file using the makefile in the build directory"
  (interactive)
  (emamux:send-command "make -C build"))
;;;###autoload
(defun sk/tmux-compile-cpp-build-doc ()
  "Generates the documentation using the makefile in the build directory"
  (interactive)
  (emamux:send-command "make -C build doc"))
;;;###autoload
(defun sk/tmux-compile-cpp-omp-math (arg)
  "Compiles the file with OpenMP and math libraries"
  (interactive "P")
  (if arg
	  (emamux:send-command
	   (concat "icc -fast -Wall -Wextra -pedantic -fopenmp -lgsl -lcblas -llapack -O3 -g "
			   (buffer-name) " -o " (file-name-sans-extension (buffer-name))))
	(emamux:send-command
	 (concat "g++ -std=c++14 -Wall -Wextra -pedantic -fopenmp -lgsl -lcblas -llapack -O3 -g "
			 (buffer-name) " -o " (file-name-sans-extension (buffer-name))))))
;;;###autoload
(defun sk/tmux-compile-cpp-omp-simple (arg)
  "Compiles the file with OpenMP"
  (interactive "P")
  (if arg
	  (emamux:send-command
	   (concat "icc -fast -O3 -Wall -Wextra -pedantic -fopenmp -g " (buffer-name)
			   " -o " (file-name-sans-extension (buffer-name))))
	(emamux:send-command
	 (concat "g++ -std=c++14 -O3 -Wall -Wextra -pedantic -fopenmp -g " (buffer-name)
			 " -o " (file-name-sans-extension (buffer-name))))))
;;;###autoload
(defun sk/tmux-compile-cpp-mpi-math ()
  "Compiles the file with MPI and math libraries"
  (interactive)
  (emamux:send-command
   (cond ((eq system-type 'darwin)
		  (concat
		   "/usr/local/openmpi/bin/mpic++ -Wall -Wextra -pedantic -lgsl -lcblas -llapack -larmadillo -O3 -g "
		   (buffer-name) " -o " (file-name-sans-extension (buffer-name)) ".out"))
		 ((eq system-type 'gnu/linux)
		  (concat "mpic++ -Wall -Wextra -pedantic -lgsl -lcblas -llapack -larmadillo -O3 -g "
				  (buffer-name) " -o " (file-name-sans-extension (buffer-name)) ".out")))))
;;;###autoload
(defun sk/tmux-compile-cpp-mpi-simple ()
  "Compiles the file with MPI"
  (interactive)
  (emamux:send-command
   (cond ((eq system-type 'darwin)
		  (concat "/usr/local/openmpi/bin/mpic++ -O3 -Wall -Wextra -pedantic -g " (buffer-name)
				  " -o " (file-name-sans-extension (buffer-name)) ".out"))
		 ((eq system-type 'gnu/linux)
		  (concat "mpic++ -O3 -Wall -Wextra -pedantic -g " (buffer-name)
				  " -o " (file-name-sans-extension (buffer-name)) ".out")))))
;;;###autoload
(defun sk/tmux-compile-cpp-hybrid-math ()
  "Compiles the file with OpenMP, MPI and math libraries"
  (interactive)
  (emamux:send-command
   (cond ((eq system-type 'darwin)
		  (concat
		   "/usr/local/openmpi/bin/mpic++ -Wall -Wextra -pedantic -fopenmp -lgsl -lcblas -llapack -larmadillo -O3 -g "
		   (buffer-name) " -o " (file-name-sans-extension (buffer-name)) ".out"))
		 ((eq system-type 'gnu/linux)
		  (concat
		   "mpic++ -Wall -Wextra -pedantic -fopenmp -lgsl -lcblas -llapack -larmadillo -O3 -g "
		   (buffer-name) " -o " (file-name-sans-extension (buffer-name)) ".out")))))
;;;###autoload
(defun sk/tmux-compile-cpp-hybrid-simple ()
  "Compiles the file with OpenMP and MPI"
  (interactive)
  (emamux:send-command
   (cond ((eq system-type 'darwin)
		  (concat "/usr/local/openmpi/bin/mpic++ -O3 -Wall -Wextra -pedantic -fopenmp -g "
				  (buffer-name) " -o " (file-name-sans-extension (buffer-name)) ".out"))
		 ((eq system-type 'gnu/linux)
		  (concat "mpic++ -O3 -Wall -Wextra -pedantic -fopenmp -g " (buffer-name)
				  " -o " (file-name-sans-extension (buffer-name)) ".out")))))
;;;###autoload
(defun sk/tmux-compile-cpp-math (arg)
  "Compiles the file with math libraries"
  (interactive "P")
  (if arg
	  (emamux:send-command
	   (concat "icc -fast -Wall -Wextra -pedantic -lgsl -lcblas -llapack -larmadillo -O3 -g "
			   (buffer-name) " -o " (file-name-sans-extension (buffer-name))))
	(emamux:send-command
	 (concat "g++ -std=c++14 -Wall -Wextra -pedantic -lgsl -lcblas -llapack -larmadillo -O3 -g "
			 (buffer-name) " -o " (file-name-sans-extension (buffer-name))))))
;;;###autoload
(defun sk/tmux-compile-cpp-simple (arg)
  "Compiles the file"
  (interactive "P")
  (if arg
	  (emamux:send-command
	   (concat "icc -fast -O3 -Wall -Wextra -pedantic -g " (buffer-name)
			   " -o " (file-name-sans-extension (buffer-name))))
	(emamux:send-command
	 (concat "g++ -std=c++14 -O3 -Wall -Wextra -pedantic -g " (buffer-name)
			 " -o " (file-name-sans-extension (buffer-name))))))
;;;###autoload
(defun sk/tmux-compile-cpp-run (arg)
  "Runs the compiled file"
  (interactive
   (list
	(read-string "Enter a command line argument (if any): ")))
  (emamux:send-command
   (concat "clear && ./" (file-name-sans-extension (buffer-name)) " " arg)))
;;;###autoload
(defun sk/tmux-compile-cpp-mpirun (arg)
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

;; better syntax highlighting for modern c++
(use-package modern-cpp-font-lock
  :ensure t
  :diminish modern-c++-font-lock-mode
  :hook (c++-mode . modern-c++-font-lock-mode))

;; indexer based on cmake and compile_commands.json
(use-package rtags
  :ensure t
  :ensure-system-package cmake

  :init
  (setq rtags-completions-enabled t)
  :hook ((c++-mode . rtags-start-process-unless-running)
		 (c-mode . rtags-start-process-unless-running))

  :commands
  (rtags-mode
   rtags-fixit
   rtags-imenu
   rtags-tokens
   rtags-install
   rtags-taglist
   rtags-diagnostics
   rtags-quit-rdm
   rtags-find-file
   rtags-next-diag
   rtags-is-running
   rtags-next-match
   rtags-make-member
   rtags-compile-file
   rtags-include-file
   rtags-list-results
   rtags-reparse-file
   rtags-taglist-mode
   rtags-close-taglist
   rtags-previous-diag
   rtags-check-includes
   rtags-previous-match
   rtags-recompile-file
   rtags-unsuspend-file
   rtags-dependency-tree
   rtags-find-references
   rtags-preprocess-file
   rtags-preprocess-mode
   rtags-references-tree
   rtags-restart-process
   rtags-diagnostics-mode
   rtags-clear-diagnostics
   rtags-compilation-flags
   rtags-print-dependencies
   rtags-bury-or-delete
   rtags-post-command-hook
   rtags-maybe-reparse-file
   rtags-update-buffer-list
   rtags-dependency-tree-all
   rtags-remove-other-window
   rtags-dependency-tree-mode
   rtags-references-tree-mode
   rtags-print-class-hierarchy
   rtags-create-doxygen-comment
   rtags-print-current-location
   rtags-restart-tracking-timer
   rtags-update-current-project
   rtags-display-tooltip-function
   rtags-clear-diagnostics-overlays
   rtags-fix-fixit-at-point
   rtags-call-bury-or-delete
   rtags-apply-fixit-at-point
   rtags-find-virtuals-at-point
   rtags-find-references-at-point
   rtags-dependency-tree-find-path
   rtags-dependency-tree-expand-all
   rtags-dependency-tree-next-level
   rtags-references-tree-expand-all
   rtags-references-tree-next-level
   rtags-find-references-current-dir
   rtags-dependency-tree-collapse-all
   rtags-find-references-current-file
   rtags-references-tree-collapse-all
   rtags-restart-find-container-timer
   rtags-clear-all-diagnostics-overlays
   rtags-dependency-tree-expand-current
   rtags-dependency-tree-previous-level
   rtags-references-tree-expand-current
   rtags-references-tree-previous-level
   rtags-dependency-tree-collapse-current
   rtags-references-tree-collapse-current
   rtags-select
   rtags-print-enum-value-at-point
   rtags-find-all-references-at-point
   rtags-copy-and-print-current-location
   rtags-restart-update-current-project-timer
   rtags-dependency-tree-toggle-current-expanded
   rtags-references-tree-toggle-current-expanded
   rtags-stack-cost
   rtags-symbol-info
   rtags-symbol-type
   rtags-suspend-file
   rtags-select-caller
   rtags-stop-diagnostics
   rtags-find-functions-called-by-this-function
   rtags-find-symbol
   rtags-rename-symbol
   rtags-display-summary
   rtags-show-rtags-buffer
   rtags-suspend-all-files
   rtags-select-other-window
   rtags-set-current-project
   rtags-start-process-maybe
   rtags-set-diagnostics-suspended
   rtags-after-save-hook
   rtags-print-symbol-info
   rtags-location-stack-back
   rtags-location-stack-jump
   rtags-list-suspended-files
   rtags-location-stack-reset
   rtags-clear-suspended-files
   rtags-location-stack-filter
   rtags-location-stack-forward
   rtags-print-source-arguments
   rtags-location-stack-visualize
   rtags-enable-standard-keybindings
   rtags-show-in-other-window
   rtags-select-caller-other-window
   rtags-set-periodic-reparse-timeout
   rtags-start-process-unless-running
   rtags-toggle-file-suspended
   rtags-toggle-diagnostics-suspended
   rtags-find-symbol-at-point
   rtags-find-symbol-current-dir
   rtags-find-symbol-current-file
   rtags-display-summary-as-message
   rtags-location-stack-visualize-mode
   rtags-show-target-in-other-window
   rtags-select-and-remove-rtags-buffer
   rtags-cycle-overlays-on-screen
   rtags-get-include-file-for-symbol
   rtags-goto-offset
   rtags-guess-function-at-point)

  :config
  (unless (package-installed-p 'rtags)
	(rtags-install)))

;; rtags auto completion
(use-package company-rtags
  :ensure t
  :hook ((c++-mode . sk/company-clang)
		 (c-mode . sk/company-clang))
  :bind* (("C-j r" . company-rtags))
  :bind (:map c++-mode-map
			  ("C-d" . company-rtags)))

;; irony mode
(use-package irony
  :ensure t
  :ensure-system-package cmake
  :diminish irony-mode
  :hook ((c++-mode . irony-mode)
		 (c-mode . irony-mode))
  :config
  (unless (package-installed-p 'irony)
	(irony-install-server)))

;; irony auto completion
(use-package company-irony
  :ensure t
  :hook ((c++-mode . sk/company-clang)
		 (c-mode . sk/company-clang))
  :bind* (("C-j i" . company-irony)))

;; cc based modes
(use-package cc-mode
  :init
  ;; language server protocol
  (require 'lsp-mode)
  ;; (require 'lsp-common)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "clangd")
					:major-modes '(cc-mode)
					:server-id 'clangd)))

;; c++-mode bindings
(ryo-modal-major-mode-keys
 'c++-mode
 ("m j j" rtags-find-symbol-at-point :name "symbol at point")
 ("m j s" rtags-find-symbol :name "symbol")
 ("m j r" rtags-find-references-at-point :name "references at point")
 ("m j a" rtags-find-all-references-at-point :name "all references at point")
 ("m j l" rtags-find-references :name "list references")
 ("m j v" rtags-find-virtuals-at-point :name "virtuals")
 ("m j f" rtags-find-functions-called-by-this-function :name "functions")

 ("m p" rtags-preprocess-file :name "preprocess file")
 ("m f" rtags-fixit :name "fix it")
 ("m l" rtags-location-stack-visualize :name "location stack")
 ("m b" rtags-location-stack-back :name "jump back")
 ("m t" rtags-symbol-type :name "symbol type")
 ("m d" rtags-symbol-info :name "symbol info")
 ("m e" rtags-print-enum-value-at-point :name "enum value")
 ("m i" rtags-taglist :name "tag list info")

 ("m c f" rtags-compilation-flags :name "compile flags")
 ("m c c" compile :name "compile")
 ("m c r" rtags-compile-file :name "rtags compile")

 ("m a k" rtags-dependency-tree :name "dependency tree")
 ("m a r" rtags-references-tree :name "references tree")
 ("m a a" rtags-dependency-tree-all :name "all dependencies")
 ("m a p" rtags-print-dependencies :name "print dependencies")
 ("m a h" rtags-print-class-hierarchy :name "print class hierarchy"))

;; which key hints
(which-key-add-major-mode-key-based-replacements 'c++-mode
  "m g" "debug"
  "m j" "jump"
  "m a" "analyse"
  "m d" "doc"
  "m c" "compilation"

  "m s" "eval"
  "m s i" "inside"
  "m s a" "around"
  "m s g" "global")

;; provide configuration for all c based languages
(provide 'sk-clang)
