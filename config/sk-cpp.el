;;; sk-cpp.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; For C++

;;; Code:

;; Irony mode for C++
(sk/require-package 'irony)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun sk/irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook  'sk/irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; Cmake ide - for cmake projects
(sk/require-package 'cmake-ide)
(cmake-ide-setup)

;; Multiple compilation commands
(defun sk/compile-cpp-omp-math ()
  "Compiles the file with OpenMP and math libraries"
  (interactive)
  (compile
   (concat "g++ -Wall -fopenmp -lgsl -lcblas -llapack -O2 -g -std=c++11 " (buffer-file-name) " -o " (file-name-sans-extension buffer-file-name) ".out")
   ))
(defun sk/compile-cpp-omp-simple ()
  "Compiles the file with OpenMP"
  (interactive)
  (compile
   (concat "g++ -Wall -fopenmp -g -std=c++11 " (buffer-file-name) " -o " (file-name-sans-extension buffer-file-name) ".out")
   ))
(defun sk/compile-cpp-mpi-math ()
  "Compiles the file with MPI and math libraries"
  (interactive)
  (compile
   (concat "/usr/local/openmpi/bin/mpic++ -Wall -lgsl -lcblas -llapack -larmadillo -O2 -g -std=c++11 " (buffer-file-name) " -o " (file-name-sans-extension buffer-file-name) ".out")
   ))
(defun sk/compile-cpp-mpi-simple ()
  "Compiles the file with MPI"
  (interactive)
  (compile
   (concat "/usr/local/openmpi/bin/c++ -Wall -g -std=c++11 " (buffer-file-name) " -o " (file-name-sans-extension buffer-file-name) ".out")
   ))
(defun sk/compile-cpp-hybrid-math ()
  "Compiles the file with OpenMP, MPI and math libraries"
  (interactive)
  (compile
   (concat "/usr/local/openmpi/bin/c++ -Wall -fopenmp -lgsl -lcblas -llapack -larmadillo -O2 -g -std=c++11 " (buffer-file-name) " -o " (file-name-sans-extension buffer-file-name) ".out")
   ))
(defun sk/compile-cpp-hybrid-simple ()
  "Compiles the file with OpenMP and MPI"
  (interactive)
  (compile
   (concat "/usr/local/openmpi/bin/c++ -Wall -fopenmp -g -std=c++11 " (buffer-file-name) " -o " (file-name-sans-extension buffer-file-name) ".out")
   ))
(defun sk/compile-cpp-math ()
  "Compiles the file with math libraries"
  (interactive)
  (compile
   (concat "g++ -Wall -lgsl -lcblas -llapack -larmadillo -O2 -g -std=c++11 " (buffer-file-name) " -o " (file-name-sans-extension buffer-file-name) ".out")
   ))
(defun sk/compile-cpp-simple ()
  "Compiles the file"
  (interactive)
  (compile
   (concat "g++ -Wall -g -std=c++11 " (buffer-file-name) " -o " (file-name-sans-extension buffer-file-name) ".out")
   ))

;; Rtags
(sk/require-package 'rtags)
(add-hook 'c-mode-common-hook 'rtags-start-process-unless-running)
(add-hook 'c++-mode-common-hook 'rtags-start-process-unless-running)
(setq rtags-autostart-diagnostics t)
(setq rtags-completions-enabled t)

;; aux requirements
(require 'sk-cpp-hydra)

(provide 'sk-cpp)
;;; sk-cpp.el ends here
