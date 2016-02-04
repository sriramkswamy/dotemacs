;; Eshell
(setq eshell-glob-case-insensitive t
      eshell-scroll-to-bottom-on-input 'this
      eshell-buffer-shorthand t
      eshell-history-size 1024
      eshell-cmpl-ignore-case t
      eshell-aliases-file (concat user-emacs-directory ".eshell-aliases")
      eshell-last-dir-ring-size 512)
(add-hook 'shell-mode-hook 'goto-address-mode)

;; Vertical split eshell
(defun sk/eshell-vertical ()
  "opens up a new shell in the directory associated with the current buffer's file."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (name (car (last (split-string parent "/" t)))))
    (split-window-right)
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))
    (eshell-send-input)))

;; Horizontal split eshell
(defun sk/eshell-horizontal ()
  "opens up a new shell in the directory associated with the current buffer's file."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (name (car (last (split-string parent "/" t)))))
    (split-window-below)
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))
    (eshell-send-input)))

;; Make the compilation window automatically disapper from enberg on #emacs
(setq compilation-finish-functions
      (lambda (buf str)
        (if (null (string-match ".*exited abnormally.*" str))
            ;;no errors, make the compilation window go away in a few seconds
            (progn
              (run-at-time
               "2 sec" nil 'delete-windows-on
               (get-buffer-create "*compilation*"))
              (message "No Compilation Errors!")))))

;; Quickrun
(sk/require-package 'quickrun)

;; Compile and multi-compile
(sk/require-package 'multi-compile)
(setq multi-compile-alist '((c++-mode . (("cpp-omp" . "g++ %file-name -Wall -fopenmp -o -g %file-sans.out")
                                         ("cpp-mpi" . "mpic++ %file-name -o -g %file-sans.out")
                                         ("cpp-g++" . "g++ %file-name -o %file-sans.out")))
                            (c-mode . (("c-omp" . "gcc %file-name -Wall -fopenmp -o -g %file-sans.out")
                                       ("c-mpi" . "mpicc %file-name -o -g %file-sans.out")
                                       ("c-gcc" . "gcc %file-name -o %file-sans.out")))))
(setq multi-compile-completion-system 'default)

;; Multi-term
(sk/require-package 'multi-term)

;; Vertical split multi-term
(defun sk/multi-term-vertical ()
  "opens up a new terminal in the directory associated with the current buffer's file."
  (interactive)
  (split-window-right)
  (other-window 1)
  (multi-term))

;; Horizontal split multi-term
(defun sk/multi-term-horizontal ()
  "opens up a new terminal in the directory associated with the current buffer's file."
  (interactive)
  (split-window-below)
  (other-window 1)
  (multi-term))

;; Interact with Tmux
(sk/require-package 'emamux)

;; Hydra for emamux

(provide 'sk-repl)

;;; sk-repl.el ends here
