;;; sk-repl.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; For REPL based stuff

;;; Code:

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
              (run-at-time "0.4 sec" nil
                           (lambda ()
                             (select-window (get-buffer-window (get-buffer-create "*compilation*")))
                             (switch-to-buffer nil)))
              (message "No Compilation Errors!")))))

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

;; aux requirements
(require 'sk-repl-bindings)
(require 'sk-repl-hydra)

(provide 'sk-repl)
;;; sk-repl.el ends here
