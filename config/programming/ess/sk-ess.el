;;; sk-ess.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; For Python

;;; Code:

(sk/require-package 'ess)
(add-to-list 'auto-mode-alist '("\\.R$" . R-mode))

;; Vertical split julia REPL
(defun sk/julia-shell-here ()
  "opens up a new julia REPL in the directory associated with the current buffer's file."
  (interactive)
  (require 'ess-site)
  (split-window-right)
  (other-window 1)
  (julia)
  (other-window 1))

;; Vertical split r REPL
(defun sk/r-shell-here ()
  "opens up a new r REPL in the directory associated with the current buffer's file."
  (interactive)
  (require 'ess-site)
  (split-window-right)
  (other-window 1)
  (R)
  (other-window 1))

;; aux requirements
(require 'sk-ess-hydra)

(provide 'sk-ess)
;;; sk-ess.el ends here
