;;; sk-ess.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; For Python

;;; Code:

(use-package ess
  :ensure t
  :mode (("\\.R$" . R-mode)
	 ("\\.r$" . R-mode)
	 ("\\.jl$'" . julia-mode)))

;; Vertical split julia REPL
(defun sk/julia-shell-here ()
  "opens up a new julia REPL in the directory associated with the current buffer's file."
  (interactive)
  (split-window-right)
  (other-window 1)
  (julia))

;; Vertical split r REPL
(defun sk/r-shell-here ()
  "opens up a new r REPL in the directory associated with the current buffer's file."
  (interactive)
  (split-window-right)
  (other-window 1)
  (R))

;; aux requirements
(require 'sk-ess-hydra)

(provide 'sk-ess)
;;; sk-ess.el ends here
