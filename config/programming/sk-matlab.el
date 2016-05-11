;;; sk-matlab.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Matlab config

;;; Code:

;; MATLAB mode
(use-package matlab-mode
  :ensure t
  :mode ("\\.m$" . matlab-mode)
  :init
  (setq matlab-shell-command "/Applications/MATLAB_R2014a.app/bin/matlab"
	matlab-indent-function t)
  (eval-after-load 'matlab
    '(add-to-list 'matlab-shell-command-switches "-nosplash")))

;; Vertical split matlab shell
(defun sk/matlab-shell-here ()
  "opens up a new matlab shell in the directory associated with the current buffer's file."
  (interactive)
  (split-window-right)
  (other-window 1)
  (matlab-shell))

;; aux requirements
(require 'sk-matlab-hydra)

(provide 'sk-matlab)
;;; sk-matlab.el ends here
