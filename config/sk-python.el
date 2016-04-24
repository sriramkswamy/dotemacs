;;; sk-python.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; For Python

;;; Code:

;; Emacs python helpers
(setq python-shell-interpreter "ipython"
       python-shell-interpreter-args "-i")

;; Anaconda mode
(sk/require-package 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

(provide 'sk-python)
;;; sk-python.el ends here
