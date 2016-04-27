;;; sk-python.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; For Python

;;; Code:

;; ;; Emacs python helpers
;; (setq python-shell-interpreter "ipython"
;;        python-shell-interpreter-args "-i")

;; Anaconda mode
(sk/require-package 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
;; Diminish anaconda mode
(defun sk/diminish-anaconda ()
  (interactive)
  (diminish 'anaconda-mode ""))
(add-hook 'anaconda-mode-hook 'sk/diminish-anaconda)

;; Cython
(sk/require-package 'cython-mode)

;; Virtual env wrapper
(sk/require-package 'pyenv-mode)

;; Beautify stuff
(sk/require-package 'py-yapf)

;; Testing
(sk/require-package 'pytest)
(sk/require-package 'nose)

;; aux requirements
(require 'sk-python-hydra)

(provide 'sk-python)
;;; sk-python.el ends here
