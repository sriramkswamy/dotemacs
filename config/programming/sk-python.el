;;; sk-python.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; For Python

;;; Code:

;; Emacs python helpers
(use-package python
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :config
  (setq python-shell-interpreter "ipython"
	python-shell-interpreter-args "-i"))

;; Anaconda mode
(use-package anaconda-mode
  :ensure t
  :diminish anaconda-mode
  :diminish anaconda-eldoc-mode
  :commands (anaconda-mode
	     anaconda-eldoc-mode
	     anaconda-mode-find-assignments
	     anaconda-mode-find-definitions
	     anaconda-mode-find-file
	     anaconda-mode-find-references
	     anaconda-mode-show-doc
	     anaconda-mode-go-back))

;; Virtual env wrapper
(use-package pyenv-mode
  :ensure t
  :commands (pyenv-mode
	     pyenv-mode-set
	     pyenv-mode-unset))

;; Beautify stuff
(use-package py-yapf
  :ensure t
  :commands (py-yapf-buffer
	     py-yapf-enable-on-save))

;; Testing
(use-package pytest
  :ensure t
  :commands (pytest-all
	     pytest-directory
	     pytest-failed
	     pytest-module
	     pytest-one
	     pytest-pdb-all
	     pytest-pdb-directory
	     pytest-pdb-module
	     pytest-pdb-one))

;; aux requirements
(require 'sk-python-hydra)

(provide 'sk-python)
;;; sk-python.el ends here
