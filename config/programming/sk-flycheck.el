;;; sk-flycheck.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Error checker on the fly

;;; Code:

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :defer 2
  :bind (
	 ("C-c f n" . flycheck-next-error)
	 ("C-c f p" . flycheck-previous-error)
	 ("C-c f l" . flycheck-list-errors))
  :config
  (global-flycheck-mode))

;; aux requirements
(require 'sk-flycheck-modalka)

(provide 'sk-flycheck)
;;; sk-flycheck.el ends here
