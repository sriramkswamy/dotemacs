;;; sk-which-key.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Discoverability using which-key

;;; Code:

(use-package which-key
  :ensure t
  :defer t
  :diminish which-key-mode
  :init
  (setq which-key-sort-order 'which-key-key-order-alpha)
  :bind (
	 ("C-x ?" . which-key-show-top-level)
	 )
  :config
  (which-key-mode)
  (which-key-add-key-based-replacements
  "C-x ?" "top level bindings"))

(provide 'sk-which-key)
;;; sk-which-key.el ends here
