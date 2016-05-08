;;; sk-which-key.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Discoverability using which-key

;;; Code:

(sk/require-package 'which-key)
(setq which-key-sort-order 'which-key-key-order-alpha)
(global-set-key (kbd "C-x ?") 'which-key-show-top-level)
(which-key-add-key-based-replacements
  "C-x ?" "top level bindings")
(add-hook 'after-init-hook 'which-key-mode)

;; aux requirements
(require 'sk-which-key-bindings)
(require 'sk-which-key-diminish)

(provide 'sk-which-key)
;;; sk-which-key.el ends here
