;;; sk-which-key-bindings.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Bindings for which-key functions

;;; Code:

(global-set-key (kbd "C-x ?") 'which-key-show-top-level)

;; Add which key explanations
(require 'sk-which-key-which-key)

(provide 'sk-which-key-bindings)
;;; sk-which-key-bindings.el ends here
