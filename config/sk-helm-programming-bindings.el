;;; sk-helm-programming-bindings.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Helm programming helpers bindings

;;; Code:

;; Error checking
(global-set-key (kbd "C-c f") 'helm-flycheck)

;; aux requirements
(require 'sk-helm-programming-modalka)

(provide 'sk-helm-programming-bindings)
;;; sk-helm-programming-bindings.el ends here
