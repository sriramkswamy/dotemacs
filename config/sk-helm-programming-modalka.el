;;; sk-helm-programming-modalka.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Helm programming helpers modal bindings

;;; Code:

;; Error checking
(modalka-define-kbd "SPC l" "C-c f")

;; aux requirements
(require 'sk-helm-programming-modalka-which-key)

(provide 'sk-helm-programming-modalka)
;;; sk-helm-programming-modalka.el ends here
