;;; sk-flycheck-modalka.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Modalka bindings

;;; Code:

(modalka-define-kbd "] l" "C-c ! n")
(modalka-define-kbd "[ l" "C-c ! p")

;; aux requirements
(require 'sk-flycheck-modalka-which-key)

(provide 'sk-flycheck-modalka)
;;; sk-flycheck-modalka.el ends here
