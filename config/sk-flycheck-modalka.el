;;; sk-flycheck-modalka.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Modalka bindings

;;; Code:

(modalka-define-kbd "] l" "C-c ! n")
(modalka-define-kbd "[ l" "C-c ! p")
(modalka-define-kbd "SPC l" "C-c ! l")

;; aux requirements
(require 'sk-flycheck-modalka-which-key)

(provide 'sk-flycheck-modalka)
;;; sk-flycheck-modalka.el ends here
