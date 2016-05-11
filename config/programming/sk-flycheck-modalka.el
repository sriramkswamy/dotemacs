;;; sk-flycheck-modalka.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Modalka bindings

;;; Code:

(modalka-define-kbd "] l" "C-c f n")
(modalka-define-kbd "[ l" "C-c f p")
(modalka-define-kbd "SPC l" "C-c f l")

;; aux requirements
(require 'sk-flycheck-modalka-which-key)

(provide 'sk-flycheck-modalka)
;;; sk-flycheck-modalka.el ends here
