;;; sk-editing-functions-modalka.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Modalka bindings for user defined editing functions

;;; Code:

;; Select line
(modalka-define-kbd "i l" "C-c e i l")

;; Vi style open
(modalka-define-kbd "o" "C-S-o")
(modalka-define-kbd "O" "C-o")

;; Join line
(modalka-define-kbd "g J" "C-S-j")

;; Move line/region
(modalka-define-kbd "[ e" "C-c m k")
(modalka-define-kbd "] e" "C-c m j")

;; which key explanations
(require 'sk-editing-functions-modalka-which-key)

(provide 'sk-editing-functions-modalka)
;;; sk-editing-functions-modalka.el ends here
