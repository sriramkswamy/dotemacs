;;; sk-editing-functions-modalka.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Modalka bindings for user defined editing functions

;;; Code:

;; Toggle camelCase and snake_case
(modalka-define-kbd "g C" "C-c v g C")

;; Auto correct misspelled words
(modalka-define-kbd "g =" "C-c v g =")

;; Transpose stuff
(modalka-define-kbd "[ w" "C-c v [ w")
(modalka-define-kbd "] w" "C-c v ] w")
(modalka-define-kbd "[ c" "C-c v [ c")
(modalka-define-kbd "] c" "C-c v ] c")

;; Copy to the end of line
(modalka-define-kbd "Y" "C-c v Y")

;; Select line
(modalka-define-kbd "i l" "C-c e i l")

;; Vi style open
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
