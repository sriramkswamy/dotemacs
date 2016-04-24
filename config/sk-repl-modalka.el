;;; sk-repl-modalka.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; For REPL modal bindings

;;; Code:

;; Eshell
(modalka-define-kbd "SPC u" "C-c t e v")
(modalka-define-kbd "SPC U" "C-c t e h")

;; Multi term
(modalka-define-kbd "SPC m" "C-c t m v")
(modalka-define-kbd "SPC M" "C-c t m h")

;; aux requirements
(require 'sk-repl-modalka-which-key)

(provide 'sk-repl-modalka)
;;; sk-repl-modalka.el ends here
