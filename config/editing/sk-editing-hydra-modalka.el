;;; sk-editing-hydra-modalka.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Modalka bindings for editing hydras

;;; Code:

;; Edit rectangle
(modalka-define-kbd "E" "C-x r R")

;; Registers
(modalka-define-kbd "\"" "C-x r \"")

;; Macros
(modalka-define-kbd "@" "C-c v @")

;; which key for modalka
(require 'sk-editing-hydra-modalka-which-key)

(provide 'sk-editing-hydra-modalka)
;;; sk-editing-hydra-modalka.el ends here
