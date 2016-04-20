;;; sk-hydra-modalka.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Modalka mode bindings for hydras

;;; Code:

(modalka-define-kbd "SPC x" "C-c h h")
(modalka-define-kbd "E" "C-x r R")

;; which key for modalka bindings
(require 'sk-hydra-modalka-which-key)

(provide 'sk-hydra-modalka)
;;; sk-hydra-modalka.el ends here
