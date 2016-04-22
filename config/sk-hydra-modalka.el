;;; sk-hydra-modalka.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Modalka mode bindings for hydras

;;; Code:

;; Help
(modalka-define-kbd "SPC x" "C-c h h")

;; activate modes
(modalka-define-kbd "g a" "C-c h a")

;; which key for modalka bindings
(require 'sk-hydra-modalka-which-key)

(provide 'sk-hydra-modalka)
;;; sk-hydra-modalka.el ends here
