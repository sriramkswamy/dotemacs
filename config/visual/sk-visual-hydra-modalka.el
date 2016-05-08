;;; sk-visual-hydra-modalka.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Bindings for visual hydras

;;; Code:

;; activate modes
(modalka-define-kbd "g a" "C-c h a")

;; Vimish fold
(modalka-define-kbd "-" "C-c v z a")
(modalka-define-kbd "_" "C-c v z f")

;; which key explanations for bindings
(require 'sk-visual-hydra-modalka-which-key)

(provide 'sk-visual-hydra-modalka)
;;; sk-visual-hydra-modalka.el ends here
