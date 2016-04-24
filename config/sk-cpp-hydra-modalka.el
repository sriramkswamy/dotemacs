;;; sk-cpp-hydra-modalka.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Hydras bindings for C++

;;; Code:

;; Make hydra
(modalka-define-kbd "m c" "C-c h m c")

;; Code jump hydra
(modalka-define-kbd "c c" "C-c h c c")

;; which key explanations
(require 'sk-cpp-hydra-modalka-which-key)

(provide 'sk-cpp-hydra-modalka)
;;; sk-cpp-hydra-modalka.el ends here
