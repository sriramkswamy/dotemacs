;;; sk-editing-hydra-bindings.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Bindings for hydras defined in sk-editing-hydra.el

;;; Code:

;; Rectangle marks
(global-set-key (kbd "C-x r R") 'sk/hydra-rectangle/body)

;; Registers
(global-set-key (kbd "C-x r \"") 'sk/hydra-registers/body)

;; Macros
(global-set-key (kbd "C-c v @") 'sk/hydra-of-macros/body)

;; Modal bindings
(require 'sk-editing-hydra-modalka)

(provide 'sk-editing-hydra-bindings)
;;; sk-editing-hydra-bindings.el ends here
