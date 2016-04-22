;;; sk-hydra-bindings.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs bindings for hydras defined in sk-hydra.el

;;; Code:

;; Help
(global-set-key (kbd "C-c h h") 'sk/hydra-of-help/body)

;; Activation
(global-set-key (kbd "C-c h a") 'sk/hydra-of-activate/body)

;; Modal bindings
(require 'sk-hydra-modalka)

;; which key explanation for bindings
(require 'sk-hydra-which-key)

(provide 'sk-hydra-bindings)
;;; sk-hydra-bindings.el ends here
