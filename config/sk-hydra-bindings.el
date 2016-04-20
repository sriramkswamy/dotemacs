;;; sk-hydra-bindings.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs bindings for hydras defined in sk-hydra.el

;;; Code:

(global-set-key (kbd "C-c h h") 'sk/hydra-of-help/body)
(global-set-key (kbd "C-x r R") 'sk/hydra-rectangle/body)

;; which key explanation for bindings
(require 'sk-hydra-which-key)

(provide 'sk-hydra-bindings)
;;; sk-hydra-bindings.el ends here
