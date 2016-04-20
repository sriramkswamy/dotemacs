;;; sk-editing-hydra-bindings.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Bindings for hydras defined in sk-editing-hydra.el

;;; Code:

(global-set-key (kbd "C-c v @") 'sk/hydra-of-macros/body)

;; Modal bindings
(require 'sk-editing-hydra-modalka)

(provide 'sk-editing-hydra-bindings)
;;; sk-editing-hydra-bindings.el ends here
