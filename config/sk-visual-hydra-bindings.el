;;; sk-visual-hydra-bindings.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Bindings for visual hydras

;;; Code:

;; Origami
(global-set-key (kbd "C-c v z M") 'origami-toggle-node)

;; Vimish fold
(global-set-key (kbd "C-c v z a") 'vimish-fold-toggle)
(global-set-key (kbd "C-c v z f") 'sk/hydra-vimish-fold)

;; Modal bindings
(require 'sk-visual-hydra-modalka)

;; which key explanations for bindings
(require 'sk-visual-hydra-bindings-which-key)

(provide 'sk-visual-hydra-bindings)
;;; sk-visual-hydra-bindings.el ends here
