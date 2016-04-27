;;; sk-navigation-bindings.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Bindings for packages/functions defined in sk-navigation.el

;;; Code:

;; Flyspell
(global-set-key (kbd "C-c v [ s") 'sk/flyspell-goto-previous-error)
(global-set-key (kbd "C-c v ] s") 'flyspell-goto-next-error)

;; Beacon mode
(global-set-key (kbd "C-c v g i") 'beacon-blink)

;; Undo tree
(global-set-key (kbd "C-c v u") 'undo-tree-undo)
(global-set-key (kbd "C-c v C-r") 'undo-tree-redo)
(global-set-key (kbd "C-c v U") 'undo-tree-visualize)

;; Avy
(global-set-key (kbd "C-t") 'avy-goto-char-in-line)
(global-set-key (kbd "M-t") 'avy-goto-line)
(global-set-key (kbd "C-S-t") 'avy-goto-char-2)

;; Highlight stuff
(global-set-key (kbd "C-M-s") 'highlight-symbol)
(global-set-key (kbd "C-M-r") 'highlight-symbol-remove-all)

;; Neotree
(global-set-key (kbd "C-c n") 'neotree-toggle)

;; Ggtags
(global-set-key (kbd "C-c v T") 'ggtags-find-tag-regexp)

;; Back button
(global-set-key (kbd "C-c v C-o") 'back-button-local-backward)
(global-set-key (kbd "C-c v C-i") 'back-button-local-forward)
(global-set-key (kbd "C-c v C-S-o") 'back-button-global-backward)
(global-set-key (kbd "C-c v C-S-i") 'back-button-global-forward)

;; modal bindings
(require 'sk-navigation-modalka)

;; which key for bindings
(require 'sk-navigation-bindings-which-key)

(provide 'sk-navigation-bindings)
;;; sk-navigation-bindings.el ends here
