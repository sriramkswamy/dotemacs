;;; sk-navigation-functions-bindings.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Bindings for user-defined functions

;;; Code:

;; Matching parenthesis
(global-set-key (kbd "C-c v %") 'sk/goto-match-paren)

;; Full screen
(global-set-key (kbd "C-z") 'sk/toggle-frame-fullscreen-non-native)

;; Split windows
(global-set-key (kbd "C-x 2") 'sk/split-below-and-move)
(global-set-key (kbd "C-x 3") 'sk/split-right-and-move)

;; DocView
(global-set-key (kbd "C-S-n") 'sk/other-pdf-next)
(global-set-key (kbd "C-S-p") 'sk/other-pdf-previous)

;; Mark
(global-set-key (kbd "C-c v C-i") 'sk/unpop-to-mark-command)

;; Modal bindings
(require 'sk-navigation-functions-modalka)

(provide 'sk-navigation-functions-bindings)
;;; sk-navigation-functions-bindings.el ends here
