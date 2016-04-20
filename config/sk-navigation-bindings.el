;;; sk-navigation-bindings.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Bindings for packages/functions defined in sk-navigation.el

;;; Code:

;; Beacon mode
(global-set-key (kbd "C-c v g i") 'beacon-blink)

;; Undo tree
(global-set-key (kbd "C-c v u") 'undo-tree-undo)
(global-set-key (kbd "C-c v C-r") 'undo-tree-redo)
(global-set-key (kbd "C-c v U") 'undo-tree-visualize)

;; Avy
(global-set-key (kbd "C-t") 'avy-goto-char-in-line)
(global-set-key (kbd "M-t") 'avy-goto-line)
(global-set-key (kbd "C-M-t") 'avy-goto-char-2)

;; Swiper, Ivy and Counsel
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-s") 'counsel-ag)
(global-set-key (kbd "C-S-s") 'ivy-resume)
(global-set-key (kbd "C-r") 'counsel-imenu)
(global-set-key (kbd "C-M-r") 'ivy-recentf)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-c s ,") 'counsel-descbinds)
;; Ivy maps
(define-key ivy-minibuffer-map (kbd "C-t") 'ivy-toggle-fuzzy)
(define-key ivy-minibuffer-map (kbd "C-j") 'ivy-done)
(define-key ivy-minibuffer-map (kbd "C-i") 'ivy-alt-done)
(define-key ivy-minibuffer-map (kbd "C-m") 'ivy-alt-done)
(define-key ivy-minibuffer-map (kbd "C-S-m") 'ivy-immediate-done)

;; Swoop
(global-set-key (kbd "C-c v *") 'swoop)
(global-set-key (kbd "C-c v #") 'swoop-pcre-regexp)

;; Ag and Wgrep
(global-set-key (kbd "C-M-s") 'ag)
(global-set-key (kbd "C-M-S-r") 'wgrep-change-to-wgrep-mode)

;; Neotree
(global-set-key (kbd "C-c n") 'neotree-toggle)

;; Spotlight
(global-set-key (kbd "C-S-r") 'spotlight)

;; Ggtags
(global-set-key (kbd "C-c v T") 'ggtags-find-tag-regexp)

;; Back button
(global-set-key (kbd "C-c v C-o") 'back-button-local-backward)
(global-set-key (kbd "C-c v C-i") 'back-button-local-forward)
(global-set-key (kbd "C-c v C-S-o") 'back-button-global-backward)
(global-set-key (kbd "C-c v C-S-i") 'back-button-global-forward)

;; which key for bindings
(require 'sk-navigation-which-key)

(provide 'sk-navigation-bindings)
;;; sk-navigation-bindings.el ends here
