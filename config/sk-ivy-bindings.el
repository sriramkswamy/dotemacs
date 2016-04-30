;;; sk-ivy-bindings.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Bindings for ivy commands

;;; Code:

;; Counsel locate
(global-set-key (kbd "C-x l") 'counsel-locate)

;; Counsel pt
(global-set-key (kbd "M-s") 'counsel-pt)

;; counsel descbinds
(global-set-key (kbd "C-c ,") 'counsel-descbinds)

;; counsel theme
(global-set-key (kbd "C-c c") 'counsel-load-theme)

;; Spotlight
(global-set-key (kbd "C-c d") 'spotlight)

;; Counsel projectile
(global-set-key (kbd "C-c P") 'counsel-projectile)

;; Swiper
(global-set-key (kbd "C-s") 'counsel-grep-or-swiper)

;; ivy bibtex
(global-set-key (kbd "C-c b") 'ivy-bibtex)

;; wgrep
(global-set-key (kbd "C-M-S-r") 'wgrep-change-to-wgrep-mode)

;; modal bindings
(require 'sk-ivy-modalka)

(provide 'sk-ivy-bindings)
;;; sk-ivy-bindings.el ends here
