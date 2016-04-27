;;; sk-helm-navigation-modalka.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Modal bindings for helm navigation commands

;;; Code:

;; Helm
(modalka-define-kbd "B" "C-S-s")
(modalka-define-kbd "t" "C-r")
(modalka-define-kbd "g R" "C-M-S-r")
(modalka-define-kbd "SPC r" "C-x l")
(modalka-define-kbd "SPC x" "C-x c a")
(modalka-define-kbd "g M" "C-x c C-c SPC")
(modalka-define-kbd "SPC v" "C-x c F")

;; Helm ag
(modalka-define-kbd "g s" "M-s")

;; Helm descbinds
(modalka-define-kbd "SPC ," "C-c ,")

;; Helm swoop
(modalka-define-kbd "*" "C-c v *")

;; Helm bibtex
(modalka-define-kbd "SPC b" "C-c b")

;; modal bindings
(require 'sk-helm-navigation-modalka-which-key)

(provide 'sk-helm-navigation-modalka)
;;; sk-helm-navigation-modalka.el ends here
