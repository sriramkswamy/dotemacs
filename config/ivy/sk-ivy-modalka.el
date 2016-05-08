;;; sk-ivy-modalka.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Modal bindings for ivy commands

;;; Code:

;; Resume
(modalka-define-kbd "B" "C-S-s")

;; imenu
(modalka-define-kbd "t" "C-r")

;; recent files
(modalka-define-kbd "SPC r" "C-c r")

;; Locate
(modalka-define-kbd "SPC y" "C-x l")

;; Help
(modalka-define-kbd "SPC x" "C-h f")
(modalka-define-kbd "SPC v" "C-h v")

;; Pt
(modalka-define-kbd "g s" "M-s")

;; descbinds
(modalka-define-kbd "SPC ?" "C-h b")

;; theme
(modalka-define-kbd "SPC c" "C-c c")

;; Spotlight
(modalka-define-kbd "SPC t" "C-c d")

;; Projecile
(modalka-define-kbd "SPC p" "C-c P")

;; bibtex
(modalka-define-kbd "SPC b" "C-c b")

;; modal bindings
(require 'sk-ivy-modalka-which-key)

(provide 'sk-ivy-modalka)
;;; sk-ivy-modalka.el ends here
