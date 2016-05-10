;;; sk-versions-modalka.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Modal bindings

;;; Code:

;; Magit - Best git wrapper ever
(modalka-define-kbd "SPC e" "C-c g s")
(modalka-define-kbd "g b" "C-c g b")

;; Diff hl
(modalka-define-kbd "] h" "C-c g d n")
(modalka-define-kbd "[ h" "C-c g d p")
(modalka-define-kbd "i h" "C-c g d m")
(modalka-define-kbd "g h" "C-c g d g")
(modalka-define-kbd "g H" "C-c g d h")

;; Time machine
(modalka-define-kbd "g l" "C-c g l")
(modalka-define-kbd "g L" "C-c g L")

;; Yagist
(modalka-define-kbd "g p" "C-c g g p")
(modalka-define-kbd "g P" "C-c g g P")

;; aux requirements
(require 'sk-versions-modalka-which-key)

(provide 'sk-versions-modalka)
;;; sk-versions-modalka.el ends here
