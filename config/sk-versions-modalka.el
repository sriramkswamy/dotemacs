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
(modalka-define-kbd "g h" "C-c g d h")

;; aux requirements
(require 'sk-versions-modalka-which-key)

(provide 'sk-versions-modalka)
;;; sk-versions-modalka.el ends here
