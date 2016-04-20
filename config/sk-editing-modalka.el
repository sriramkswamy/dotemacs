;;; sk-editing-modalka.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; For all kinds of editing

;;; Code:

;; Expand regions
(modalka-define-kbd "a" "C-c e a")
(modalka-define-kbd "A" "C-c e A")
(modalka-define-kbd "i p" "C-c e i p")
(modalka-define-kbd "i f" "C-c e i f")
(modalka-define-kbd "i w" "C-c e i w")
(modalka-define-kbd "i u" "C-c e i u")
(modalka-define-kbd "i $" "C-c e i $")
(modalka-define-kbd "i c" "C-c e i c")
(modalka-define-kbd "i b" "C-c e i b")
(modalka-define-kbd "i q" "C-c e i q")
(modalka-define-kbd "i o" "C-c e i o")
(modalka-define-kbd "i m" "C-c e i m")
(modalka-define-kbd "i j" "C-c e i j")
(modalka-define-kbd "i v" "C-c e i v")
(modalka-define-kbd "i s" "C-c e i s")

;; Commenting
(modalka-define-kbd "g c" "C-c v g c")

;; Snippets
(modalka-define-kbd "S" "C-c s")

;; Visual regexp
(modalka-define-kbd "SPC s" "C-c v s")

;; which key for bindings
(require 'sk-editing-modalka-which-key)

(provide 'sk-editing-modalka)
;;; sk-editing-modalka.el ends here
