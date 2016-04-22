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

;; Smartparens
(modalka-define-kbd "s j" "C-c s j")
(modalka-define-kbd "s k" "C-c s k")
(modalka-define-kbd "s J" "C-c s J")
(modalka-define-kbd "s K" "C-c s K")
(modalka-define-kbd "s h" "C-c s h")
(modalka-define-kbd "s l" "C-c s l")
(modalka-define-kbd "s H" "C-c s H")
(modalka-define-kbd "s L" "C-c s L")
(modalka-define-kbd "s n" "C-c s n")
(modalka-define-kbd "s p" "C-c s p")
(modalka-define-kbd "s N" "C-c s N")
(modalka-define-kbd "s P" "C-c s P")
(modalka-define-kbd "s f" "C-c s f")
(modalka-define-kbd "s b" "C-c s b")
(modalka-define-kbd "s F" "C-c s F")
(modalka-define-kbd "s B" "C-c s B")
(modalka-define-kbd "s x" "C-c s x")
(modalka-define-kbd "s d" "C-c s d")
(modalka-define-kbd "s y" "C-c s y")
(modalka-define-kbd "s s" "C-c s s")
(modalka-define-kbd "s S" "C-c s S")
(modalka-define-kbd "s (" "C-c s (")
(modalka-define-kbd "s )" "C-c s )")
(modalka-define-kbd "s [" "C-c s [")
(modalka-define-kbd "s ]" "C-c s ]")
(modalka-define-kbd "s {" "C-c s {")
(modalka-define-kbd "s }" "C-c s }")
(modalka-define-kbd "s $" "C-c s $")
(modalka-define-kbd "s '" "C-c s '")
(modalka-define-kbd "s \"" "C-c s \"")
(modalka-define-kbd "s `" "C-c s `")

;; Snippets
(modalka-define-kbd "S" "C-c S")

;; Visual regexp
(modalka-define-kbd "SPC s" "C-c v s")

;; Iedit mode
(modalka-define-kbd "M" "C-c M")

;; which key for bindings
(require 'sk-editing-modalka-which-key)

(provide 'sk-editing-modalka)
;;; sk-editing-modalka.el ends here
