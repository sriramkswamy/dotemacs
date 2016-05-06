;;; sk-navigation-modalka.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Modalka bindings for packages/functions defined in sk-navigation.el

;;; Code:

;; Flyspell
(modalka-define-kbd "[ s" "C-c v [ s")
(modalka-define-kbd "] s" "C-c v ] s")

;; Beacon mode
(modalka-define-kbd "g i" "C-c v g i")

;; Undo tree
(modalka-define-kbd "u" "C-c v u")
(modalka-define-kbd "U" "C-c v U")
(modalka-define-kbd "r" "C-c v C-r")

;; Avy
(modalka-define-kbd "f" "C-t")
(modalka-define-kbd "F" "M-t")
(modalka-define-kbd "W" "C-S-t")

;; Highlight symbols
(modalka-define-kbd "#" "C-M-s")
(modalka-define-kbd "g r" "C-M-r")

;; FFIP
(modalka-define-kbd "SPC d" "C-c p f")
(modalka-define-kbd "SPC TAB" "C-c p a")

;; Neotree
(modalka-define-kbd "SPC n" "C-c n")

;; swoop
(modalka-define-kbd "*" "C-c v *")

;; Ggtags
(modalka-define-kbd "g t" "C-c G")
(modalka-define-kbd "T" "C-c v T")

;; Perspective mode
(modalka-define-kbd "C s" "C-x x s")
(modalka-define-kbd "C a" "C-x x a")
(modalka-define-kbd "C A" "C-x x A")
(modalka-define-kbd "C b" "C-x x b")
(modalka-define-kbd "C c" "C-x x c")
(modalka-define-kbd "C r" "C-x x r")
(modalka-define-kbd "C k" "C-x x k")
(modalka-define-kbd "C n" "C-x x n")
(modalka-define-kbd "C p" "C-x x p")
(modalka-define-kbd "C i" "C-x x i")
(modalka-define-kbd "C C" "C-x x C-x x")

;; which key for modalka bindings
(require 'sk-navigation-modalka-which-key)

(provide 'sk-navigation-modalka)
;;; sk-navigation-modalka.el ends here
