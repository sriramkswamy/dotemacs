;;; sk-navigation-modalka.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Modalka bindings for packages/functions defined in sk-navigation.el

;;; Code:

;; Beacon mode
(modalka-define-kbd "g i" "C-c v g i")

;; Undo tree
(modalka-define-kbd "u" "C-c v u")
(modalka-define-kbd "U" "C-c v U")
(modalka-define-kbd "r" "C-c v C-r")

;; Avy
(modalka-define-kbd "f" "C-t")
(modalka-define-kbd "F" "M-t")
(modalka-define-kbd "w" "C-M-t")

;; Swiper, Ivy and Counsel
(modalka-define-kbd "S" "C-S-s")
(modalka-define-kbd "t" "C-r")
(modalka-define-kbd "R" "C-M-r")
(modalka-define-kbd "g s s" "M-s")
(modalka-define-kbd "g l" "C-x l")
(modalka-define-kbd "SPC ," "C-c s ,")

;; Ag and Wgrep
(modalka-define-kbd "g s r" "C-M-s")
(modalka-define-kbd "g R" "C-M-S-r")

;; Projectile
(modalka-define-kbd "!" "C-c p !")
(modalka-define-kbd "&" "C-c p &")
(modalka-define-kbd "g p" "C-c p p")
(modalka-define-kbd "g t" "C-c p R")
(modalka-define-kbd "SPC p" "C-c p f")
(modalka-define-kbd "SPC TAB" "C-c p a")

;; Neotree
(modalka-define-kbd "SPC n" "C-c n")

;; Spotlight
(modalka-define-kbd "SPC r" "C-S-r")

;; Ggtags
(modalka-define-kbd "T" "C-c v T")

;; Back button
(modalka-define-kbd ";" "C-c v C-o")
(modalka-define-kbd "," "C-c v C-i")
(modalka-define-kbd "<" "C-c v C-S-o")
(modalka-define-kbd ">" "C-c v C-S-i")

;; Perspective mode
(modalka-define-kbd "I" "C-x x s")
(modalka-define-kbd "E s" "C-x x s")
(modalka-define-kbd "E a" "C-x x a")
(modalka-define-kbd "E A" "C-x x A")
(modalka-define-kbd "E b" "C-x x b")
(modalka-define-kbd "E c" "C-x x b")
(modalka-define-kbd "E r" "C-x x r")
(modalka-define-kbd "E k" "C-x x k")
(modalka-define-kbd "E n" "C-x x n")
(modalka-define-kbd "E p" "C-x x p")
(modalka-define-kbd "E i" "C-x x i")
(modalka-define-kbd "E E" "C-x x C-x x")

;; which key for modalka bindings
(require 'sk-navigation-modalka-which-key)

(provide 'sk-navigation-modalka)
;;; sk-navigation-modalka.el ends here
