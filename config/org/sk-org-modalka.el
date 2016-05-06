;;; sk-org-modalka.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Org mode modal bindings

;;; Code:

;; Default org bindings
(modalka-define-kbd "o a" "C-c O a")
(modalka-define-kbd "o c" "C-c O c")
(modalka-define-kbd "o i" "C-c O i")
(modalka-define-kbd "o s" "C-c O s")
(modalka-define-kbd "o S" "C-c O S")
(modalka-define-kbd "o A" "C-c O A")
(modalka-define-kbd "o g" "C-c O g")
(modalka-define-kbd "o l" "C-c O l")
(modalka-define-kbd "o L" "C-c O L")
(modalka-define-kbd "o I" "C-c O I")
(modalka-define-kbd "o k" "C-c O k")
(modalka-define-kbd "o V" "C-c O V")
(modalka-define-kbd "o R" "C-c O R")
(modalka-define-kbd "o y" "C-c O y")
(modalka-define-kbd "o h" "C-c O h")
(modalka-define-kbd "o H" "C-c O H")
(modalka-define-kbd "o e" "C-c O e")
(modalka-define-kbd "o u" "C-c O u")
(modalka-define-kbd "o U" "C-c O U")
(modalka-define-kbd "o F" "C-c O F")
(modalka-define-kbd "o ]" "C-c O ]")
(modalka-define-kbd "o [" "C-c O [")
(modalka-define-kbd "o N" "C-c O N")
(modalka-define-kbd "o O" "C-c O O")
(modalka-define-kbd "o F" "C-c O F")
(modalka-define-kbd "o E" "C-c O E")
(modalka-define-kbd "o B" "C-c O B")
(modalka-define-kbd "o <" "C-c O <")
(modalka-define-kbd "o >" "C-c O >")
(modalka-define-kbd "o d" "C-c O d")
(modalka-define-kbd "o t" "C-c O t")

;; deft
(modalka-define-kbd "o f" "C-c O f")

;; interleave
(modalka-define-kbd "o n" "C-c O n")

;; mark
(modalka-define-kbd "i *" "C-c O M")

;; which key explanations
(require 'sk-org-modalka-which-key)

(provide 'sk-org-modalka)
;;; sk-org-modalka.el ends here
