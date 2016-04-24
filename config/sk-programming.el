;;; sk-programming.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; For all kinds of programming

;;; Code:

;; Indentation and stuff
(sk/require-package 'editorconfig)
(editorconfig-mode 1)

;; C++
(require 'sk-cpp)

;; Python
(require 'sk-python)

;; ESS


;; Go


;; MATLAB


;; ELisp


;; JS/HTML/CSS


;; Error checking
(require 'sk-flycheck)

;; Auto completion


;; aux requirements
(require 'sk-programming-which-key)

(provide 'sk-programming)
;;; sk-programming.el ends here
