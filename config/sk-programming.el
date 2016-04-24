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
(require 'sk-ess)

;; MATLAB
(require 'sk-matlab)

;; JS/HTML/CSS
(require 'sk-web)

;; Error checking
(require 'sk-flycheck)

;; Auto completion
(require 'sk-company)

;; REPL stuff
(require 'sk-repl)

;; aux requirements
(require 'sk-programming-which-key)

;; start services easily
(sk/require-package 'prodigy)

;; Better debugging
(sk/require-package 'realgud)

(provide 'sk-programming)
;;; sk-programming.el ends here
