;;; sk-repl-bindings.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; For REPL bindings

;;; Code:

;; Eshell
(global-set-key (kbd "C-c t e v") 'sk/eshell-vertical)
(global-set-key (kbd "C-c t e h") 'sk/eshell-horizontal)

;; Multi term
(global-set-key (kbd "C-c t m v") 'sk/multi-term-vertical)
(global-set-key (kbd "C-c t m h") 'sk/multi-term-horizontal)

;; aux requirements
(require 'sk-repl-bindings-which-key)

;; Modal bindings
(require 'sk-repl-modalka)

(provide 'sk-repl-bindings)
;;; sk-repl-bindings.el ends here
