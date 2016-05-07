;;; sk-web-modalka.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Web stuff modal bindings

;;; Code:

;; Emmet expand
(modalka-define-kbd "L" "C-c v C-y")
(modalka-define-kbd "H" "C-c v C-S-y")

;; aux requirements
(require 'sk-web-modalka-which-key)

(provide 'sk-web-modalka)
;;; sk-web-modalka.el ends here
