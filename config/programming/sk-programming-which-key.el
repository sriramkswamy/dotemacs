;;; sk-programming-which-key.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; For prefix explanations

;;; Code:

(which-key-add-key-based-replacements
  "C-c h m" "make hydras prefix"
  "C-c h c" "code jump prefix")

;; aux requirement
(require 'sk-programming-modalka-which-key)

(provide 'sk-programming-which-key)
;;; sk-programming-which-key.el ends here
