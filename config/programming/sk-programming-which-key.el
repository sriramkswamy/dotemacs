;;; sk-programming-which-key.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; For prefix explanations

;;; Code:

(which-key-add-key-based-replacements
  "C-c h c" "code prefix")

;; aux requirement
(require 'sk-programming-modalka-which-key)

(provide 'sk-programming-which-key)
;;; sk-programming-which-key.el ends here
