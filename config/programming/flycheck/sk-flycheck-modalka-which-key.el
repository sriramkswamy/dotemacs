;;; sk-flycheck-modalka-which-key.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; which key explanations for modalka

;;; Code:

(which-key-add-key-based-replacements
  "] l" "next error"
  "[ l" "previous error"
  "SPC l" "list errors")

(provide 'sk-flycheck-modalka-which-key)
;;; sk-flycheck-modalka-which-key.el ends here
