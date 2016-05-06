;;; sk-repl-bindings-which-key.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; REPL bindings explanations

;;; Code:

(which-key-add-key-based-replacements
  "C-c t" "terminal prefix"
  "C-c t e" "eshell prefix"
  "C-c t m" "multi-term prefix")

(provide 'sk-repl-bindings-which-key)
;;; sk-repl-bindings-which-key.el ends here
