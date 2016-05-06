;;; sk-editing-bindings-which-key.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Bindings for editing

;;; Code:

(which-key-add-key-based-replacements
  "C-c s" "smartparens prefix"

  "C-c e" "expand region prefix"
  "C-c e i" "expand region inside prefix"
  
  "C-c M" "multiple cursor prefix")

(provide 'sk-editing-bindings-which-key)
;;; sk-editing-bindings-which-key.el ends here
