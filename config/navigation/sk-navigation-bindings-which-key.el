;;; sk-navigation-bindings-which-key.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Which key explanations for bindings in sk-navigation-bindings.el

;;; Code:

(which-key-add-key-based-replacements
  "C-c v [" "vi previous prefix"
  "C-c v ]" "vi next prefix")

(provide 'sk-navigation-bindings-which-key)
;;; sk-navigation-bindings-which-key.el ends here
