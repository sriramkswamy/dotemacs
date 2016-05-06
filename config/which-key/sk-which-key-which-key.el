;;; sk-which-key-which-key.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Which key definitions for which key bindings

;;; Code:

(which-key-add-key-based-replacements
  "C-c v" "vi prefix"
  "C-c v g" "vi global prefix"

  "C-x ?" "top level bindings")

(provide 'sk-which-key-which-key)
;;; sk-which-key-which-key.el ends here
