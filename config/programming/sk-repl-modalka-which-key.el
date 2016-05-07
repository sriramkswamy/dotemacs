;;; sk-repl-modalka-which-key.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; For REPL modal bindings explanations

;;; Code:

(which-key-add-key-based-replacements
  "SPC u" "vertical eshell"
  "SPC U" "horizontal eshell"

  "SPC m" "vertical multi-term"
  "SPC M" "horizontal mutli-term")

(provide 'sk-repl-modalka-which-key)
;;; sk-repl-modalka-which-key.el ends here
