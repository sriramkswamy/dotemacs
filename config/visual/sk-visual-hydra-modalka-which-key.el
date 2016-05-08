;;; sk-visual-hydra-modalka-which-key.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Bindings for visual hydras

;;; Code:

(which-key-add-key-based-replacements
  "g a" "activate modes"

  "-" "selection-based fold toggle"
  "_" "selection-based fold menu")

(provide 'sk-visual-hydra-modalka-which-key)
;;; sk-visual-hydra-modalka-which-key.el ends here
