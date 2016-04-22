;;; sk-visual-hydra-modalka-which-key.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Bindings for visual hydras

;;; Code:

(which-key-add-key-based-replacements
  "=" "origami toggle"
  
  "g a" "activate modes"

  "-" "vimish fold toggle"
  "_" "vimish fold hydra")

(provide 'sk-visual-hydra-modalka-which-key)
;;; sk-visual-hydra-modalka-which-key.el ends here
