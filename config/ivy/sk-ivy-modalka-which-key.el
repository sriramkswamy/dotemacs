;;; sk-ivy-modalka-which-key.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Which key explanations for bindings in sk-ivy-modalka.el

;;; Code:

(which-key-add-key-based-replacements
  "B" "previous ivy"
  "t" "tags/func in buffer"

  "SPC r" "recent files"

  "SPC y" "locate"

  "SPC t" "spotlight"

  "SPC p" "switch projects"

  "SPC b" "bibliography")

(provide 'sk-ivy-modalka-which-key)
;;; sk-ivy-modalka-which-key.el ends here
