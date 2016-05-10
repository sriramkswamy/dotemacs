;;; sk-ivy-modalka-which-key.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Which key explanations for bindings in sk-ivy-modalka.el

;;; Code:

(which-key-add-key-based-replacements
  "B" "previous ivy"
  "t" "tags/func in buffer"

  "SPC x" "desc function"
  "SPC v" "desc variable"

  "SPC r" "recent files"

  "SPC y" "locate"

  "SPC t" "spotlight"

  "SPC d" "projectile"

  "SPC p" "counsel projectile"

  "SPC b" "ivy bibtex"

  "SPC ?" "search bindings")

(provide 'sk-ivy-modalka-which-key)
;;; sk-ivy-modalka-which-key.el ends here