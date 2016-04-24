;;; sk-helm-navigation-modalka-which-key.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Which key explanations for bindings in sk-navigation-modalka.el

;;; Code:

(which-key-add-key-based-replacements
  "B" "helm resume"
  "t" "imenu"
  "g R" "regexp build"
  "SPC r" "locate/spotlight"
  "SPC j" "helm M x"
  "SPC x" "apropos"
  "g M" "mark rings"

  "SPC d" "helm projectile"

  "*" "helm swoop"

  "SPC b" "helm bibtex"

  "SPC ," "helm descbinds"

  "s" "search"
  "g s s" "helm ag")

(provide 'sk-helm-navigation-modalka-which-key)
;;; sk-helm-navigation-modalka-which-key.el ends here
