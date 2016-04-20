;;; sk-navigation-which-key.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Which key explanations for bindings in sk-navigation-bindings.el

;;; Code:

(which-key-add-key-based-replacements
  "C-c s" "swiper prefix"
  
  "C-c v [" "vi previous prefix"
  "C-c v ]" "vi next prefix")

(provide 'sk-navigation-which-key)
;;; sk-navigation-which-key.el ends here
