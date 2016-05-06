;;; sk-navigation-functions-modalka-which-key.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Which key explanations for Modalka bindings in sk-navigation-functions-modalka.el

;;; Code:

;; DocView
(which-key-add-key-based-replacements
  "%" "matching paren"

  "SPC z" "toggle fullscreen"

  "] d" "adjacent pdf next page"
  "[ d" "adjacent pdf prev page"
  
  ">" "unpop mark"

  "g B" "browse file in browser")

(provide 'sk-navigation-functions-modalka-which-key)
;;; sk-navigation-functions-modalka-which-key.el ends here
