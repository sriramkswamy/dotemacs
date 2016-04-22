;;; sk-editing-functions-modalka-which-key.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; which key explanations for modalka bindings in sk-editing-functions-modalka

;;; Code:

(which-key-add-key-based-replacements
  "i l" "expand inside line"
  
  "o" "open line below"
  
  "g J" "join line"
  
  "[ e" "move line up"
  "] e" "move line down")

(provide 'sk-editing-functions-modalka-which-key)
;;; sk-editing-functions-modalka-which-key.el ends here
