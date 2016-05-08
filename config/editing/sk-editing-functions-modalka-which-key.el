;;; sk-editing-functions-modalka-which-key.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; which key explanations for modalka bindings in sk-editing-functions-modalka

;;; Code:

(which-key-add-key-based-replacements
  "[ w" "exchange with prev word"
  "] w" "exchange with next word"
  "[ c" "exchange with prev char"
  "] c" "exchange with next char"

  "Y" "copy to the end of line"

  "i l" "expand inside line"
  
  "O" "open line above"
  
  "g J" "join line"
  
  "[ e" "move line/region up"
  "] e" "move line/region down")

(provide 'sk-editing-functions-modalka-which-key)
;;; sk-editing-functions-modalka-which-key.el ends here
