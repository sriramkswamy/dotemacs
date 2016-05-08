;;; sk-editing-modalka-which-key.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; which key explanations for modalka

;;; Code:

(which-key-add-key-based-replacements
  "a" "expand region"
  "A" "contract region"
  "i" "expand region prefix"
  "i p" "expand in para"
  "i f" "expand in func"
  "i w" "expand in word"
  "i u" "expand in url"
  "i $" "expand in latex"
  "i c" "expand in comment"
  "i b" "expand in pair"
  "i q" "expand in quote"
  "i o" "expand in org code"
  "i m" "expand in python"
  "i j" "expand in julia"
  "i v" "expand in symbol"
  "i s" "expand in sentence"
  
  "g c" "comment line/region"

  "s" "smart nav prefix"
  "s >" "forward prefix"
  "s <" "backward prefix"
  "s j" "smart down"
  "s k" "smart backward up"
  "s h" "smart backward down"
  "s l" "smart up"
  "s f" "smart forward"
  "s b" "smart backward"
  "s a" "smart beginning"
  "s e" "smart end"
  "s n" "smart next"
  "s p" "smart previous"
  "s > >" "expression forward barf"
  "s < <" "expression backward barf"
  "s > <" "expression forward slurp"
  "s < >" "expression backward slurp"
  "s x" "smart transpose"
  "s d" "smart delete"
  "s y" "smart copy"
  "s s" "selection unwrap"
  "s S" "backward unwrap"
  "s (" "wrap region with ("
  "s )" "wrap region with )"
  "s [" "wrap region with ["
  "s ]" "wrap region with ]"
  "s {" "wrap region with {"
  "s }" "wrap region with }"
  "s $" "wrap region with $"
  "s '" "wrap region with '"
  "s `" "wrap region with `"
  "s \"" "wrap with \""
  
  "S" "snippets"
  
  "SPC s" "replace word/expression"

  "I" "multi-edit word"

  "m" "multiple cursor prefix"
  "m a" "mark all similar selection"
  "m k" "mark prev similar selection"
  "m j" "mark next similar selection"
  "m h" "skip prev similar selection"
  "m l" "skip next similar selection"
  "m m" "mark extend similar selection"
  "m s" "edit start of selection"
  "m e" "edit end of selection")
  
(provide 'sk-editing-modalka-which-key)
;;; sk-editing-modalka-which-key.el ends here
