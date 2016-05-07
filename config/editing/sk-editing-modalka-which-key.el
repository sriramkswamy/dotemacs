;;; sk-editing-modalka-which-key.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; which key explanations for modalka

;;; Code:

(which-key-add-key-based-replacements
  "a" "expand region"
  "A" "contract region"
  "i" "expand region prefix"
  "i p" "expand inside para"
  "i f" "expand inside func"
  "i w" "expand inside word"
  "i u" "expand inside url"
  "i $" "expand inside latex"
  "i c" "expand inside comment"
  "i b" "expand inside pair"
  "i q" "expand inside quote"
  "i o" "expand inside org code"
  "i m" "expand inside python"
  "i j" "expand inside julia"
  "i v" "expand inside symbol"
  "i s" "expand inside sentence"
  
  "g c" "comment dwim"

  "s" "smartparens prefix"
  "s >" "forward prefix"
  "s <" "backward prefix"
  "s j" "sexp down"
  "s k" "sexp backward up"
  "s h" "sexp backward down"
  "s l" "sexp up"
  "s f" "sexp forward"
  "s b" "sexp backward"
  "s a" "sexp beginning"
  "s e" "sexp end"
  "s n" "sexp next"
  "s p" "sexp previous"
  "s > >" "sexp forward barf"
  "s < <" "sexp backward barf"
  "s > <" "sexp forward slurp"
  "s < >" "sexp backward slurp"
  "s x" "sexp transpose"
  "s d" "sexp kill"
  "s y" "sexp copy"
  "s s" "sexp unwrap"
  "s S" "sexp backward unwrap"
  "s (" "sexp wrap with parens"
  "s )" "sexp wrap with parens"
  "s [" "sexp wrap with brackets"
  "s ]" "sexp wrap with brackets"
  "s {" "sexp wrap with braces"
  "s }" "sexp wrap with braces"
  "s $" "sexp wrap with latex-dollars"
  "s '" "sexp wrap with single-quotes"
  "s `" "sexp wrap with back-quotes"
  "s \"" "sexp wrap with double-quotes"
  
  "S" "snippets"
  
  "SPC s" "visual replace"

  "I" "iedit mode"

  "m" "multiple cursor prefix"
  "m a" "mark all similar selection"
  "m k" "mark prev similar selection"
  "m j" "mark next similar selection"
  "m h" "skip prev similar selection"
  "m l" "skip next similar selection"
  "m m" "mark extend similar selection"
  "m s" "edit start of selected region"
  "m e" "edit end of selected region")
  
(provide 'sk-editing-modalka-which-key)
;;; sk-editing-modalka-which-key.el ends here
