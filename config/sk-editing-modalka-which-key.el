;;; sk-editing-modalka-which-key.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; which key explanations for modalka

;;; Code:

(which-key-add-key-based-replacements
  "a" "expand region"
  "A" "contract region"
  "i" "expand region inside prefix"
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
  "s j" "sexp down"
  "s k" "sexp up"
  "s J" "sexp backward down"
  "s K" "sexp backward up"
  "s h" "sexp backward"
  "s l" "sexp forward"
  "s H" "sexp beginning"
  "s L" "sexp end"
  "s n" "sexp next"
  "s p" "sexp previous"
  "s f" "sexp forward barf"
  "s b" "sexp backward barf"
  "s F" "sexp forward slurp"
  "s B" "sexp backward slurp"
  "s N" "symbol next"
  "s P" "symbol previous"
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

  "M" "iedit mode")
  
(provide 'sk-editing-modalka-which-key)
;;; sk-editing-modalka-which-key.el ends here
