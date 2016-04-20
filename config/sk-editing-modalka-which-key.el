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
  
  "S" "snippets"
  
  "SPC s" "visual replace")
  
(provide 'sk-editing-modalka-which-key)
;;; sk-editing-modalka-which-key.el ends here
