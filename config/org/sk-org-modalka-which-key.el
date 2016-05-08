;;; sk-org-modalka-which-key.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; which key for modal bindings

;;; Code:

(which-key-add-key-based-replacements
  "o" "org prefix"
  "o a" "org agenda"
  "o c" "org capture"
  "o i" "org insert link"
  "o s" "org store link"
  "o S" "org subtree from list"
  "o A" "org archive subtree"
  "o g" "org goto"
  "o l" "org latex preview"
  "o L" "org toggle link display"
  "o I" "org image preview"
  "o k" "org kill subtree"
  "o V" "org reveal"
  "o R" "org refile"
  "o y" "org copy subtree"
  "o h" "org toggle heading"
  "o H" "org insert heading"
  "o e" "org export"
  "o u" "org update current"
  "o U" "org update all"
  "o F" "org footnote"
  "o ]" "org narrow subtree"
  "o [" "org widen"
  "o N" "org note"
  "o O" "org open"
  "o F" "org attach"
  "o E" "org set effort"
  "o B" "org table blank field"
  "o <" "org select from cal"
  "o >" "org goto cal"
  "o t" "org tag"
  "o d" "org todo"

  "o f" "org search in files"

  "o n" "interleave notes"
  
  "i *" "org mark subtree")

(provide 'sk-org-modalka-which-key)
;;; sk-org-modalka-which-key.el ends here
