;;; sk-navigation-modalka-which-key.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Which key explanations for bindings in sk-navigation-modalka.el

;;; Code:

(which-key-add-key-based-replacements
  "[ s" "prev spell error"
  "] s" "next spell error"

  "g i" "beacon blink"

  "u" "undo"
  "r" "redo"
  "U" "undo tree"

  "f" "find in line"
  "F" "find line"
  "w" "sneak"

  "#" "highlight symbols"
  "g r" "remove highlight"

  "!" "shell command"
  "&" "async shell command"
  "g t" "regenerate tags"
  "SPC p" "switch projects"
  "SPC d" "project files"
  "SPC TAB" "alternate file"

  "SPC n" "neotree"

  "T" "global tags"

  ";" "local backward"
  "," "local forward"
  "<" "global backward"
  ">" "global forward"

  "C" "config perspective prefix"
  "C s" "perspective switch"
  "C a" "perspective add buffer"
  "C A" "perspective set buffer"
  "C b" "perspective switch to buffer"
  "C c" "perspective close"
  "C r" "perspective rename"
  "C k" "perspective remove buffer"
  "C n" "perspective next"
  "C p" "perspective prev"
  "C i" "perspective import"
  "C C" "perspective last")

(provide 'sk-navigation-modalka-which-key)
;;; sk-navigation-modalka-which-key.el ends here
