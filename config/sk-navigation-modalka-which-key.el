;;; sk-navigation-modalka-which-key.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Which key explanations for bindings in sk-navigation-modalka.el

;;; Code:

(which-key-add-key-based-replacements
  "g i" "beacon blink"

  "u" "undo"
  "r" "redo"
  "U" "undo tree"

  "f" "find in line"
  "F" "find line"
  "w" "sneak"

  "s" "swipe"
  "S" "ivy resume"
  "t" "imenu"
  "R" "recentf"
  "g l" "locate"
  "g s s" "counsel ag"
  "SPC j" "counsel M-x"

  "g s r" "ag wgrep"
  "g R" "wgrep mode"

  "!" "shell command"
  "&" "async shell command"
  "g p" "switch projects"
  "g t" "regenerate tags"
  "SPC p" "project files"
  "SPC TAB" "alternate file"

  "SPC n" "neotree"

  "SPC r" "spotlight search"

  "T" "global tags"

  ";" "local backward"
  "," "local forward"
  "<" "global backward"
  ">" "global forward"

  "I" "perspective switch"
  "B" "perspective prefix"
  "B a" "perspective add buffer"
  "B A" "perspective set buffer"
  "B b" "perspective switch to buffer"
  "B r" "perspective rename"
  "B k" "perspective remove buffer"
  "B n" "perspective next"
  "B p" "perspective prev"
  "B i" "perspective import"
  "B B" "perspective last")

(provide 'sk-navigation-modalka-which-key)
;;; sk-navigation-modalka-which-key.el ends here
