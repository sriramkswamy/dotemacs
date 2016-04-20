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

  "E" "perspective prefix"
  "I" "perspective switch"
  "E a" "perspective add buffer"
  "E A" "perspective set buffer"
  "E b" "perspective switch to buffer"
  "E r" "perspective rename"
  "E k" "perspective remove buffer"
  "E n" "perspective next"
  "E p" "perspective prev"
  "E i" "perspective import"
  "E E" "perspective last")

(provide 'sk-navigation-modalka-which-key)
;;; sk-navigation-modalka-which-key.el ends here
