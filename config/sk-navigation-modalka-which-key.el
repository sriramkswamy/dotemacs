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

  "s" "swipe"
  "B" "ivy resume"
  "t" "imenu"
  "R" "recentf"
  "SPC r" "locate/spotlight"
  "g s s" "counsel ag"
  "SPC j" "counsel M-x"
  "SPC ," "counsel descbinds"
  "SPC x" "apropos"
  "M" "mark rings"

  "*" "swoop"
  "#" "swoop regexp"

  "g s r" "ag wgrep"
  "g R" "wgrep mode"

  "!" "shell command"
  "&" "async shell command"
  "g p" "switch projects"
  "g t" "regenerate tags"
  "SPC p" "project files"
  "SPC TAB" "alternate file"

  "SPC n" "neotree"

  "T" "global tags"

  ";" "local backward"
  "," "local forward"
  "<" "global backward"
  ">" "global forward"

  "c" "change perspective"
  "C" "config perspective prefix"
  "C a" "perspective add buffer"
  "C A" "perspective set buffer"
  "C b" "perspective switch to buffer"
  "C r" "perspective rename"
  "C k" "perspective remove buffer"
  "C n" "perspective next"
  "C p" "perspective prev"
  "C i" "perspective import"
  "C C" "perspective last")

(provide 'sk-navigation-modalka-which-key)
;;; sk-navigation-modalka-which-key.el ends here
