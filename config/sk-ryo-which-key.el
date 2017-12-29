;; basic operators
(which-key-add-key-based-replacements
  "c" "change"
  "C" "change rest of line"
  "c i" "inside"
  "c a" "around"
  "c c" "change line or region"

  "d" "delete"
  "D" "delete rest of line"
  "d a" "inside"
  "d i" "around"
  "d d" "delete line/region"

  "v" "select"
  "V" "start select"
  "v i" "inside"
  "v a" "around"
  "v v" "expand region"
  
  "z" "fold"
  "z i" "inside"
  "z a" "around"
  
  "y" "copy"
  "Y" "copy rest of line"
  "y i" "inside"
  "y a" "around"
  "y y" "copy line/region")

;; complex operators
(which-key-add-key-based-replacements
  "v r" "rectangle mark"
  "v r i" "inside"
  "v r a" "around"

  "g c" "comment"
  "g c i" "inside"
  "g c a" "outside"

  "g u" "downcase"
  "g u i" "inside"
  "g u a" "outside"

  "g U" "upcase"
  "g U i" "inside"
  "g U a" "outside"

  "c y" "change to clipboard"
  "c y a" "inside"
  "c y i" "around"

  "d y" "delete to clipboard"
  "d y i" "inside"
  "d y a" "around")

;; repl operators
(which-key-add-key-based-replacements
  "r w" "shell run"
  "r w i" "inside"
  "r w a" "around"

  "r o" "term run"
  "r o i" "inside"
  "r o a" "outside"

  "r q" "quickrun"
  "r q i" "inside"
  "r q a" "outside"

  "r" "run (default eshell)"
  "r i" "inside"
  "r a" "outside"

  "r u" "tmux run"
  "r u a" "inside"
  "r u i" "around"

  "r s" "tmux send"
  "r s i" "inside"
  "r s a" "around")

;; movement
(which-key-add-key-based-replacements
  "c o" "change option")

;; movement
(which-key-add-key-based-replacements
  "s" "smart/scroll"
  "w" "window")

;; prefixes
(which-key-add-key-based-replacements
  "m" "mode"
  "g" "global"
  "SPC" "leader")

;; general functions
(which-key-add-key-based-replacements
  "." "repeat"
  "ESC" "cancel")

;; provide which key config
(provide 'sk-ryo-which-key)
