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

  "y" "copy"
  "Y" "copy rest of line"
  "y i" "inside"
  "y a" "around"
  "y y" "copy line/region")

;; movement
(which-key-add-key-based-replacements
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
