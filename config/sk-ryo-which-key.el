;; basic operators
(which-key-add-key-based-replacements
  "c" "change"
  "C" "change rest of line"
  "c i" "inside"
  "c a" "around"
  "c g" "global"
  "c c" "change line or region"

  "d" "delete"
  "D" "delete rest of line"
  "d a" "inside"
  "d i" "around"
  "d g" "global"
  "d d" "delete line/region"

  "v" "select"
  "V" "start select"
  "v i" "inside"
  "v a" "around"
  "v g" "global"
  "v v" "expand region"
  
  "z" "fold"
  "z i" "inside"
  "z a" "around"
  "z g" "global"
  
  "y" "copy"
  "Y" "copy rest of line"
  "y i" "inside"
  "y a" "around"
  "y g" "global"
  "y y" "copy line/region")

;; alignment
(which-key-add-key-based-replacements
  "g l SPC" "align whitespace"
  "g l SPC i" "inside"
  "g l SPC a" "around"
  "g l SPC g" "global"
  
  "g l s" "align semicolon"
  "g l s i" "inside"
  "g l s a" "around"
  "g l s g" "global"
  
  "g l &" "align ampersand"
  "g l & i" "inside"
  "g l & a" "around"
  "g l & g" "global"
  
  "g l q" "align quote"
  "g l q i" "inside"
  "g l q a" "around"
  "g l q g" "global"
  
  "g l =" "align equal"
  "g l = i" "inside"
  "g l = a" "around"
  "g l = g" "global"
  
  "g l c" "align comma"
  "g l c i" "inside"
  "g l c a" "around"
  "g l c g" "global"
  
  "g l ." "align dot"
  "g l . i" "inside"
  "g l . a" "around"
  "g l . g" "global"
  
  "g l :" "align colon"
  "g l : i" "inside"
  "g l : a" "around"
  "g l : g" "global"
  
  "g l %" "align percent"
  "g l % i" "inside"
  "g l % a" "around"
  "g l % g" "global"
  
  "g l #" "align hash"
  "g l # i" "inside"
  "g l # a" "around"
  "g l # g" "global"
  
  "g l" "align"
  "g l i" "inside"
  "g l a" "around"
  "g l g" "global")

;; complex operators
(which-key-add-key-based-replacements
  "v r" "rectangle mark"
  "v r i" "inside"
  "v r a" "around"
  "v r g" "global"

  "g c" "comment"
  "g c i" "inside"
  "g c a" "outside"
  "g c g" "global"

  "g u" "downcase"
  "g u i" "inside"
  "g u a" "outside"
  "g u g" "global"

  "g U" "upcase"
  "g U i" "inside"
  "g U a" "outside"
  "g U g" "global"

  "c y" "change to clipboard"
  "c y a" "inside"
  "c y i" "around"
  "c y g" "global"

  "d y" "delete to clipboard"
  "d y i" "inside"
  "d y a" "around"
  "d y g" "global")

;; repl operators
(which-key-add-key-based-replacements
  "r w" "shell run"
  "r w i" "inside"
  "r w a" "around"
  "r w g" "global"

  "r o" "term run"
  "r o i" "inside"
  "r o a" "outside"
  "r o g" "global"

  "r q" "quickrun"
  "r q i" "inside"
  "r q a" "outside"
  "r q g" "global"

  "r" "run (default eshell)"
  "r i" "inside"
  "r a" "outside"
  "r g" "global"

  "r u" "tmux run"
  "r u a" "inside"
  "r u i" "around"
  "r u g" "global"

  "r s" "tmux send"
  "r s i" "inside"
  "r s a" "around"
  "r s g" "global")

;; movement
(which-key-add-key-based-replacements
  "v o" "visual option"
  "c o" "change option"
  "y o" "snippet option")

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
