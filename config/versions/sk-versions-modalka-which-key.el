;;; sk-versions-modalka-which-key.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Modal which key explanations

;;; Code:

;; Magit - Best git wrapper ever
(which-key-add-key-based-replacements
  "g b" "git blame"
  "SPC e" "git status"

  "g l" "git timemachine"
  "g L" "git timemachine switch branch"

  "g p" "gist post"
  "g P" "gist private post"

  "] h" "next git chunk"
  "[ h" "previous git chunk"
  "i h" "mark git chunk"
  "g H" "revert git chunk"
  "g h" "goto git chunk")

(provide 'sk-versions-modalka-which-key)
;;; sk-versions-modalka-which-key.el ends here
