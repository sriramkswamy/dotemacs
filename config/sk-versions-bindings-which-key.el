;;; sk-versions-bindings-which-key.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Which key explanations for bindings

;;; Code:

;; Magit - Best git wrapper ever
(which-key-add-key-based-replacements
  "C-c g" "magit prefix"

  "C-c g d" "diff hl prefix")

(provide 'sk-versions-bindings-which-key)
;;; sk-versions-bindings-which-key.el ends here
