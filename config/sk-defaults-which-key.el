;;; sk-defaults-which-key.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Which key for bindings defined in sk-defaults-bindings.el

;;; Code:

(which-key-add-key-based-replacements
  "C-c v" "vi prefix"
  "C-c v g" "vi global prefix")

(provide 'sk-defaults-which-key)
;;; sk-defaults-which-key.el ends here
