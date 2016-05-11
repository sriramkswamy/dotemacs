;;; sk-editing-functions-bindings.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Bindings for user defined editing functions

;;; Code:

;; camelCase region
(global-set-key (kbd "C-c v g C") 'sk/toggle-camelcase-underscores)

;; Autocorrect word
(global-set-key (kbd "C-c v g =") #'sk/ispell-word-then-abbrev)

;; Transpose stuff
(global-set-key (kbd "C-c v [ w") 'transpose-words)
(global-set-key (kbd "C-c v ] w") 'sk/transpose-words-forward)
(global-set-key (kbd "C-c v [ c") 'transpose-chars)
(global-set-key (kbd "C-c v ] c") 'sk/transpose-chars-forward)

;; Copy to the end of line
(global-set-key (kbd "C-c v Y") 'sk/copy-to-end-of-line)

;; Select line
(global-set-key (kbd "C-c e i l") 'sk/select-current-line)

;; Vi style open lines
(global-set-key (kbd "C-o") 'sk/open-line-above)

;; Join line
(global-set-key (kbd "C-S-j") 'sk/join-line)

;; Move line/region
(global-set-key (kbd "C-c m k") 'sk/move-text-up)
(global-set-key (kbd "C-c m j") 'sk/move-text-down)

;; Modal bindings
(require 'sk-editing-functions-modalka)

;; which key explanations for bindings
(require 'sk-editing-functions-bindings-which-key)

(provide 'sk-editing-functions-bindings)
;;; sk-editing-functions-bindings.el ends here
