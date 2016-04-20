;;; sk-visual.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Visual aids

;;; Code:

;; Volatile highlights
(sk/require-package 'volatile-highlights)
(add-hook 'after-init-hook 'volatile-highlights-mode)

;; Column force as a lookout for length
(sk/require-package 'column-enforce-mode)
(setq column-enforce-column 99)
(add-hook 'prog-mode-hook 'column-enforce-mode)

;; Highlight indentation
(sk/require-package 'highlight-indentation)

;; FCI to display margin
(sk/require-package 'fill-column-indicator)
(setq fci-rule-width 5
      fci-rule-column 79)

;; Delete only changed trailing whitespace on save
(sk/require-package 'ws-butler)
(ws-butler-global-mode)

;; Region information
(sk/require-package 'region-state)
(region-state-mode)

;; Aggressive indent
(sk/require-package 'aggressive-indent)
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)
(add-hook 'ruby-mode-hook #'aggressive-indent-mode)
(add-hook 'cc-mode-hook #'aggressive-indent-mode)

;; Smart tabs
(sk/require-package 'smart-tab)
(global-smart-tab-mode)

;; Origami mode
(sk/require-package 'origami)
(global-origami-mode)

;; Vimish-fold mode
(sk/require-package 'vimish-fold)

;; aux requirements
(require 'sk-visual-hydra)
(require 'sk-visual-diminish)

(provide 'sk-visual)
;;; sk-visual.el ends here
