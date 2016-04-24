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

;; Smart tabs
(sk/require-package 'smart-tab)
(global-smart-tab-mode)

;; Origami mode
(sk/require-package 'origami)

;; Vimish-fold mode
(sk/require-package 'vimish-fold)

;; Colorschemes
(sk/require-package 'badwolf-theme)
(sk/require-package 'color-theme-solarized)
(sk/require-package 'gotham-theme)
(sk/require-package 'moe-theme)
(sk/require-package 'monokai-theme)
(sk/require-package 'zenburn-theme)
(sk/require-package 'reykjavik-theme)
(sk/require-package 'railscasts-theme)
(sk/require-package 'caroline-theme)
(sk/require-package 'pastelmac-theme)
(sk/require-package 'white-sand-theme)
(sk/require-package 'paper-theme)
(sk/require-package 'ample-theme)
(sk/require-package 'anti-zenburn-theme)
(load-theme 'ample t)

;; aux requirements
(require 'sk-visual-hydra)
(require 'sk-visual-diminish)

(provide 'sk-visual)
;;; sk-visual.el ends here
