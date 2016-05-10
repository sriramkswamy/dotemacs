;;; sk-visual.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Visual aids

;;; Code:

;; Volatile highlights
(use-package volatile-highlights
  :ensure t
  :demand t
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

;; Column force as a lookout for length
(use-package column-enforce-mode
  :ensure t
  :diminish column-enforce-mode
  :init
  (setq column-enforce-column 99)
  :config
  (progn
    (add-hook 'prog-mode-hook 'column-enforce-mode)))

;; Highlight indentation
(use-package highlight-indentation
  :ensure t
  :commands (highlight-indentation-mode))

;; FCI to display margin
(use-package fill-column-indicator
  :ensure t
  :commands (fci-mode)
  :init
  (setq fci-rule-width 5
	fci-rule-column 79))

;; Delete only changed trailing whitespace on save
(use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :config
  (ws-butler-global-mode))

;; Region information
(use-package region-state
  :ensure t
  :config
  (region-state-mode))

;; Smarter tabs
(use-package smart-tab
  :ensure t
  :diminish smart-tab-mode
  :config
  (global-smart-tab-mode))

;; Origami mode
(use-package origami
  :ensure t
  :commands (origami-toggle-node)
  :bind (
	 ("C-c v z M" . orgiami-toggle-node)
	 )
  :config
  (modalka-define-kbd "|" "C-c v z M")
  (which-key-add-key-based-replacements
    "|" "syntax-based fold toggle"))

;; Vimish-fold mode
(use-package vimish-fold
  :ensure t
  :commands (vimish-fold-toggle
	     vimish-fold))

;; Colorschemes
(use-package sk-theme-pack
  :ensure badwolf-theme
  :ensure color-theme-solarized
  :ensure dracula-theme
  :ensure gotham-theme
  :ensure moe-theme
  :ensure monokai-theme
  :ensure zenburn-theme
  :ensure reykjavik-theme
  :ensure railscasts-theme
  :ensure caroline-theme
  :ensure pastelmac-theme
  :ensure white-sand-theme
  :ensure paper-theme
  :ensure ample-theme
  :ensure anti-zenburn-theme
  :ensure color-theme-sanityinc-tomorrow)

;; The modeline
(use-package spaceline
  :ensure t
  :demand t
  :init
  (setq powerline-default-separator 'arrow-fade)
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme))

;; Fancy battery
(use-package fancy-battery
  :ensure t
  :init
  (setq fancy-battery-show-percentage t)
  :config
  (fancy-battery-mode))

;; Fancy weather
(use-package wttrin
  :ensure t
  :commands (wttrin)
  :init
  (setq wttrin-default-cities '("Gainesville"
				"Albuquerque"
				"Chennai"
				"Hyderabad"
				"Columbus"
				"Hillsboro")))

;; Focus mode
(use-package focus
  :ensure t
  :commands (focus-mode)
  :bind (
	 ("C-c v g F" . focus-mode)
	 )
  :config
  (modalka-define-kbd "g F" "C-c v g F")
  (which-key-add-key-based-replacements "g F" "focus"))

;; aux requirements
(require 'sk-visual-hydra)

(provide 'sk-visual)
;;; sk-visual.el ends here
