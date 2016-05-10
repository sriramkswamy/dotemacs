;;; sk-navigation.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; For all kinds of navigation

;;; Code:

;; Don't lose the cursor
(use-package beacon
  :ensure t
  :defer t
  :commands (beacon-blink)
  :diminish beacon-mode
  :bind (
	 ("C-c v g i" . beacon-blink)
	 )
  :config
  (beacon-mode 1))

;; Undo tree
(use-package undo-tree
  :ensure t
  :commands (undo-tree-undo undo-tree-redo undo-tree-visualize)
  :defer t
  :diminish undo-tree-mode
  :bind (
	 ("C-c v u" . undo-tree-undo)
	 ("C-c v C-r" . undo-tree-redo)
	 ("C-c v U" . undo-tree-visualize)
	 )
  :config
  (global-undo-tree-mode 1))

;; Avy for on-screen motion
(use-package avy
  :ensure t
  :commands (avy-goto-char-in-line avy-goto-char-2 avy-goto-line)
  :defer t
  :init
  (setq avy-keys-alist
	`((avy-goto-char-2 . (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))
	  (avy-goto-char-in-line . (?j ?k ?l ?f ?s ?d))
	  (avy-goto-line . (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))))
  (setq avy-style 'pre)
  :bind (
	 ("C-t" . avy-goto-char-in-line)
	 ("M-t" . avy-goto-line)
	 ("C-S-t" . avy-goto-char-2)
	 ))

;; Highlight current symbol
(use-package highlight-symbol
  :ensure t
  :commands (highlight-symbol)
  :defer t
  :bind (
	 ("C-M-s" . highlight-symbol)
	 ("C-M-r" . highlight-symbol-remove-all)
	 ))

;; Projectile
(use-package projectile
  :ensure t
  :commands (projectile-find-file projectile-find-other-file)
  :defer t
  :init
  (setq projectile-file-exists-remote-cache-expire (* 10 60))
  :diminish projectile-mode
  :config
  (projectile-global-mode))

;; Swoop stuff
(use-package swoop
  :ensure t
  :commands (swoop swoop-pcre-regexp)
  :defer t
  :bind (
	 ("C-c v *" . swoop)
	 ))

;; ag and wgrep
(use-package ag
  :ensure t
  :commands (ag ag-project ag-regexp)
  :bind (
	 ("C-c C-S-s" . ag-regexp)
	 )
  :defer t
  :config
  (use-package wgrep-ag
    :ensure t
    :commands (wgrep-change-to-wgrep-mode)
    :defer t
    :bind (
	   ("C-M-S-r" . wgrep-change-to-wgrep-mode)
	   )))

;; pt and wgrep
(use-package pt
  :ensure t
  :commands (pt-regexp pt-regexp-file-pattern)
  :defer t
  :bind (
	 ("C-c C-s" . pt-regexp)
	 )
  :config
  (use-package wgrep-pt
    :ensure t
    :commands (wgrep-change-to-wgrep-mode)
    :defer t
    :bind (
	   ("C-M-S-r" . wgrep-change-to-wgrep-mode)
	   )))

;; Improve dired
(use-package dired+
  :ensure t
  :defer t)

;; Neotree
(use-package neotree
  :ensure t
  :commands (neotree-toggle neotree-show neotree)
  :defer t
  :bind (
	 ("C-c n". neotree-toggle)
	 )
  :init
  (setq neo-smart-open t))

;; GTags
(use-package ggtags
  :ensure t
  :commands (ggtags-create-tags ggtags-update-tags ggtags-find-tag-regexp)
  :defer t
  :diminish ggtags-mode
  :bind (
	 ("C-c v T" . ggtags-find-tag-regexp)
	 ("C-c G" . ggtags-update-tags))
  :init
  (setq-local imenu-create-index-function #'ggtags-build-imenu-index)
  :config
  (add-hook 'prog-mode-hook 'ggtags-mode))

;; Perspective mode
(use-package perspective
  :ensure t
  :commands (persp-mode)
  :defer t
  :config
  (persp-mode 1))

;; Back button mode for navigation
(use-package back-button
  :commands (back-button-mode)
  :ensure t
  :diminish back-button-mode
  :defer 2
  :init
  (setq back-button-show-toolbar-buttons nil)
  :bind (
	 ("C-c v C-o" . back-button-local-backward)
	 ("C-c v C-S-o" . back-button-global-backward)
	 ("C-c v C-i" . back-button-local-forward)
	 ("C-c v C-S-i" . back-button-global-backward)
	 )
  :config
  (back-button-mode 1))

;; aux requirements
(require 'sk-navigation-modalka)
(require 'sk-navigation-functions)
(require 'sk-navigation-hydra)

(provide 'sk-navigation)
;;; sk-navigation.el ends here
