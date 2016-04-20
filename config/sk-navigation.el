;;; sk-navigation.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; For all kinds of navigation

;;; Code:

;; Don't lose the cursor
(sk/require-package 'beacon)
(add-hook 'after-init-hook 'beacon-mode)

;; Undo tree
(sk/require-package 'undo-tree)
(add-hook 'after-init-hook 'undo-tree-mode)

;; Avy for on-screen motion
(sk/require-package 'avy)
(setq avy-keys-alist
      `((avy-goto-char-2 . (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))
        (avy-goto-char-in-line . (?j ?k ?l ?f ?s ?d))
        (avy-goto-line . (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))))
(setq avy-style 'pre)

;; Swiper, Ivy and Counsel for other kinds of navigation
(sk/require-package 'swiper)
(sk/require-package 'counsel)
(require 'ivy)
(setq ivy-display-style 'fancy
      ivy-height 15
      ivy-initial-inputs-alist nil)
(setq ivy-re-builders-alist
      '((t . ivy--regex-fuzzy)))
(setq completion-in-region-function 'ivy-completion-in-region)
(setq counsel-yank-pop-truncate t)
(add-hook 'after-init-hook 'ivy-mode)
(add-hook 'after-init-hook 'counsel-mode)

;; Ag and Wgrep
(sk/require-package 'ag)
(sk/require-package 'wgrep-ag)

;; Projectile
(sk/require-package 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-file-exists-remote-cache-expire (* 10 60))
(setq projectile-require-project-root nil)
(setq projectile-completion-system 'ivy)

;; Improve dired
(sk/require-package 'dired+)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;; Neotree
(sk/require-package 'neotree)
(setq neo-smart-open t)

;; Spotlight on OS X
(sk/require-package 'spotlight)

;; GTags
(sk/require-package 'ggtags)
(add-hook 'prog-mode-hook 'ggtags-mode)
(setq-local imenu-create-index-function #'ggtags-build-imenu-index)

;; Back button mode
(sk/require-package 'back-button)
(add-hook 'after-init-hook 'back-button-mode)

;; Perspective mode
(sk/require-package 'perspective)
(add-hook 'after-init-hook 'persp-mode)

(provide 'sk-navigation)
;;; sk-navigation.el ends here
