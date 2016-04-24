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

;; Highlight current symbol
(sk/require-package 'highlight-symbol)

;; Projectile
(sk/require-package 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-file-exists-remote-cache-expire (* 10 60))
(setq projectile-completion-system 'helm)

;; Improve dired
(sk/require-package 'dired+)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;; Neotree
(sk/require-package 'neotree)
(setq neo-smart-open t)

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

;; aux requirements
(require 'sk-navigation-bindings)
(require 'sk-navigation-functions)
(require 'sk-navigation-hydra)
(require 'sk-navigation-diminish)

(provide 'sk-navigation)
;;; sk-navigation.el ends here
