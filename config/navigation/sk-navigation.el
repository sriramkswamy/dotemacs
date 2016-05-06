;;; sk-navigation.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; For all kinds of navigation

;;; Code:

;; Don't lose the cursor
(sk/require-package 'beacon)
(beacon-mode 1)

;; Undo tree
(sk/require-package 'undo-tree)
(global-undo-tree-mode 1)

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
(add-hook 'prog-mode-hook 'projectile-mode)
(add-hook 'text-mode-hook 'projectile-mode)
(setq projectile-file-exists-remote-cache-expire (* 10 60))

;; Swoop stuff
(sk/require-package 'swoop)

;; ag, pt and wgrep
(sk/require-package 'ag)
(sk/require-package 'wgrep-ag)
(sk/require-package 'pt)
(sk/require-package 'wgrep-pt)

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

;; Perspective mode
(sk/require-package 'perspective)
(persp-mode 1)

;; aux requirements
(require 'sk-navigation-bindings)
(require 'sk-navigation-functions)
(require 'sk-navigation-hydra)
(require 'sk-navigation-diminish)

(provide 'sk-navigation)
;;; sk-navigation.el ends here
