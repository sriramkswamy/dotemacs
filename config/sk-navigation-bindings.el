;;; sk-navigation-bindings.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Bindings for packages/functions defined in sk-navigation.el

;;; Code:

;; Flyspell
(global-set-key (kbd "C-c v [ s") 'sk/flyspell-goto-previous-error)
(global-set-key (kbd "C-c v ] s") 'flyspell-goto-next-error)

;; Beacon mode
(global-set-key (kbd "C-c v g i") 'beacon-blink)

;; Undo tree
(global-set-key (kbd "C-c v u") 'undo-tree-undo)
(global-set-key (kbd "C-c v C-r") 'undo-tree-redo)
(global-set-key (kbd "C-c v U") 'undo-tree-visualize)

;; Avy
(global-set-key (kbd "C-t") 'avy-goto-char-in-line)
(global-set-key (kbd "M-t") 'avy-goto-line)
(global-set-key (kbd "C-M-t") 'avy-goto-char-2)

;; Helm
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-M-S-r") 'helm-regexp)
(global-set-key (kbd "C-S-s") 'helm-resume)
(global-set-key (kbd "C-r") 'helm-semantic-or-imenu)
(global-set-key (kbd "C-x l") 'helm-locate)
;; helm maps
(define-key helm-find-files-map (kbd "<return>") 'helm-execute-persistent-action)

;; Helm ag
(global-set-key (kbd "C-s") 'helm-do-ag-this-file)
(global-set-key (kbd "M-s") 'helm-do-ag)

;; Helm descbinds
(global-set-key (kbd "C-c ,") 'helm-descbinds)

;; Helm themes
(global-set-key (kbd "C-c c") 'helm-themes)

;; Helm swoop
(global-set-key (kbd "C-c v *") 'helm-swoop)
(global-set-key (kbd "C-c v #") 'helm-multi-swoop)

;; Ag and Wgrep
(global-set-key (kbd "C-M-s") 'ag)
(global-set-key (kbd "C-M-r") 'wgrep-change-to-wgrep-mode)

;; Helm projectile
(global-set-key (kbd "C-c P") 'helm-projectile)

;; Helm bibtex
(global-set-key (kbd "C-c b") 'helm-bibtex)

;; Neotree
(global-set-key (kbd "C-c n") 'neotree-toggle)

;; Ggtags
(global-set-key (kbd "C-c v T") 'ggtags-find-tag-regexp)

;; Back button
(global-set-key (kbd "C-c v C-o") 'back-button-local-backward)
(global-set-key (kbd "C-c v C-i") 'back-button-local-forward)
(global-set-key (kbd "C-c v C-S-o") 'back-button-global-backward)
(global-set-key (kbd "C-c v C-S-i") 'back-button-global-forward)

;; modal bindings
(require 'sk-navigation-modalka)

;; which key for bindings
(require 'sk-navigation-bindings-which-key)

(provide 'sk-navigation-bindings)
;;; sk-navigation-bindings.el ends here
