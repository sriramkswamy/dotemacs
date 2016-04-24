;;; sk-helm-navigation-bindings.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Bindings for helm commands

;;; Code:

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
(global-set-key (kbd "M-s") 'helm-do-ag-project-root)

;; Helm descbinds
(global-set-key (kbd "C-c ,") 'helm-descbinds)

;; Helm themes
(global-set-key (kbd "C-c c") 'helm-themes)

;; Helm swoop
(global-set-key (kbd "C-s") 'helm-swoop-without-pre-input)
(global-set-key (kbd "C-c v *") 'helm-swoop)

;; Helm bibtex
(global-set-key (kbd "C-c b") 'helm-bibtex)

;; modal bindings
(require 'sk-helm-navigation-modalka)

(provide 'sk-helm-navigation-bindings)
;;; sk-helm-navigation-bindings.el ends here
