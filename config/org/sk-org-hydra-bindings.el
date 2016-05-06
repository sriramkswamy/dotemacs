;;; sk-org-hydra-bindings.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Org mode hydras bindings

;;; Code:

;; Manipulation hydra
(global-set-key (kbd "C-c h o o") 'sk/hydra-org-organize/body)

;; Todo hydra
(global-set-key (kbd "C-c h o D") 'sk/hydra-org-todo/body)

;; Checkbox hydra
(global-set-key (kbd "C-c h o b") 'sk/hydra-org-checkbox/body)

;; Blog page hydra
(global-set-key (kbd "C-c h o p") 'sk/hydra-org-page/body)

;; Property hydra
(global-set-key (kbd "C-c h o P") 'sk/hydra-org-property/body)

;; clock hydra
(global-set-key (kbd "C-c h o C") 'sk/hydra-org-clock/body)

;; Table manipulation hydra
(global-set-key (kbd "C-c h o m") 'sk/hydra-org-tables/body)

;; Jump hydra
(global-set-key (kbd "C-c h o j") 'sk/hydra-org-jump/body)

;; Agenda mode binding
(global-set-key (kbd "C-c h o v") 'sk/hydra-org-agenda-view/body)

;; Org ref
(global-set-key (kbd "C-c h o r") 'sk/hydra-org-ref/body)

;; Modal bindings
(require 'sk-org-hydra-modalka)

;; which key explanations
(require 'sk-org-hydra-bindings-which-key)

(provide 'sk-org-hydra-bindings)
;;; sk-org-hydra-bindings.el ends here
