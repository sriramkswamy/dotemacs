;;; sk-org-hydra-modalka.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Org mode hydras modal bindings

;;; Code:

;; Manipulation hydra
(modalka-define-kbd "o o" "C-c h o o")

;; Todo hydra
(modalka-define-kbd "o D" "C-c h o D")

;; Checkbox hydra
(modalka-define-kbd "o b" "C-c h o b")

;; Blog page hydra
(modalka-define-kbd "o p" "C-c h o p")

;; Property hydra
(modalka-define-kbd "o P" "C-c h o P")

;; Clock hydra
(modalka-define-kbd "o C" "C-c h o C")

;; Table manipulation hydra
(modalka-define-kbd "o m" "C-c h o m")

;; Jump hydra
(modalka-define-kbd "o j" "C-c h o j")

;; Agenda
(modalka-define-kbd "o v" "C-c h o v")

;; Org ref
(modalka-define-kbd "o r" "C-c h o r")

;; which key explanations
(require 'sk-org-hydra-modalka-which-key)

(provide 'sk-org-hydra-modalka)
;;; sk-org-hydra-modalka.el ends here
