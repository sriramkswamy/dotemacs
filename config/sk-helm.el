;;; sk-helm.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Helm is OK

;;; Code:

;; Helm
(sk/require-package 'helm)
(require 'helm)
(require 'helm-config)
(helm-autoresize-mode 1)
(helm-mode 1)

;; Helm for navigation
(require 'sk-helm-navigation)

;; Helm for programming
(require 'sk-helm-programming)

;; Config
(require 'sk-helm-config)

(provide 'sk-helm)
;;; sk-helm.el ends here
