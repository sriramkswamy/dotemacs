;;; init.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Here be dragons

;;; Code:

;; Load path

;; Package.el
(require 'package)

;; Don't initialize it until I say so
(setq package-enable-at-startup nil)

(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

;; Garbage collector - increase threshold
(setq gc-cons-threshold (* 1024 1024 1024 1024))

;; Now initiliaze packages
(package-initialize)

;; Defaults
(require 'sk-defaults)

;; packages configuration
(require 'sk-package)

;; Diminish minor modes
(require 'sk-diminish)

;; Discover bindings
(require 'sk-which-key)

;; Modal editing
(require 'sk-modalka)

;; Sticky bindings
(require 'sk-hydra)

;; Navigation using search/completion/motion
(require 'sk-navigation)

;; Garbage collector - decrease threshold
(setq gc-cons-threshold (* 1024 1024 1024))
;;; init.el ends here
