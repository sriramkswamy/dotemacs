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
(require 'sk-defaults-bindings)

;; packages configuration
(require 'sk-package)

;; Diminish minor modes
(require 'sk-diminish)

;; Discover bindings
(require 'sk-which-key)
(require 'sk-defaults-which-key)
(require 'sk-which-key-bindings)
(require 'sk-which-key-which-key)
(require 'sk-which-key-diminish)

;; Modal editing
(require 'sk-modalka)
(require 'sk-modalka-bindings)
(require 'sk-modalka-which-key)
(require 'sk-modalka-diminish)

;; Sticky bindings
(require 'sk-hydra)
(require 'sk-hydra-bindings)
(require 'sk-hydra-which-key)
(require 'sk-hydra-modalka)
(require 'sk-hydra-modalka-which-key)

;; Navigation using search/completion/motion
(require 'sk-navigation)
(require 'sk-navigation-bindings)
(require 'sk-navigation-which-key)
(require 'sk-navigation-modalka)
(require 'sk-navigation-modalka-which-key)
(require 'sk-navigation-diminish)
(require 'sk-navigation-functions)
(require 'sk-navigation-functions-bindings)
(require 'sk-navigation-functions-modalka)
(require 'sk-navigation-functions-modalka-which-key)
(require 'sk-navigation-hydra)
(require 'sk-navigation-hydra-bindings)
(require 'sk-navigation-hydra-modalka)
(require 'sk-navigation-hydra-modalka-which-key)

;;; init.el ends here
