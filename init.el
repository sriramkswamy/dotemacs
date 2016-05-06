;;; init.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Here be dragons

;;; Code:

;; Load path

;; Package.el
(require 'package)

;; Don't initialize it until I say so
(setq package-enable-at-startup nil)

;; Specify load path and custom path
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(let ((default-directory user-emacs-directory))
  (normal-top-level-add-subdirs-to-load-path))

;; Garbage collector - increase threshold by a lot
(setq gc-cons-threshold (* 1024 1024 1024 1024))

;; Now initiliaze packages - 0.8 seconds
(package-initialize)

;; Defaults - 0.8 seconds
(require 'sk-defaults)

;; packages configuration - 0.9 seconds - adds 0.1 seconds
(require 'sk-package)

;; Diminish minor modes - 0.9 seconds
(require 'sk-diminish)

;; Discover bindings - 0.9 seconds
(require 'sk-which-key)

;; Modal editing - 1.0 seconds - adds 0.1 seconds
(require 'sk-modal)

;; Navigation using search/completion/motion - 1.2 seconds - adds 0.2 seconds
(require 'sk-navigation)

;; Editing - 1.5 seconds - adds 0.3 seconds (not sure why)
(require 'sk-editing)

;; Visual helpers - 1.7 seconds - adds 0.2 seconds
(require 'sk-visual)

;; Writing - 1.7 seconds
(require 'sk-writing)

;; Org - you beauty - 1.8 seconds - adds 0.1 seconds
(require 'sk-org)

;; Version control - 1.9 seconds - adds 0.1 seconds
(require 'sk-versions)

;; Programming - 2.4 seconds - adds 0.5 seconds (not sure why)
(require 'sk-programming)

;; ivy - 2.5 seconds - adds 0.1 seconds
(require 'sk-ivy)

;; Fun stuff - 2.6 seconds - adds 0.1 seconds
(require 'sk-fun)

;; Garbage collector - decrease threshold
(setq gc-cons-threshold (* 1024 1024 1024))
;;; init.el ends here
