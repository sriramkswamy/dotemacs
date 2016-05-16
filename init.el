;;; init.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Here be dragons

;;; Code:

;; Increase the garbage collection threshold to 200 MB to ease startup
(setq gc-cons-threshold (* 200 1024 1024))

;; Initialize the packages but don't enable them at start
(package-initialize nil)
(setq package-enable-at-startup nil)

;; Load config.org - my Emacs configuration
(org-babel-load-file (concat user-emacs-directory "config.org"))

;; Garbage collector - decrease threshold
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 800 1024))))
;;; init.el ends here
