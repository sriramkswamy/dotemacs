;;; init.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Here be dragons

;;; Code:

;; Increase the garbage collection threshold to 500 MB to ease startup
(setq gc-cons-threshold (* 500 1024 1024))

;; List package archives and initialize them
(require 'package)
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Make sure Org is installed
(unless (package-installed-p 'org)
    (package-refresh-contents)
    (package-install 'org))

;; Org plus contrib needs to be loaded before any org related functionality is called
(unless (package-installed-p 'org-plus-contrib)
    (package-refresh-contents)
    (package-install 'org-plus-contrib))

;; Load config.org - my Emacs configuration
(org-babel-load-file (concat user-emacs-directory "config.org"))

;; Garbage collector - decrease threshold to 5 MB
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 5 1024 1024))))
  ;;; init.el ends here
