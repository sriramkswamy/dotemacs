;;; sk-package.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Package configuration

;;; Code:

;; packages based on versions
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;; Add homebrew packages
(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; Define function for a required package
(defun sk/require-package (package)
  (setq-default highlight-tabs t)
  "Install given PACKAGE."
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

;; Get the proper path
(sk/require-package 'exec-path-from-shell)
;; (exec-path-from-shell-copy-env "PYTHONPATH")
(setq exec-path-from-shell-check-startup-files nil)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Better package menu
(sk/require-package 'paradox)
(setq paradox-github-token t)

;; Profiler
(sk/require-package 'esup)

;; Restart emacs from emacs
(sk/require-package 'restart-emacs)

;; Libraries for emacs
(sk/require-package 'cl-lib)
(sk/require-package 'dash)
(sk/require-package 's)

(provide 'sk-package)
;;; sk-package.el ends here
