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

;; Use package macro for packages
(sk/require-package 'use-package)
(sk/require-package 'diminish)
(eval-when-compile
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

;; Chords support in use-package macro
(use-package use-package-chords
  :ensure t
  :config
  (key-chord-mode 1)
  (key-chord-define-global "JK" 'completion-at-point))

;; Diminish built-in stuff
;; (diminish auto-revert-mode "")
(use-package diminish
  :ensure t
  :diminish (visual-line-mode . "ω")
  :diminish hs-minor-mode
  :diminish abbrev-mode
  :diminish auto-fill-function
  :diminish subword-mode)
(use-package eldoc
  :diminish eldoc-mode)
(use-package flyspell
  :diminish (flyspell-mode . "φ")
  :bind (
	 ("C-c v ] s" . flyspell-goto-next-error)
	 ))

;; Better package menu
(use-package paradox
  :ensure t
  :commands (paradox-list-packages)
  :init
  (setq paradox-github-token t))

;; Profiler
(use-package esup
  :ensure t
  :commands (esup))

;; Restart emacs from emacs
(use-package restart-emacs
  :ensure t
  :commands (restart-emacs))

;; Libraries for emacs
(use-package cl-lib
  :ensure t)
(use-package dash
  :ensure t)
(use-package s
  :ensure t)

(provide 'sk-package)
;;; sk-package.el ends here
