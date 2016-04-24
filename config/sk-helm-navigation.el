;;; sk-helm-navigation.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Helm for navigation

;;; Code:

;; Not helm but let's mix it in
(sk/require-package 'flx)

;; Flx matching
(sk/require-package 'helm-flx)
(helm-flx-mode +1)

;; Fuzzier matching
(sk/require-package 'helm-fuzzier)
(helm-fuzzier-mode 1)

;; Helm ag
(sk/require-package 'helm-ag)

;; Helm descbinds
(sk/require-package 'helm-descbinds)

;; Helm theme selection
(sk/require-package 'helm-themes)

;; Swoop
(sk/require-package 'helm-swoop)
(require 'helm-swoop)

;; Helm bibtex
(sk/require-package 'helm-bibtex)
(setq helm-bibtex-bibliography "~/Dropbox/references/references.bib")
(setq helm-bibtex-library-path "~/Dropbox/references/bibtex-pdfs")
(setq helm-bibtex-notes-path "~/Dropbox/references/helm-bibtex-notes")

;; aux requirements
(require 'sk-helm-navigation-bindings)

(provide 'sk-helm-navigation)
;;; sk-helm-navigation.el ends here
