;;; sk-ivy.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Ivy is great!

;;; Code:

;; Ivy, Counsel and Swiper
(use-package ivy
  :ensure t
  :demand t
  :init
  (setq ivy-display-style 'fancy
	ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
	'((t . ivy--regex-fuzzy)))
  (setq completion-in-region-function 'ivy-completion-in-region)
  :diminish ivy-mode
  :bind (
	 ("C-S-s" . ivy-resume)
	 ("C-c r" . ivy-recentf)
	 :map ivy-minibuffer-map
	      ("C-t" . ivy-toggle-fuzzy)
	      ("C-j" . ivy-done)
	      ("C-m" . ivy-alt-done)
	      ("C-S-m" . ivy-immediate-done)
	      ("C-i" . ivy-dispatching-done)
	      ("TAB" . ivy-dispatching-done)
	      )
  :config
  (ivy-mode 1)
  ;; Add swiper
  (use-package swiper
    :ensure t
    :commands (swiper)
    :bind (
	   ("C-s" . swiper)
	   ))
  ;; Search using spotlight
  (use-package spotlight
    :ensure t
    :commands (spotlight
	       spotlight-fast)
    :bind (
	   ("C-c d" . spotlight)
	   ))
  ;; Add counsel to the mix
  (use-package counsel
    :ensure t
    :demand t
    :init
    (setq counsel-yank-pop-truncate t)
    :bind (
	   ("C-r" . counsel-imenu)
	   ("C-x 8" . counsel-unicode-char)
	   ("C-x l" . counsel-locate)
	   ("M-s" . counsel-pt)
	   ("C-s" . counsel-grep-or-swiper)
	   )
    :diminish counsel-mode
    :config
    (counsel-mode 1))
  ;; counsel projectile now
  (use-package counsel-projectile
    :ensure t
    :commands (counsel-projectile)
    :bind (
	   ("C-c P" . counsel-projectile)
	   )
    :init
    (setq projectile-completion-system 'ivy))
  ;; Ivy bibtex
  (use-package ivy-bibtex
    :ensure t
    :commands (ivy-bibtex)
    :bind (
	   ("C-c b" . ivy-bibtex)
	   )
    :init
    (setq bibtex-completion-bibliography '("~/Dropbox/org/references/multiphysics.bib" "~/Dropbox/org/references/chanceconstraints.bib"))
    (setq bibtex-completion-library-path "~/Dropbox/org/references/pdfs")
    (setq bibtex-completion-notes-path "~/Dropbox/org/references/articles.org"))
  )

;; aux requirements
(require 'sk-ivy-modalka)

(provide 'sk-ivy)
;;; sk-ivy.el ends here
