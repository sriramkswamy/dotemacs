;;; sk-ivy.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Ivy is great!

;;; Code:

;; Ivy, Counsel and Swiper
(sk/require-package 'ivy)
(sk/require-package 'counsel)
(sk/require-package 'swiper)
(ivy-mode 1)
(counsel-mode 1)

;; Ivy and counsel configuration
(setq ivy-display-style 'fancy
      ivy-initial-inputs-alist nil)
(setq ivy-re-builders-alist
      '((t . ivy--regex-fuzzy)))
(setq completion-in-region-function 'ivy-completion-in-region)
(setq counsel-yank-pop-truncate t)

;; Ivy diminish
(defun sk/diminish-ivy ()
  (interactive)
  (diminish 'ivy-mode " ஐ"))
(add-hook 'ivy-mode-hook 'sk/diminish-ivy)
(add-hook 'after-init-hook 'sk/diminish-ivy)

;; Counsel diminish
(defun sk/diminish-counsel ()
  (interactive)
  (diminish 'counsel-mode ""))
(add-hook 'counsel-mode-hook 'sk/diminish-counsel)
(add-hook 'after-init-hook 'sk/diminish-counsel)

;; Ivy mode maps
(define-key ivy-minibuffer-map (kbd "C-t") 'ivy-toggle-fuzzy)
(define-key ivy-minibuffer-map (kbd "C-j") 'ivy-done)
(define-key ivy-minibuffer-map (kbd "C-m") 'ivy-alt-done)
(define-key ivy-minibuffer-map (kbd "C-S-m") 'ivy-immediate-done)
(define-key ivy-minibuffer-map (kbd "C-i") 'ivy-dispatching-done)
(define-key ivy-minibuffer-map (kbd "TAB") 'ivy-dispatching-done)

;; ivy bibtex
(sk/require-package 'ivy-bibtex)
(setq bibtex-completion-bibliography '("~/Dropbox/org/references/multiphysics.bib" "~/Dropbox/org/references/chanceconstraints.bib"))
(setq bibtex-completion-library-path "~/Dropbox/org/references/pdfs")
(setq bibtex-completion-notes-path "~/Dropbox/org/references/articles.org")

;; counsel projectile
(setq projectile-completion-system 'ivy)
(sk/require-package 'counsel-projectile)

;; Spotlight search
(sk/require-package 'spotlight)

;; aux requirements
(require 'sk-ivy-bindings)

(provide 'sk-ivy)
;;; sk-ivy.el ends here
