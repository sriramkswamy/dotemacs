;;; sk-diminish.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Diminish minor modes

;;; Code:

;; Diminish
(sk/require-package 'diminish)

;; Hide/Show mode
(defun sk/diminish-hs-minor ()
  (interactive)
  (diminish 'hs-minor-mode ""))
(add-hook 'hs-minor-mode-hook 'sk/diminish-hs-minor)
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; Diminish some stuff
(defun sk/diminish-abbrev ()
  (interactive)
  (diminish 'abbrev-mode ""))
(add-hook 'abbrev-mode-hook 'sk/diminish-abbrev)
(add-hook 'prog-mode-hook 'sk/diminish-abbrev)
(add-hook 'text-mode-hook 'sk/diminish-abbrev)

;; Visual wrap mdoe
(defun sk/diminish-visual-line ()
  (interactive)
  (diminish 'visual-line-mode "ω"))
(add-hook 'visual-line-mode-hook 'sk/diminish-visual-line)

;; Diminish some stuff
(defun sk/diminish-auto-fill ()
  (interactive)
  (diminish 'auto-fill-mode ""))
(add-hook 'abbrev-mode-hook 'sk/diminish-auto-fill)
(add-hook 'prog-mode-hook 'sk/diminish-auto-fill)
(add-hook 'text-mode-hook 'sk/diminish-auto-fill)

;; Subword mode
(defun sk/diminish-subword ()
  (interactive)
  (diminish 'subword-mode ""))
(add-hook 'subword-mode-hook 'sk/diminish-subword)
(global-subword-mode)

;; Eldoc
(defun sk/diminish-eldoc ()
  (interactive)
  (diminish 'eldoc-mode ""))
(add-hook 'eldoc-mode-hook 'sk/diminish-eldoc)

;; Auto-revert mode
(defun sk/diminish-auto-revert ()
  (interactive)
  (diminish 'auto-revert-mode ""))
(add-hook 'auto-revert-mode-hook 'sk/diminish-auto-revert)

;; Flyspell
(defun sk/diminish-flyspell ()
  (interactive)
  (diminish 'flyspell-mode " φ"))
(add-hook 'flyspell-mode-hook 'sk/diminish-flyspell)


(provide 'sk-diminish)
;;; sk-diminish.el ends here
