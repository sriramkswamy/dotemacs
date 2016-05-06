;;; sk-editing-diminish.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Diminish minor modes from showing up in the mode line

;;; Code:

;; Diminish double capitals mode
(defun sk/diminish-dubcaps ()
  (interactive)
  (diminish 'sk/dubcaps-mode ""))
(add-hook 'sk/dubcaps-mode-hook 'sk/diminish-dubcaps)

;; Diminish smart parens
(defun sk/diminish-smartparens ()
  (interactive)
  (diminish 'smartparens-mode ""))
(add-hook 'smartparens-mode-hook 'sk/diminish-smartparens)

;; Diminish smart parens strict mode
(defun sk/diminish-smartparens-strict ()
  (interactive)
  (diminish 'smartparens-strict-mode ""))
(add-hook 'smartparens-strict-mode-hook 'sk/diminish-smartparens-strict)

;; YASnippet
(defun sk/diminish-yas ()
  (interactive)
  (diminish 'yas-minor-mode " γ"))
(add-hook 'yas-global-mode-hook 'sk/diminish-yas)

(provide 'sk-editing-diminish)
;;; sk-editing-diminish.el ends here
