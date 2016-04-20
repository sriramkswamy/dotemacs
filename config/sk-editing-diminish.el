;;; sk-editing-diminish.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Diminish minor modes from showing up in the mode line

;;; Code:

;; Wrap region
(defun sk/diminish-wrap-region ()
  (interactive)
  (diminish 'wrap-region-mode ""))
(add-hook 'wrap-region-mode-hook 'sk/diminish-wrap-region)

;; YASnippet
(defun sk/diminish-yas ()
  (interactive)
  (diminish 'yas-minor-mode " γ"))
(add-hook 'yas-global-mode-hook 'sk/diminish-yas)

(provide 'sk-editing-diminish)
;;; sk-editing-diminish.el ends here
