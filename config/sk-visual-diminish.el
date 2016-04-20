;;; sk-visual-diminish.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Diminish some minor modes

;;; Code:

;; Column force as a lookout for length
(defun sk/diminish-column-enforce ()
  (interactive)
  (diminish 'column-enforce-mode ""))
(add-hook 'column-enforce-mode-hook 'sk/diminish-column-enforce)

;; Volatile highlights
(defun sk/diminish-volatile-highlights ()
  (interactive)
  (diminish 'volatile-highlights-mode ""))
(add-hook 'volatile-highlights-mode-hook 'sk/diminish-volatile-highlights)

;; ws-butler
(defun sk/diminish-ws-butler ()
  (interactive)
  (diminish 'ws-butler-mode ""))
(add-hook 'ws-butler-mode-hook 'sk/diminish-ws-butler)

;; Smarter tabs
(defun sk/diminish-smart-tab ()
  (interactive)
  (diminish 'smart-tab-mode ""))
(add-hook 'smart-tab-mode-hook 'sk/diminish-smart-tab)

(provide 'sk-visual-diminish)
;;; sk-visual-diminish.el ends here
