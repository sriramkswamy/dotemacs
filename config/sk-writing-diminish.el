;;; sk-writing-diminish.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Diminish some writing modes

;;; Code:

;; Diminish olivetti
(defun sk/diminish-olivetti ()
  (interactive)
  (diminish 'olivetti-mode ""))
(add-hook 'olivetti-mode-hook 'sk/diminish-olivetti)

;; Diminish writegood
(defun sk/diminish-writegood ()
  (interactive)
  (diminish 'writegood-mode ""))
(add-hook 'writegood-mode-hook 'sk/diminish-writegood)

(provide 'sk-writing-diminish)
;;; sk-writing-diminish.el ends here
