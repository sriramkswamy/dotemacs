;;; sk-writing-diminish.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Diminish some writing modes

;;; Code:

;; Diminish olivetti
(defun sk/diminish-olivetti ()
  (interactive)
  (diminish 'olivetti-mode ""))
(add-hook 'olivetti-mode-hook 'sk/diminish-olivetti)

(provide 'sk-writing-diminish)
;;; sk-writing-diminish.el ends here
