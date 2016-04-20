;;; sk-which-key-diminish.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Diminish which key from the mode line

;;; Code:

(defun sk/diminish-which-key ()
  (interactive)
  (diminish 'which-key-mode ""))
(add-hook 'which-key-mode-hook 'sk/diminish-which-key)
(add-hook 'after-init-hook 'which-key-mode)

(provide 'sk-which-key-diminish)
;;; sk-which-key-diminish.el ends here
