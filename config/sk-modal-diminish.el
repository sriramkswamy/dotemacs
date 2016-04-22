;;; sk-modal-diminish.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Diminish modal modes

;;; Code:

;; Modalka mode
(defun sk/diminish-modalka ()
  (interactive)
  (diminish 'modalka-mode "MOD"))
(add-hook 'modalka-mode-hook 'sk/diminish-modalka)

(provide 'sk-modal-diminish)
;;; sk-modal-diminish.el ends here
