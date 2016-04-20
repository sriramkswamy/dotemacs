;;; sk-modalka-diminish.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Diminish modalka from the mode line

;;; Code:

(defun sk/diminish-modalka ()
  (interactive)
  (diminish 'modalka-mode "MOD"))
(add-hook 'modalka-mode-hook 'sk/diminish-modalka)

(provide 'sk-modalka-diminish)
;;; sk-modalka-diminish.el ends here
