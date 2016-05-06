;;; sk-org-diminish.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Org mode line cleanup

;;; Code:

;; diminish cd latex
(defun sk/diminish-org ()
  (interactive)
  (diminish 'org-indent-mode "")
  (diminish 'org-cdlatex-mode ""))
(add-hook 'org-mode-hook 'sk/diminish-org)

(provide 'sk-org-diminish)
;;; sk-org-diminish.el ends here
