;;; sk-cpp-diminish.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Diminish C++ stuff

;;; Code:

(defun sk/diminish-irony ()
  (interactive)
  (diminish 'irony-mode " Γ"))
(add-hook 'irony-mode-hook 'sk/diminish-irony)

(provide 'sk-cpp-diminish)
;;; sk-cpp-diminish.el ends here
