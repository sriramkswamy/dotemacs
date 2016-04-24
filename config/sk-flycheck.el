;;; sk-flycheck.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Error checker on the fly

;;; Code:

(sk/require-package 'flycheck)
(add-hook 'prog-mode-hook 'global-flycheck-mode)

;; aux requirements
(require 'sk-flycheck-modalka)

(provide 'sk-flycheck)
;;; sk-flycheck.el ends here
