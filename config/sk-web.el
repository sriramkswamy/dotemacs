;; Web mode
(sk/require-package 'web-mode)
(add-hook 'html-mode-hook 'web-mode)

;; JavaScript
(sk/require-package 'js2-mode)
(sk/require-package 'skewer-mode)

;; Support web mode
(sk/require-package 'company-web)

(provide 'sk-web)

;;; sk-web.el ends here
