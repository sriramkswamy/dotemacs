;; Web mode
(sk/require-package 'web-mode)
(add-hook 'html-mode-hook 'web-mode)

;; Refresh
(sk/require-package 'skewer-mode)

;; JavaScript
(sk/require-package 'js2-mode)
(sk/require-package 'js2-refactor)
(setq-default js2-basic-offset 2
              js2-indent-level 2)

;; JSON
(sk/require-package 'json-snatcher)

;; Beautify
(sk/require-package 'web-beautify)

;; Support web mode
(sk/require-package 'company-web)

(provide 'sk-web)

;;; sk-web.el ends here
