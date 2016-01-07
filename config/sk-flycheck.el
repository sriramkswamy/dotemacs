(sk/require-package 'flycheck)
(add-hook 'prog-mode-hook #'global-flycheck-mode)
(add-hook 'text-mode-hook #'global-flycheck-mode)

(provide 'sk-flycheck)

;;; sk-flycheck.el ends here
