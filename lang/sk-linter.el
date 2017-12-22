;; error checking
(use-package flycheck
  :ensure t
  :hook (prog-mode . global-flycheck-mode)
  :commands (flycheck-buffer
			 flycheck-previous-error
			 flycheck-next-error
			 flycheck-list-errors
			 flycheck-explain-error-at-point
			 flycheck-display-error-at-point
			 flycheck-select-checker
			 flycheck-verify-setup)
  :config
  (global-flycheck-mode))

;; provide linter
(provide 'sk-linter)
