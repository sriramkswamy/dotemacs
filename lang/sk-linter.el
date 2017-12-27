(defun sk/spaceline-flycheck ()
  "diminsih flycheck if spaceline is present"
  (interactive)
  (when (fboundp 'spaceline-compile)
	(diminish 'flycheck-mode "")))

;; error checking
(use-package flycheck
  :ensure t
  :hook ((prog-mode . flycheck-mode)
		 (lisp-interaction-mode . flycheck-mode)
		 (flycheck-mode . sk/spaceline-flycheck))
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
