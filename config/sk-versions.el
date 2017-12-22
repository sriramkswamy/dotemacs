;; best git wrapper ever
(use-package magit
  :ensure t
  :ensure-system-package (git)
  :bind* ("C-x g" . magit-status)
  :commands
  (magit-blame))

;; Github integration - press '@' in Magit status
(use-package magithub
  :ensure t
  :ensure-system-package (hub)
  :after (magit))

;; magit diff or generic ediff
(defun sk/ediff (arg)
  "Magit ediff normally or generic ediff if called with universal argument"
  (interactive "P")
  (if arg
	  (ediff)
	(magit-ediff-show-working-tree)))

;; highlight diffs
(use-package diff-hl
  :ensure t
  :commands (global-diff-hl-mode
			 diff-hl-mode
			 diff-hl-next-hunk
			 diff-hl-previous-hunk
			 diff-hl-mark-hunk
			 diff-hl-diff-goto-hunk
			 diff-hl-revert-hunk)
  :config
  (global-diff-hl-mode)
  ;; (diff-hl-flydiff-mode)
  (diff-hl-margin-mode)
  (diff-hl-dired-mode))

;; provide version control configuration
(provide 'sk-versions)
