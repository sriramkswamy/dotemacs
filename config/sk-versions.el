;; best git wrapper ever
(use-package magit
  :ensure t
  :ensure-system-package git
  :bind* ("C-x g" . magit-status)
  :commands
  (magit-blame))

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

;; git timemachine
(use-package git-timemachine
  :ensure t
  :commands
  (git-timemachine-toggle))

;; posting gists
(use-package yagist
  :ensure t
  :init
  (setq yagist-encrypt-risky-config t)
  :commands
  (yagist-region-or-buffer))

;; browse remote packages
(use-package browse-at-remote
  :ensure t
  :commands
  (browse-at-remote))

;; posting gists
(defun sk/post-gist (arg)
  "post the gist. Toggle privacy with ARG"
  (interactive "p")
  (if (equal arg '(4))
	  (yagist-region-or-buffer-private)
	(yagist-region-or-buffer)))

;; provide version control configuration
(provide 'sk-versions)
