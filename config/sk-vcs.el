;; git timemachine
(use-package git-timemachine
  :ensure t
  :bind* (("C-c g L" . git-timemachine-toggle)))

;; posting gists
(use-package yagist
  :ensure t
  :init
  (setq yagist-encrypt-risky-config t)
  :bind*(("C-c g P" . yagist-region-or-buffer)))

;; browse remote packages
(use-package browse-at-remote
  :ensure t
  :bind* (("C-c g r" . browse-at-remote)))

;; posting gists
(defun sk/post-gist (arg)
  "post the gist. Toggle privacy with ARG"
  (interactive "p")
  (if (equal arg '(4))
	  (yagist-region-or-buffer-private)
	(yagist-region-or-buffer)))

;; mappings
(ryo-modal-key "SPC c" 'git-timemachine-toggle :exit t)
(ryo-modal-key "g R" 'browse-at-remote)
(ryo-modal-key "g"
			   '(("L" git-timemachine-switch-branch)
				 ("P" sk/post-gist)))

;; provide this configuration
(provide 'sk-vcs)
