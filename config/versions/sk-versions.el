;;; sk-versions.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Version control packages

;;; Code:

;; Magit - Best git wrapper ever
(use-package magit
  :ensure t
  :commands (magit-status
	     magit-blame)
  :bind (
	 ("C-c g s" . magit-status)
	 ("C-c g b" . magit-blame)
	 )
  :config
  (which-key-add-key-based-replacements
  "C-c g" "git prefix"))

;; Diff hl for visual feedback of changes
(use-package diff-hl
  :ensure t
  :demand t
  :commands (diff-hl-next-hunk
	     diff-hl-previous-hunk
	     diff-hl-mark-hunk
	     diff-hl-diff-goto-hunk
	     diff-hl-revert-hunk)
  :bind (
	 ("C-c g d n" . diff-hl-next-hunk)
	 ("C-c g d p" . diff-hl-previous-hunk)
	 ("C-c g d m" . diff-hl-mark-hunk)
	 ("C-c g d g" . diff-hl-diff-goto-hunk)
	 ("C-c g d h" . diff-hl-revert-hunk))
  :config
  (diff-hl-flydiff-mode)
  (diff-hl-margin-mode)
  (diff-hl-dired-mode)
  (which-key-add-key-based-replacements
    "C-c g d" "diff hl prefix"))

;; Git timemachine
(use-package git-timemachine
  :ensure t
  :commands (git-timemachine-toggle
	     git-timemachine-switch-branch)
  :bind (
	 ("C-c g l" . git-timemachine-toggle)
	 ("C-c g L" . git-timemachine-switch-branch)
	 ))

;; Gist stuff!
(use-package yagist
  :ensure t
  :commands (yagist-region-or-buffer
	     yagist-region-or-buffer-private)
  :bind (
	 ("C-c g g p" . yagist-region-or-buffer)
	 ("C-c g g P" . yagist-region-or-buffer-private)
	 )
  :init
  (setq yagist-encrypt-risky-config t)
  :config
  (which-key-add-key-based-replacements
    "C-c g g" "gist prefix"))

;; Git config mode
(use-package gitconfig-mode
  :ensure t
  :mode "\\.gitconfig\\'")

;; Git ignore mode
(use-package gitignore-mode
  :ensure t
  :mode "\\.gitignore\\'")

;; aux requirements
(require 'sk-versions-modalka)

(provide 'sk-versions)
;;; sk-versions.el ends here
