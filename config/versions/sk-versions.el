;;; sk-versions.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Version control packages

;;; Code:

;; Magit - Best git wrapper ever
(sk/require-package 'magit)
(sk/require-package 'magit-gh-pulls)
(add-hook 'magit-status-mode-hook 'magit-gh-pulls-mode)

;; Diff hl for visual feedback of changes
(sk/require-package 'diff-hl)
(add-hook 'prog-mode-hook 'diff-hl-mode)
(add-hook 'text-mode-hook 'diff-hl-mode)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
(add-hook 'diff-hl-mode-hook 'diff-hl-flydiff-mode)
(add-hook 'diff-hl-mode-hook 'diff-hl-margin-mode)

;; Git timemachine
(sk/require-package 'git-timemachine)

;; Gist stuff!
(sk/require-package 'yagist)
(setq yagist-encrypt-risky-config t)

;; Git config mode
(sk/require-package 'gitconfig-mode)

;; Git ignore mode
(sk/require-package 'gitignore-mode)

;; aux requirements
(require 'sk-versions-bindings)

(provide 'sk-versions)
;;; sk-versions.el ends here
