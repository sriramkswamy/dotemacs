;;; sk-versions.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Version control packages

;;; Code:

;; Magit - Best git wrapper ever
(sk/require-package 'magit)

;; Diff hl for visual feedback of changes
(sk/require-package 'diff-hl)
(add-hook 'prog-mode-hook 'diff-hl-mode)
(add-hook 'text-mode-hook 'diff-hl-mode)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
(add-hook 'diff-hl-mode-hook 'diff-hl-flydiff-mode)

;; aux requirements
(require 'sk-versions-bindings)

(provide 'sk-versions)
;;; sk-versions.el ends here
