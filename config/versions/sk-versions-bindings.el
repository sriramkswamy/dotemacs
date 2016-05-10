;;; sk-versions-bindings.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Version control packages bindings

;;; Code:

;; Magit - Best git wrapper ever
(global-set-key (kbd "C-c g s") 'magit-status)
(global-set-key (kbd "C-c g b") 'magit-blame)

;; Diff hl
(global-set-key (kbd "C-c g d n") 'diff-hl-next-hunk)
(global-set-key (kbd "C-c g d p") 'diff-hl-previous-hunk)
(global-set-key (kbd "C-c g d m") 'diff-hl-mark-hunk)
(global-set-key (kbd "C-c g d g") 'diff-hl-diff-goto-hunk)
(global-set-key (kbd "C-c g d h") 'diff-hl-revert-hunk)

;; Time machine
(global-set-key (kbd "C-c g l") 'git-timemachine-toggle)
(global-set-key (kbd "C-c g L") 'git-timemachine-switch-branch)

;; Yagist
(global-set-key (kbd "C-c g g p") 'yagist-region-or-buffer)
(global-set-key (kbd "C-c g g P") 'yagist-region-or-buffer-private)

;; which key explanations
(require 'sk-versions-bindings-which-key)

;; Modal bindings
(require 'sk-versions-modalka)

(provide 'sk-versions-bindings)
;;; sk-versions-bindings.el ends here
