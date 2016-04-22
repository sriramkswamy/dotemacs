;;; sk-versions.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Version control packages

;;; Code:

;; Magit - Best git wrapper ever
(sk/require-package 'magit)

;; Diff hl for visual feedback of changes
(sk/require-package 'diff-hl)

;; aux requirements
(require 'sk-versions-bindings)

(provide 'sk-versions)
;;; sk-versions.el ends here
