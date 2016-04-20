;;; sk-editing.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; For all kinds of editing

;;; Code:

;; Expand regions
(sk/require-package 'expand-region)
(require 'expand-region)

;; Commenting
(sk/require-package 'comment-dwim-2)

;; Wrap regions
(sk/require-package 'wrap-region)
(wrap-region-global-mode)
(wrap-region-add-wrapper "$" "$")
(wrap-region-add-wrapper "<" ">")
(wrap-region-add-wrapper "*" "*" nil '(org-mode markdown-mode))
(wrap-region-add-wrapper "_" "_" nil '(org-mode markdown-mode))
(wrap-region-add-wrapper "~" "~" nil '(org-mode))
(wrap-region-add-wrapper "=" "=" nil '(org-mode))
(wrap-region-add-wrapper "/* " " */" "#" '(java-mode javascript-mode css-mode cc-mode))
(wrap-region-add-wrapper "`" "`" nil '(markdown-mode))
(wrap-region-add-wrapper "```" "```" "#" '(markdown-mode))

;; Snippets
(sk/require-package 'yasnippet)
(setq yas-prompt-functions '(yas-completing-prompt))
(add-hook 'prog-mode-hook 'yas-global-mode)
(add-hook 'org-mode-hook 'yas-global-mode)

;; Visual regexp
(sk/require-package 'visual-regexp)
(sk/require-package 'visual-regexp-steroids)

;; aux requirements
(require 'sk-editing-functions)
(require 'sk-editing-bindings)
(require 'sk-editing-modalka)
(require 'sk-editing-hydra)
(require 'sk-editing-diminish)

(provide 'sk-editing)
;;; sk-editing.el ends here
