;;; sk-writing.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Writing helpers

;;; Code:

;; Markdown support
(sk/require-package 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mkd\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Pandoc mode
(sk/require-package 'pandoc-mode)
(add-hook 'markdown-mode-hook 'pandoc-mode)

;; LaTeX mode
(sk/require-package 'auctex)
(sk/require-package 'auctex-latexmk)

;; Fountain mode for screenwriting
(sk/require-package 'fountain-mode)

;; Distraction free writing
(sk/require-package 'olivetti)

;; aux requirements
(require 'sk-writing-bindings)
(require 'sk-writing-diminish)

(provide 'sk-writing)
;;; sk-writing.el ends here
