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
(setq reftex-default-bibliography '("~/Dropbox/references/references.bib"))

;; Fountain mode for screenwriting
(sk/require-package 'fountain-mode)

;; Distraction free writing
(sk/require-package 'olivetti)

;; Filter out passive voice and weasel words
(sk/require-package 'writegood-mode)
(add-hook 'text-mode-hook 'writegood-mode)

;; PDF tools instead of docview
(sk/require-package 'pdf-tools)
;; Turns the next page in adjoining pdf-tools pdf
(defun sk/pdf-tools-pdf-next ()
  "Turns the next page in adjoining PDF file"
  (interactive)
  (other-window 1)
  (pdf-view-next-page)
  (other-window 1))
;; Turns the previous page in adjoining pdf
(defun sk/pdf-tools-pdf-previous ()
  "Turns the previous page in adjoining PDF file"
  (interactive)
  (other-window 1)
  (pdf-view-previous-page)
  (other-window 1))
(add-hook 'doc-view-mode-hook 'pdf-tools-install)

;; aux requirements
(require 'sk-writing-bindings)
(require 'sk-writing-diminish)

(provide 'sk-writing)
;;; sk-writing.el ends here
