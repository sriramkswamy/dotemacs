;;; sk-writing.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Writing helpers

;;; Code:

;; Markdown support
(use-package markdown-mode
  :ensure t
  :mode ("\\.markdown\\'" "\\.mkd\\'" "\\.md\\'")
  :config
  (use-package pandoc-mode
    :ensure t
    :mode ("\\.markdown\\'" "\\.mkd\\'" "\\.md\\'")))

;; LaTeX mode
(use-package sk-tex-pack
  :ensure auctex
  :ensure auctex-latexmk
  :ensure latex-preview-pane
  :mode ("\\.tex\\'" "\\.xtx\\'")
  :init
  (setq reftex-default-bibliography '("~/Dropbox/references/references.bib"))
  :config
  (latex-preview-pane-enable))

;; Distraction free writing
(use-package olivetti
  :ensure t
  :diminish olivetti-mode
  :bind (
	 ("C-c v g D" . olivetti-mode)
	 )
  :init
  (setq olivetti-set-width 80))
(modalka-define-kbd "g d" "C-c v g D")
(which-key-add-key-based-replacements
  "g d" "distraction free")

;; Filter out passive voice and weasel words
(use-package writegood-mode
  :ensure t
  :diminish writegood-mode
  :config
  (progn
    (add-hook 'text-mode-hook 'writegood-mode)))

;; Define the word at point
(use-package define-word
  :ensure t
  :commands (define-word-at-point
	      define-word)
  :bind (
	 ("C-c v g ?" . define-word-at-point)
	 ))
(modalka-define-kbd "g ?" "C-c v g ?")
(which-key-add-key-based-replacements
  "g ?" "define word")

(provide 'sk-writing)
;;; sk-writing.el ends here
