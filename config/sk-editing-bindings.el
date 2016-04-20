;;; sk-editing-bindings.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Bindings for editing

;;; Code:

;; Expand regions
(global-set-key (kbd "C-c e a") 'er/expand-region)
(global-set-key (kbd "C-c e A") 'er/contract-region)
(global-set-key (kbd "C-c e i p") 'er/mark-text-paragraph)
(global-set-key (kbd "C-c e i f") 'er/mark-defun)
(global-set-key (kbd "C-c e i w") 'er/mark-word)
(global-set-key (kbd "C-c e i u") 'er/mark-url)
(global-set-key (kbd "C-c e i $") 'er/mark-LaTeX-math)
(global-set-key (kbd "C-c e i c") 'er/mark-comment)
(global-set-key (kbd "C-c e i b") 'er/mark-inside-pairs)
(global-set-key (kbd "C-c e i q") 'er/mark-inside-quotes)
(global-set-key (kbd "C-c e i o") 'er/mark-org-code-block)
(global-set-key (kbd "C-c e i m") 'er/mark-python-block)
(global-set-key (kbd "C-c e i j") 'er/mark-ruby-block-up)
(global-set-key (kbd "C-c e i v") 'er/mark-symbol)
(global-set-key (kbd "C-c e i s") 'er/mark-text-sentence)

;; Commenting
(global-set-key (kbd "M-;") 'comment-dwim-2)
(global-set-key (kbd "C-c v g c") 'comment-dwim-2)

;; YASnippet
(global-set-key (kbd "C-c s") 'yas-insert-snippet)

;; Visual regexp
(global-set-key (kbd "C-c v s") 'vr/query-replace)

;; Modal bindings
(require 'sk-editing-modalka)

;; which key for bindings
(require 'sk-editing-bindings-which-key)

(provide 'sk-editing-bindings)
;;; sk-editing-bindings.el ends here
