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

;; Smartparens
(global-set-key (kbd "C-c s j") 'sp-down-sexp)
(global-set-key (kbd "C-c s k") 'sp-up-sexp)
(global-set-key (kbd "C-c s J") 'sp-backward-down-sexp)
(global-set-key (kbd "C-c s K") 'sp-backward-up-sexp)
(global-set-key (kbd "C-c s h") 'sp-backward-sexp)
(global-set-key (kbd "C-c s l") 'sp-forward-sexp)
(global-set-key (kbd "C-c s H") 'sp-beginning-of-sexp)
(global-set-key (kbd "C-c s L") 'sp-end-of-sexp)
(global-set-key (kbd "C-c s n") 'sp-next-sexp)
(global-set-key (kbd "C-c s p") 'sp-previous-sexp)
(global-set-key (kbd "C-c s N") 'sp-forward-symbol)
(global-set-key (kbd "C-c s P") 'sp-backward-symbol)
(global-set-key (kbd "C-c s f") 'sp-forward-barf-sexp)
(global-set-key (kbd "C-c s b") 'sp-backward-barf-sexp)
(global-set-key (kbd "C-c s F") 'sp-forward-slurp-sexp)
(global-set-key (kbd "C-c s B") 'sp-backward-slurp-sexp)
(global-set-key (kbd "C-c s x") 'sp-transpose-sexp)
(global-set-key (kbd "C-c s d") 'sp-kill-sexp)
(global-set-key (kbd "C-c s y") 'sp-copy-sexp)
(global-set-key (kbd "C-c s s") 'sp-unwrap-sexp)
(global-set-key (kbd "C-c s S") 'sp-backward-unwrap-sexp)
(global-set-key (kbd "C-c s (") 'sk/wrap-with-parens)
(global-set-key (kbd "C-c s )") 'sk/wrap-with-parens)
(global-set-key (kbd "C-c s [") 'sk/wrap-with-brackets)
(global-set-key (kbd "C-c s ]") 'sk/wrap-with-brackets)
(global-set-key (kbd "C-c s {") 'sk/wrap-with-braces)
(global-set-key (kbd "C-c s }") 'sk/wrap-with-braces)
(global-set-key (kbd "C-c s $") 'sk/wrap-with-latex-dollars)
(global-set-key (kbd "C-c s '") 'sk/wrap-with-single-quotes)
(global-set-key (kbd "C-c s \"") 'sk/wrap-with-double-quotes)
(global-set-key (kbd "C-c s `") 'sk/wrap-with-back-quotes)

;; YASnippet
(global-set-key (kbd "C-c S") 'yas-insert-snippet)

;; Visual regexp
(global-set-key (kbd "C-c v s") 'vr/query-replace)

;; Iedit mode
(global-set-key (kbd "C-c I") 'iedit-mode)

;; Multiple cursors (no modal bindings)
(global-set-key (kbd "C-+") 'mc/edit-beginnings-of-lines)
(global-set-key (kbd "C-,") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-.") 'mc/mark-all-like-this-dwim)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-=") 'mc/mark-more-like-this-extended)
(global-set-key (kbd "C-(") 'mc/skip-to-previous-like-this)
(global-set-key (kbd "C-)") 'mc/skip-to-next-like-this)

;; Region bindings mode (this acts as a modal binding instead)
(define-key region-bindings-mode-map "ma" 'mc/mark-all-like-this-dwim)
(define-key region-bindings-mode-map "mk" 'mc/mark-previous-like-this)
(define-key region-bindings-mode-map "mj" 'mc/mark-next-like-this)
(define-key region-bindings-mode-map "mh" 'mc/skip-to-previous-like-this)
(define-key region-bindings-mode-map "ml" 'mc/skip-to-next-like-this)
(define-key region-bindings-mode-map "mm" 'mc/mark-more-like-this-extended)
(define-key region-bindings-mode-map "ms" 'mc/edit-beginnings-of-lines)
(define-key region-bindings-mode-map "me" 'mc/edit-ends-of-lines)

;; Modal bindings
(require 'sk-editing-modalka)

;; which key for bindings
(require 'sk-editing-bindings-which-key)

(provide 'sk-editing-bindings)
;;; sk-editing-bindings.el ends here
