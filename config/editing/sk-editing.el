;;; sk-editing.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; For all kinds of editing

;;; Code:

;; Expand regions
(use-package expand-region
  :ensure t
  :commands (er/expand-region
	     er/contract-region
	     er/mark-text-paragraph
	     er/mark-defun
	     er/mark-word
	     er/mark-url
	     er/mark-LaTeX-math
	     er/mark-comment
	     er/mark-inside-pairs
	     er/mark-inside-quotes
	     er/mark-org-code-block
	     er/mark-python-block
	     er/mark-ruby-block-up
	     er/mark-symbol
	     er/mark-text-sentence)
  :defer t
  :bind (
	 ("C-c e a" . er/expand-region)
	 ("C-c e A" . er/contract-region)
	 ("C-c e i p" . er/mark-text-paragraph)
	 ("C-c e i f" . er/mark-defun)
	 ("C-c e i w" . er/mark-word)
	 ("C-c e i u" . er/mark-url)
	 ("C-c e i $" . er/mark-LaTeX-math)
	 ("C-c e i c" . er/mark-comment)
	 ("C-c e i b" . er/mark-inside-pairs)
	 ("C-c e i q" . er/mark-inside-quotes)
	 ("C-c e i o" . er/mark-org-code-block)
	 ("C-c e i m" . er/mark-python-block)
	 ("C-c e i j" . er/mark-ruby-block-up)
	 ("C-c e i v" . er/mark-symbol)
	 ("C-c e i s" . er/mark-text-sentence)
	 )
  :config
  (which-key-add-key-based-replacements
    "C-c e" "expand region prefix"
    "C-c e i" "expand region inside prefix"))

;; Commenting
(use-package comment-dwim-2
  :ensure t
  :commands (comment-dwim-2)
  :defer t
  :bind (
	 ("M-;" . comment-dwim-2)
	 ("C-c v g c" . comment-dwim-2)
	 ))

;; Smartparens
(use-package smartparens
  :ensure t
  :defer t
  :bind (
	 ("C-c s j" . sp-down-sexp)
	 ("C-c s k" . sp-backward-up-sexp)
	 ("C-c s h" . sp-backward-down-sexp)
	 ("C-c s l" . sp-up-sexp)
	 ("C-c s f" . sp-forward-sexp)
	 ("C-c s b" . sp-backward-sexp)
	 ("C-c s a" . sp-beginning-of-sexp)
	 ("C-c s e" . sp-end-of-sexp)
	 ("C-c s n" . sp-next-sexp)
	 ("C-c s p" . sp-previous-sexp)
	 ("C-c s > >" . sp-forward-barf-sexp)
	 ("C-c s < <" . sp-backward-barf-sexp)
	 ("C-c s > <" . sp-forward-slurp-sexp)
	 ("C-c s < >" . sp-backward-slurp-sexp)
	 ("C-c s x" . sp-transpose-sexp)
	 ("C-c s d" . sp-kill-sexp)
	 ("C-c s y" . sp-copy-sexp)
	 ("C-c s s" . sp-unwrap-sexp)
	 ("C-c s S" . sp-backward-unwrap-sexp)
	 )
  :diminish smartparens-mode
  :diminish smartparens-strict-mode
  :config
  (require 'smartparens-config)
  (smartparens-global-mode)
  (smartparens-global-strict-mode)
  (show-smartparens-global-mode)
  (which-key-add-key-based-replacements
  "C-c s" "smart nav prefix"
  "C-c s >" "forward prefix"
  "C-c s <" "backward prefix"))

;; Iedit mode
(use-package iedit
  :ensure t
  :commands (iedit-mode)
  :defer t
  :bind (
	 ("C-c I" . iedit-mode)
	 ))

;; Snippets
(use-package yasnippet
  :ensure t
  :commands (yas-insert-snippet yas-new-snippet)
  :defer t
  :bind (
	 ("C-c S" . yas-insert-snippet)
	 )
  :init
  (setq yas-prompt-functions '(yas-completing-prompt))
  :diminish (yas-minor-mode . "γ")
  :config
  (yas-global-mode))

;; Visual regexp
(use-package visual-regexp
  :ensure t
  :commands (vr/query-replace)
  :defer t
  :bind (
	 ("C-c v s" . vr/query-replace)
	 )
  :config
  (use-package visual-regexp-steroids
    :ensure t
    :commands (vr/select-query-replace)
    :defer t))

;; Multiple cursors
(use-package multiple-cursors
  :ensure t
  :commands (mc/edit-beginnings-of-lines
	     mc/edit-ends-of-lines
	     mc/mark-all-like-this-dwim
	     mc/mark-previous-like-this
	     mc/mark-next-like-this
	     mc/mark-more-like-this-extended
	     mc/skip-to-next-like-this
	     mc/skip-to-previous-like-this)
  :defer t
  :bind (
	 ("C-+" . mc/edit-beginnings-of-lines)
	 ("C-," . mc/edit-ends-of-lines)
	 ("C-." . mc/mark-all-like-this-dwim)
	 ("C-<" . mc/mark-previous-like-this)
	 ("C->" . mc/mark-next-like-this)
	 ("C-=" . mc/mark-more-like-this-extended)
	 ("C-(" . mc/skip-to-previous-like-this)
	 ("C-)" . mc/skip-to-next-like-this)
	 ))

;; Region bindings mode
(use-package region-bindings-mode
  :ensure t
  :commands (mc/edit-beginnings-of-lines
	     mc/edit-ends-of-lines
	     mc/mark-all-like-this-dwim
	     mc/mark-previous-like-this
	     mc/mark-next-like-this
	     mc/mark-more-like-this-extended
	     mc/skip-to-next-like-this
	     mc/skip-to-previous-like-this)
  :defer t
  :bind (:map region-bindings-mode-map
	      ("ma" . mc/mark-all-like-this-dwim)
	      ("mk" . mc/mark-previous-like-this)
	      ("mj" . mc/mark-next-like-this)
	      ("mh" . mc/skip-to-previous-like-this)
	      ("ml" . mc/skip-to-next-like-this)
	      ("mm" . mc/mark-more-like-this-extended)
	      ("ms" . mc/edit-beginnings-of-lines)
	      ("me" . mc/edit-ends-of-lines)
	      )
  :diminish (region-bindings-mode . "ρ")
  :config
  (region-bindings-mode-enable))

;; Edit really large files
(sk/require-package 'vlf)

;; aux requirements
(require 'sk-editing-functions)
(require 'sk-editing-modalka)
(require 'sk-editing-hydra)

(provide 'sk-editing)
;;; sk-editing.el ends here
