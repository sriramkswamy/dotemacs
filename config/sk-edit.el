;; save automatically
(use-package super-save
  :ensure t
  :diminish super-save-mode
  :init
  (setq super-save-auto-save-when-idle t)
  (setq auto-save-default nil)
  (setq super-save-remote-files nil)
  :config
  ;; add integration with ace-window
  (add-to-list 'super-save-triggers 'ace-window)
  ;; save on find-file
  (add-to-list 'super-save-hook-triggers 'find-file-hook)
  (super-save-mode +1))

;; commenting easily
(use-package comment-dwim-2
  :ensure t
  :bind* (("M-;" . comment-dwim-2)))

;; undo history
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :bind* (("C-/"	. undo-tree-undo)
		  ("M-/"	. undo-tree-redo)
		  ("C-x u"	. undo-tree-visualize))
  :config
  (undo-tree-mode))

;; snippets
(use-package yasnippet
  :ensure t
  :hook ((prog-mode . yas-global-mode)
         (text-mode . yas-global-mode))

  :commands
  (yas-insert-snippet
   yas-abort-snippet
   yas-tryout-snippet
   yas-minor-mode
   yas-visit-snippet-file
   yas-load-snippet-buffer
   yas-load-snippet-buffer-and-close
   yas-reload-all
   yas-new-snippet)

  :bind* ("C-z" . yas-insert-snippet)
  :diminish (yas-minor-mode . " γ")

  :config
  (setq yas/triggers-in-field t); Enable nested triggering of snippets
  (setq yas-prompt-functions '(yas-completing-prompt))
  (add-hook 'snippet-mode-hook '(lambda () (setq-local require-final-newline nil)))
  (yas-global-mode))

;; select arbitrary regions
(use-package expand-region
  :ensure t

  :commands
  (er/expand-region
   er/expand-region
   mark-whole-buffer
   mark-whole-buffer
   er/mark-text-paragraph
   mark-paragraph
   sk/select-inside-line
   sk/select-around-line
   er/mark-text-sentence
   er/mark-text-sentence
   er/mark-symbol
   sk/mark-around-symbol
   er/mark-comment
   er/mark-comment
   er/mark-word
   sk/mark-around-word
   er/mark-defun
   er/mark-defun
   er/mark-inside-quotes
   er/mark-outside-quotes
   sk/mark-inside-org-code
   er/mark-org-code-block
   sk/mark-inside-subtree
   org-mark-subtree
   er/mark-LaTeX-inside-environment
   LaTeX-mark-environment
   er/mark-method-call
   er/mark-method-call
   sk/mark-inside-ruby-block
   er/ruby-block-up
   er/mark-inside-python-string
   er/mark-outside-python-string
   sk/mark-inside-python-block
   er/mark-outer-python-block
   er/mark-python-statement
   er/mark-python-block-and-decorator
   er/mark-LaTeX-math
   sk/mark-inside-LaTeX-math
   er/mark-inside-pairs
   er/mark-outside-pairs)

  :config
  (require 'sk-expand-defuns))

;; multiple cursors
(use-package multiple-cursors
  :ensure t
  :commands
  (mc/mark-pop
   mc/mmlte--up
   mc/edit-lines
   mc/mmlte--down
   mc/mmlte--left
   mc/mmlte--right
   mc/sort-regions
   mc/cycle-forward
   mc/keyboard-quit
   mc/cycle-backward
   mc/insert-letters
   mc/insert-numbers
   mc/repeat-command
   mc/vertical-align
   mc/reverse-regions
   mc/mark-all-dwim
   mc/mark-next-lines
   mc/mark-previous-lines
   mc/edit-ends-of-lines
   mc/mark-all-in-region
   mc/mark-all-like-this
   mc/mark-sgml-tag-pair
   mc/add-cursor-on-click
   mc/mark-next-like-this
   mc/unmark-next-like-this
   mc/toggle-cursor-on-click
   mc/mark-previous-like-this
   mc/edit-beginnings-of-lines
   mc/unmark-previous-like-this
   mc/vertical-align-with-space
   mc/skip-to-next-like-this
   mc/mark-all-like-this-dwim
   mc/mark-all-words-like-this
   mc/mark-next-like-this-word
   mc/mark-next-word-like-this
   mc/mark-all-in-region-regexp
   mc/mark-all-symbols-like-this
   mc/mark-next-like-this-symbol
   mc/mark-next-symbol-like-this
   mc/skip-to-previous-like-this
   mc/mark-more-like-this-extended
   mc/mark-previous-like-this-word
   mc/mark-previous-word-like-this
   mc/mark-previous-like-this-symbol
   mc/mark-previous-symbol-like-this
   mc/mark-all-like-this-in-defun
   mc/mark-all-words-like-this-in-defun
   mc/mark-all-symbols-like-this-in-defun))

;; work on indentation
(use-package indent-tools
  :ensure t
  :commands
  (indent-tools-comment
   indent-tools-indent
   indent-tools-demote
   indent-tools-minor-mode
   indent-tools-kill-tree
   indent-tools-goto-child
   indent-tools-kill-level
   indent-tools-goto-parent
   indent-tools-indent-paragraph
   indent-tools-select
   indent-tools-end-of-level
   indent-tools-indent-space
   indent-tools-goto-end-of-tree
   indent-tools-copy
   indent-tools-indent-end-of-defun
   indent-tools-indent-end-of-level
   indent-tools-goto-next-sibling
   indent-tools-goto-previous-sibling
   indent-tools-select-end-of-tree
   indent-tools-hydra/body)
  :config
  (indent-tools-minor-mode))

;; some wrapper functions for indent-tools
(defun sk/select-indent-tree ()
  "select the indent tree"
  (interactive)
  (indent-tools-select-end-of-tree)
  (exchange-point-and-mark)
  (indent-tools-select))
(defun sk/copy-indent-level ()
  "copies the indent level"
  (interactive)
  (indent-tools-copy "level"))
(defun sk/copy-indent-tree ()
  "copies the indent tree"
  (interactive)
  (indent-tools-copy "tree"))

;; surrounding changing based on expand region
(use-package embrace
  :ensure t
  :commands
  (embrace-commander))

;; safe operators
(use-package smartparens
  :ensure t
  :hook ((prog-mode . smartparens-strict-mode)
         (prog-mode . smartparens-mode)
         (text-mode . smartparens-mode))
  :diminish smartparens-strict-mode
  :diminish smartparens-mode
  :commands
  (sp-kill-hybrid-sexp
   smartparens-strict-mode
   smartparens-mode)
  :config
  (require 'smartparens-config)
  (smartparens-global-mode)
  (smartparens-global-strict-mode)
  (show-smartparens-global-mode))

;; convert cases
(use-package string-inflection
  :ensure t
  :commands (string-inflection-all-cycle))

;; fill programming modes
(use-package prog-fill
  :ensure t
  :commands
  (prog-fill))

;; install pdf-tools
(use-package pdf-tools
  :ensure t
  :magic ("%PDF" . pdf-view-mode)
  :config (pdf-tools-install)
  :hook ((pdf-view-mode . pdf-view-fit-page-to-window))
  :commands
  (pdf-view-next-page
   pdf-view-previous-page
   pdf-view-fit-page-to-window
   pdf-view-revert-buffer))

;; provide editing related packages
(provide 'sk-edit)
