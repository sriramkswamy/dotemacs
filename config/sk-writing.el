;; pick out weasel words
(use-package writegood-mode
  :ensure t
  :diminish writegood-mode
  :bind* (("C-c {" . writegood-grade-level)
		  ("C-c }" . writegood-reading-ease))
  :config
  (progn
	(add-hook 'text-mode-hook 'writegood-mode)))

;; count the number of words - useful for org mode
(use-package wc-mode
  :ensure t
  :commands (wc
			 wc-mode))
(ryo-modal-key "%" 'wc)
(which-key-add-key-based-replacements
  "%" "word count")

;;;;;;;;;;;;;;;;;;;;
;;    Markdown    ;;
;;;;;;;;;;;;;;;;;;;;

;; wrapper function for hexo watchandcode blog
(defun sk/hexo-watchandcode ()
  "open watch and code blog"
  (interactive)
  (hexo "~/Downloads/JS/watchandcode"))

;; wrapper function for personal blog
(defun sk/hexo-personal ()
  "open dropbox blog"
  (interactive)
  (hexo "~/Dropbox/blog"))

;; markdown and pandoc support
(use-package markdown-mode
  :ensure t
  :mode (("\\.txt\\'" . markdown-mode)
		 ("\\.md\\'" . markdown-mode)
         ("\\.mkd\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))

  :init
  (setq markdown-open-command "/Applications/Markoff.app/Contents/MacOS/Markoff")
  :config

  ;; pandoc
  (use-package pandoc-mode
	:ensure t
	:diminish pandoc-mode
	:config
	(add-hook 'markdown-mode-hook 'pandoc-mode))

  ;; hexo
  (use-package hexo
	:ensure t
	:commands (hexo-new
			   hexo-command-open-file
			   hexo-command-show-article-info
			   hexo-command-rename-file
			   hexo-command-revert-tabulated-list
			   tabulated-list-sort
			   hexo-command-filter-tag
			   hexo-move-article
			   hexo-touch-files-in-dir-by-time
			   hexo-toggle-article-status
			   hexo-command-tags-toggler
			   hexo-command-mark
			   hexo-command-unmark
			   hexo-command-add-tags
			   hexo-command-remove-tags
			   hexo-command-help
			   hexo-server-run
			   hexo-server-stop
			   hexo-server-deploy
			   hexo-insert-article-link
			   hexo-insert-file-link
			   hexo-update-current-article-date
			   hexo-follow-post-link
			   kill-buffer-and-window))

  ;; nice hooks
  (add-hook 'markdown-mode-hook 'visual-line-mode)
  (add-hook 'gfm-mode-hook 'visual-line-mode)

  ;; some global bindings
  (ryo-modal-key "DEL" 'mode-line-previous-buffer :mode 'markdown-mode)

  ;; navigation
  (ryo-modal-key "m j" 'markdown-next-visible-heading :mode 'markdown-mode)
  (ryo-modal-key "m k" 'markdown-previous-visible-heading :mode 'markdown-mode)
  (ryo-modal-key "m h" 'markdown-up-heading :mode 'markdown-mode)
  ;; organizing
  (ryo-modal-key "m RET" 'markdown-follow-thing-at-point :mode 'markdown-mode)
  (ryo-modal-key "m d" 'markdown-kill-thing-at-point :mode 'markdown-mode)
  (ryo-modal-key "m [" 'markdown-move-up :mode 'markdown-mode)
  (ryo-modal-key "m ]" 'markdown-move-down :mode 'markdown-mode)
  (ryo-modal-key "m <" 'markdown-promote :mode 'markdown-mode)
  (ryo-modal-key "m >" 'markdown-demote :mode 'markdown-mode)
  ;; inserting
  (ryo-modal-key "m i i" 'markdown-insert-italic :mode 'markdown-mode :exit t)
  (ryo-modal-key "m i b" 'markdown-insert-bold :mode 'markdown-mode :exit t)
  (ryo-modal-key "m i h" 'markdown-insert-hr :mode 'markdown-mode :exit t)
  (ryo-modal-key "m i l" 'markdown-insert-link :mode 'markdown-mode :exit t)
  (ryo-modal-key "m i r" 'markdown-insert-reference-link :mode 'markdown-mode :exit t)
  (ryo-modal-key "m i w" 'markdown-insert-wiki-link :mode 'markdown-mode :exit t)
  (ryo-modal-key "m i u" 'markdown-insert-uri :mode 'markdown-mode :exit t)
  (ryo-modal-key "m i f" 'markdown-insert-footnote :mode 'markdown-mode :exit t)
  (ryo-modal-key "m i m" 'markdown-insert-image :mode 'markdown-mode :exit t)
  (ryo-modal-key "m i g" 'markdown-insert-reference-image :mode 'markdown-mode :exit t)
  (ryo-modal-key "m i c" 'markdown-insert-gfm-code-block :mode 'markdown-mode :exit t)
  (ryo-modal-key "m i p" 'markdown-insert-pre :mode 'markdown-mode :exit t)
  (ryo-modal-key "m i q" 'markdown-insert-blockquote :mode 'markdown-mode :exit t)
  (ryo-modal-key "m i t" 'markdown-insert-list-item :mode 'markdown-mode :exit t)
  (ryo-modal-key "m i k" 'markdown-insert-kbd :mode 'markdown-mode :exit t)
  (ryo-modal-key "m i 1" 'markdown-insert-header-atx-1 :mode 'markdown-mode :read t)
  (ryo-modal-key "m i 2" 'markdown-insert-header-atx-2 :mode 'markdown-mode :read t)
  (ryo-modal-key "m i 3" 'markdown-insert-header-atx-3 :mode 'markdown-mode :read t)
  (ryo-modal-key "m i 4" 'markdown-insert-header-atx-4 :mode 'markdown-mode :read t)
  (ryo-modal-key "m i 5" 'markdown-insert-header-atx-5 :mode 'markdown-mode :read t)
  (ryo-modal-key "m i 6" 'markdown-insert-header-atx-6 :mode 'markdown-mode :read t)
  (ryo-modal-key "m i =" 'markdown-insert-header-setext-1 :mode 'markdown-mode)
  (ryo-modal-key "m i -" 'markdown-insert-header-setext-2 :mode 'markdown-mode)
  (ryo-modal-key "m r p" 'markdown-insert-pre :mode 'markdown-mode :exit t)
  (ryo-modal-key "m r q" 'markdown-insert-blockquote :mode 'markdown-mode :exit t)

  ;; preview
  (ryo-modal-key "m l" 'markdown-toggle-inline-images :mode 'markdown-mode)
  (ryo-modal-key "m m" 'markdown-export :mode 'markdown-mode)
  (ryo-modal-key "m x" 'markdown-export-and-preview :mode 'markdown-mode)
  (ryo-modal-key "m v" 'markdown-preview :mode 'markdown-mode)
  (ryo-modal-key "m c" 'markdown-check-refs :mode 'markdown-mode)
  (ryo-modal-key "m n" 'markdown-cleanup-list-numbers :mode 'markdown-mode)
  (ryo-modal-key "m o" 'markdown-complete-buffer :mode 'markdown-mode)
  (ryo-modal-key "m w" 'markdown-other-window :mode 'markdown-mode)
  ;; pandoc
  (ryo-modal-key "m g" 'pandoc-main-hydra/body :mode 'markdown-mode))

;; ryo bindings
(ryo-modal-key "SPC z h" 'sk/hexo-personal)
(ryo-modal-key "SPC z w" 'sk/hexo-watchandcode)

;; ryo major mode hints
(which-key-add-major-mode-key-based-replacements 'markdown-mode
  "m i" "insert"
  "m r" "region"
  "m j" "next heading"
  "m k" "prev heading"
  "m h" "up heading"
  "m RET" "jump"
  "m g" "goto"
  "m d" "delete"
  "m [" "move up"
  "m ]" "move down"
  "m <" "promote"
  "m >" "demote"
  "m i i" "italic"
  "m i b" "bold"
  "m i h" "horizontal rule"
  "m i l" "link"
  "m i r" "reference link"
  "m i w" "wiki link"
  "m i u" "uri"
  "m i f" "footnote"
  "m i m" "image"
  "m i g" "reference image"
  "m i c" "gfm code"
  "m i p" "pre"
  "m i q" "blockquote"
  "m i t" "list item"
  "m i k" "kbd"
  "m i 1" "atx h1"
  "m i 2" "atx h2"
  "m i 3" "atx h3"
  "m i 4" "atx h4"
  "m i 5" "atx h5"
  "m i 6" "atx h6"
  "m i =" "setext h1"
  "m i -" "setext h2"
  "m r p" "pre"
  "m r q" "blockquote"
  "m l" "toggle images"
  "m e" "export"
  "m x" "export and preview"
  "m v" "preview"
  "m c" "check refs"
  "m n" "cleanup numbers"
  "m o" "complete buffer"
  "m m" "other win"
  "m p" "pandoc")

;; hexo mode
(which-key-add-major-mode-key-based-replacements 'hexo-mode
  "SPC" "info"
  "R" "rename"
  "h" "help"
  "T T" "touch files"
  "f" "filter tag"
  "RET" "open file"
  "g" "refresh"
  "S" "sort"
  "T s" "article status"
  "t" "toggle tags"
  "m" "mark"
  "M" "marked actions"
  "M a" "add tags"
  "M r" "remove tags"
  "u" "unmark"
  "s" "server"
  "s r" "run"
  "s s" "stop"
  "s d" "deploy"
  "Q" "quit")

;;;;;;;;;;;;;;;;;
;;    LaTeX    ;;
;;;;;;;;;;;;;;;;;

;; LaTeX support
(use-package tex-site
  :defer 2
  :ensure auctex
  :ensure auctex-latexmk
  :ensure latex-preview-pane
  :commands (LaTeX-mark-section
			 LaTeX-mark-environment
			 reftex-reference
			 reftex-label
			 reftex-citation
			 reftex-view-crossref
			 reftex-toc-recenter
			 reftex-index
			 latex-preview-pane
			 latex-preview-pane-enable
			 latex-preview-pane-mode)
  :mode (("\\.tex\\'" . latex-mode)
		 ("\\.xtx\\'" . latex-mode))
  :init
  (setq reftex-plug-into-AUCTeX t)
  (setq reftex-default-bibliography
		'("~/Dropbox/PhD/articles/tensors/tensors.bib"
		  "~/Dropbox/PhD/articles/datatensors/datatensors.bib"
		  "~/Dropbox/PhD/articles/association/association.bib"
		  "~/Dropbox/PhD/articles/lorenz/lorenz.bib"
		  "~/Dropbox/PhD/articles/multiphysics/multiphysics.bib"))
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq TeX-PDF-mode t)
  :config
  (require 'tex)
  (require 'tex-buf)
  (require 'tex-bar)
  (require 'tex-fold)
  (require 'tex-font)
  (require 'tex-site)
  (require 'tex-wizard)
  (require 'latex)
  (require 'font-latex)
  (require 'preview)
  (require 'texmathp)
  ;; Use Skim as viewer, enable source <-> PDF sync
  ;; make latexmk available via C-c C-c
  ;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
  (add-hook 'LaTeX-mode-hook (lambda ()
							   (push
								'("latexmk" "latexmk -xelatex -pdf %s" TeX-run-TeX nil t
								  :help "Run latexmk on file")
								TeX-command-list)))
  (add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))
  ;; use Skim as default pdf viewer
  ;; Skim's displayline is used for forward search (from .tex to .pdf)
  ;; option -b highlights the current line; option -g opens Skim in the background
  (setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
  (setq TeX-view-program-list
		'(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))
  ;; turn on reftex
  (reftex-mode)

  ;; bindings
  ;; preview
  (ryo-modal-key "m x" 'preview-at-point :mode 'latex-mode)
  (ryo-modal-key "m a" 'preview-buffer :mode 'latex-mode)
  (ryo-modal-key "m p" 'latex-preview-pane-mode :mode 'latex-mode)
  ;; fill
  (ryo-modal-key "m q s" 'LaTeX-fill-section :mode 'latex-mode)
  (ryo-modal-key "m q e" 'LaTeX-fill-environment :mode 'latex-mode)
  (ryo-modal-key "m q p" 'LaTeX-fill-paragraph :mode 'latex-mode)
  (ryo-modal-key "m q q" 'LaTeX-fill-region :mode 'latex-mode)
  ;; reftex
  (ryo-modal-key "m r" 'reftex-reference :mode 'latex-mode)
  (ryo-modal-key "m l" 'reftex-label :mode 'latex-mode)
  (ryo-modal-key "m c" 'reftex-citation :mode 'latex-mode)
  (ryo-modal-key "m j" 'reftex-view-crossref :mode 'latex-mode)
  (ryo-modal-key "m t" 'reftex-toc-recenter :mode 'latex-mode)
  (ryo-modal-key "m i" 'reftex-index :mode 'latex-mode)
  ;; inserting
  (ryo-modal-key "m e" 'LaTeX-insert-environment :mode 'latex-mode)
  (ryo-modal-key "m s" 'LaTeX-section :mode 'latex-mode)
  ;; processing
  (ryo-modal-key "m v" 'TeX-view :mode 'latex-mode)
  (ryo-modal-key "m f" 'TeX-master-file-ask :mode 'latex-mode)
  (ryo-modal-key "m h" 'TeX-home-buffer :mode 'latex-mode)
  (ryo-modal-key "m m" 'TeX-command-run-all :mode 'latex-mode)
  (ryo-modal-key "m o" 'TeX-recenter-output-buffer :mode 'latex-mode)
  (ryo-modal-key "m k" 'TeX-kill-job :mode 'latex-mode)
  (ryo-modal-key "m d" 'preview-clearout-buffer :mode 'latex-mode))
(add-hook 'latex-mode-hook 'latex-preview-pane-enable)

;; custom function to setup latex mode properly - why isn't use-package definition working?
(defun sk/latex-setup ()
  "hooks for latex"
  (interactive)
  ;; (auctex-latexmk-setup)
  (flyspell-mode)
  (visual-line-mode)
  (reftex-mode)
  (turn-on-reftex))
(add-hook 'latex-mode-hook 'sk/latex-setup)
(defun sk/diminish-reftex ()
  "diminish reftex because use-package is unable to do it"
  (interactive)
  (diminish 'reftex-mode))
(add-hook 'reftex-mode-hook 'sk/diminish-reftex)

;; ryo major mode hints
(which-key-add-major-mode-key-based-replacements 'latex-mode

  "m q" "fill"
  "m q s" "fill section"
  "m q e" "fill env"
  "m q p" "fill para"
  "m q q" "fill region"
  "m p" "live preview"
  "m s" "sections"
  "m v" "tex view"
  "m e" "environment"
  "m x" "preview"
  "m a" "preview all"
  "m c" "citation"
  "m r" "reference"
  "m l" "label"
  "m j" "jump to ref"
  "m t" "table of contents"
  "m i" "index"
  "m m" "run command"
  "m d" "delete preview"
  "m h" "home buffer"
  "m o" "output buffer"
  "m f" "master file"
  "m k" "kill job")

;;;;;;;;;;;;;;;;;;
;;    BibTeX    ;;
;;;;;;;;;;;;;;;;;;

(use-package bibtex
  :config
  ;; bindings
  (ryo-modal-key "m a" 'bibtex-autofill-entry :mode 'bibtex-mode)
  (ryo-modal-key "m b" 'bibtex-empty-field :mode 'bibtex-mode)
  (ryo-modal-key "m c" 'bibtex-ispell-entry :mode 'bibtex-mode)
  (ryo-modal-key "m d" 'bibtex-kill-entry :mode 'bibtex-mode)
  (ryo-modal-key "m i" 'bibtex-entry-update :mode 'bibtex-mode)
  (ryo-modal-key "m f" 'bibtex-make-field :mode 'bibtex-mode)
  (ryo-modal-key "m g" 'bibtex-print-help-message :mode 'bibtex-mode)
  (ryo-modal-key "m h" 'bibtex-beginning-of-entry :mode 'bibtex-mode)
  (ryo-modal-key "m e" 'bibtex-entry :mode 'bibtex-mode)
  (ryo-modal-key "m j" 'bibtex-next-field :mode 'bibtex-mode)
  (ryo-modal-key "m l" 'bibtex-end-of-entry :mode 'bibtex-mode)
  (ryo-modal-key "m m" 'bibtex-narrow-to-entry :mode 'bibtex-mode)
  (ryo-modal-key "m n" 'bibtex-skip-to-valid-entry :mode 'bibtex-mode)
  (ryo-modal-key "m o" 'bibtex-count-entries :mode 'bibtex-mode)
  (ryo-modal-key "m p" 'bibtex-yank :mode 'bibtex-mode)
  (ryo-modal-key "m q e" 'bibtex-fill-entry :mode 'bibtex-mode)
  (ryo-modal-key "m q f" 'bibtex-fill-field :mode 'bibtex-mode)
  (ryo-modal-key "m r" 'bibtex-search-crossref :mode 'bibtex-mode)
  (ryo-modal-key "m s" 'bibtex-search-entries :mode 'bibtex-mode)
  (ryo-modal-key "m u" 'bibtex-url :mode 'bibtex-mode)
  (ryo-modal-key "m v" 'bibtex-mark-entry :mode 'bibtex-mode)
  (ryo-modal-key "m x" 'bibtex-kill-field :mode 'bibtex-mode)
  (ryo-modal-key "m y e" 'bibtex-copy-entry-as-kill :mode 'bibtex-mode)
  (ryo-modal-key "m y f" 'bibtex-copy-field-as-kill :mode 'bibtex-mode)
  (ryo-modal-key "m y s" 'bibtex-copy-summary-as-kill :mode 'bibtex-mode)
  (ryo-modal-key "m z" 'bibtex-remove-OPT-or-ALT :mode 'bibtex-mode))
(add-hook 'bibtex-mode-hook 'sk/enable-ryo-modal-mode)

(which-key-add-major-mode-key-based-replacements 'bibtex-mode
  "m q" "fill"
  "m q e" "entry"
  "m q p" "field"
  "m p" "paste"
  "m b" "blank field"
  "m x" "kill field"
  "m i" "entry update"
  "m e" "new entry"
  "m v" "mark entry"
  "m a" "autofill entry"
  "m c" "correct spelling"
  "m r" "crossrefs"
  "m l" "end of entry"
  "m j" "next field"
  "m m" "narrow to entry"
  "m d" "delete entry"
  "m h" "beginning of entry"
  "m o" "count entries"
  "m f" "make field"
  "m n" "next entry"
  "m s" "search entries"
  "m u" "url"
  "m y" "yank"
  "m y e" "entry"
  "m y f" "field"
  "m y s" "summary"
  "m z" "remove OPT or ALT"
  "m g" "help message")

;; provide this configuration
(provide 'sk-writing)
