;; scan and call reftex - useful when using with yasnippet
(defun sk/reftex-reference ()
  "scan and call reftex-reference"
  (interactive)
  (let ((current-prefix-arg 4)) ;; emulate C-u
	(call-interactively 'reftex-reference)))

(use-package tex-site
  :ensure auctex
  :ensure auctex-latexmk
  :ensure latex-preview-pane
  :diminish reftex-mode
  :defer 1
  
  :commands
  (LaTeX-mark-section
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
  (reftex-mode))

;; bindings
(ryo-modal-major-mode-keys
 'latex-mode
 ("m x" preview-at-point :name "preview at point")
 ("m a" preview-buffer :name "preview buffer")
 ("m p" latex-preview-pane-mode :name "preview pane")
 ("m q s" LaTeX-fill-section :name "fill section")
 ("m q e" LaTeX-fill-environment :name "fill env")
 ("m q p" LaTeX-fill-paragraph :name "fill para")
 ("m q q" LaTeX-fill-region :name "fill region")
 ("m r" reftex-reference :name "ref")
 ("m l" reftex-label :name "label")
 ("m c" reftex-citation :name "cite")
 ("m j" reftex-view-crossref :name "crossrefs")
 ("m t" reftex-toc-recenter :name "table of contents")
 ("m i" reftex-index :name "index")
 ("m e" LaTeX-insert-environment :name "environment")
 ("m s" LaTeX-section :name "section")
 ("m v" TeX-view :name "view")
 ("m f" TeX-master-file-ask :name "master file")
 ("m h" TeX-home-buffer :name "home buffer")
 ("m m" TeX-command-run-all :name "compile")
 ("m o" TeX-recenter-output-buffer :name "compile output")
 ("m k" TeX-kill-job :name "kill compilation")
 ("m d" preview-clearout-buffer :name "clear preview"))
;; hints
(which-key-add-major-mode-key-based-replacements 'latex-mode
  "m q" "fill")

;; bindings
(ryo-modal-major-mode-keys
 'LaTeX-mode
 ("m x" preview-at-point :name "preview at point")
 ("m a" preview-buffer :name "preview buffer")
 ("m p" latex-preview-pane-mode :name "preview pane")
 ("m q s" LaTeX-fill-section :name "fill section")
 ("m q e" LaTeX-fill-environment :name "fill env")
 ("m q p" LaTeX-fill-paragraph :name "fill para")
 ("m q q" LaTeX-fill-region :name "fill region")
 ("m r" reftex-reference :name "ref")
 ("m l" reftex-label :name "label")
 ("m c" reftex-citation :name "cite")
 ("m j" reftex-view-crossref :name "crossrefs")
 ("m t" reftex-toc-recenter :name "table of contents")
 ("m i" reftex-index :name "index")
 ("m e" LaTeX-insert-environment :name "environment")
 ("m s" LaTeX-section :name "section")
 ("m v" TeX-view :name "view")
 ("m f" TeX-master-file-ask :name "master file")
 ("m h" TeX-home-buffer :name "home buffer")
 ("m m" TeX-command-run-all :name "compile")
 ("m o" TeX-recenter-output-buffer :name "compile output")
 ("m k" TeX-kill-job :name "kill compilation")
 ("m d" preview-clearout-buffer :name "clear preview"))
;; hints
(which-key-add-major-mode-key-based-replacements 'LaTeX-mode
  "m q" "fill")

;; bindings
(ryo-modal-major-mode-keys
 'TeX-mode
 ("m x" preview-at-point :name "preview at point")
 ("m a" preview-buffer :name "preview buffer")
 ("m p" latex-preview-pane-mode :name "preview pane")
 ("m q s" LaTeX-fill-section :name "fill section")
 ("m q e" LaTeX-fill-environment :name "fill env")
 ("m q p" LaTeX-fill-paragraph :name "fill para")
 ("m q q" LaTeX-fill-region :name "fill region")
 ("m r" reftex-reference :name "ref")
 ("m l" reftex-label :name "label")
 ("m c" reftex-citation :name "cite")
 ("m j" reftex-view-crossref :name "crossrefs")
 ("m t" reftex-toc-recenter :name "table of contents")
 ("m i" reftex-index :name "index")
 ("m e" LaTeX-insert-environment :name "environment")
 ("m s" LaTeX-section :name "section")
 ("m v" TeX-view :name "view")
 ("m f" TeX-master-file-ask :name "master file")
 ("m h" TeX-home-buffer :name "home buffer")
 ("m m" TeX-command-run-all :name "compile")
 ("m o" TeX-recenter-output-buffer :name "compile output")
 ("m k" TeX-kill-job :name "kill compilation")
 ("m d" preview-clearout-buffer :name "clear preview"))
;; hints
(which-key-add-major-mode-key-based-replacements 'TeX-mode
  "m q" "fill")

;; provide latex configuration
(provide 'sk-latex)
