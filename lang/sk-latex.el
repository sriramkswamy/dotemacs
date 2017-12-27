;; scan and call reftex - useful when using with yasnippet
(defun sk/reftex-reference ()
  "scan and call reftex-reference"
  (interactive)
  (let ((current-prefix-arg 4)) ;; emulate C-u
	(call-interactively 'reftex-reference)))

(use-package latex
  :ensure auctex
  :defer t
  :diminish reftex-mode
  :hook ((latex-mode . sk/setup-ryo-latex-bindings))
  
  :commands
  (LaTeX-mark-section
   LaTeX-mark-environment
   reftex-reference
   reftex-label
   reftex-citation
   reftex-view-crossref
   reftex-toc-recenter
   reftex-index
   preview-at-point
   preview-buffer
   LaTeX-fill-section
   LaTeX-fill-environment
   LaTeX-fill-paragraph
   LaTeX-fill-region
   reftex-reference
   reftex-label
   reftex-citation
   reftex-view-crossref
   reftex-toc-recenter
   reftex-index
   LaTeX-insert-environment
   LaTeX-section
   TeX-view
   TeX-master-file-ask
   TeX-home-buffer
   TeX-command-run-all
   TeX-recenter-output-buffer
   TeX-kill-job
   preview-clearout-buffer)
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

;; latex mode bindings
(ryo-modal-keys
 ("\\ x" preview-at-point :name "preview at point")
 ("\\ a" preview-buffer :name "preview buffer")
 ("\\ q s" LaTeX-fill-section :name "fill section")
 ("\\ q e" LaTeX-fill-environment :name "fill env")
 ("\\ q p" LaTeX-fill-paragraph :name "fill para")
 ("\\ q q" LaTeX-fill-region :name "fill region")
 ("\\ r" reftex-reference :name "ref")
 ("\\ l" reftex-label :name "label")
 ("\\ c" reftex-citation :name "cite")
 ("\\ j" reftex-view-crossref :name "crossrefs")
 ("\\ t" reftex-toc-recenter :name "table of contents")
 ("\\ i" reftex-index :name "index")
 ("\\ e" LaTeX-insert-environment :name "environment")
 ("\\ s" LaTeX-section :name "section")
 ("\\ v" TeX-view :name "view")
 ("\\ f" TeX-master-file-ask :name "master file")
 ("\\ h" TeX-home-buffer :name "home buffer")
 ("\\ \\" TeX-command-run-all :name "compile")
 ("\\ o" TeX-recenter-output-buffer :name "compile output")
 ("\\ k" TeX-kill-job :name "kill compilation")
 ("\\ d" preview-clearout-buffer :name "clear preview"))
;; hints
(which-key-add-key-based-replacements
  "\\" "auctex"
  "\\ q" "fill")

;; provide latex configuration
(provide 'sk-latex)
