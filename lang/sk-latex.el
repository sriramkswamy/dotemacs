;; scan and call reftex - useful when using with yasnippet
(defun sk/reftex-reference ()
  "scan and call reftex-reference"
  (interactive)
  (let ((current-prefix-arg 4)) ;; emulate C-u
	(call-interactively 'reftex-reference)))

(use-package latex
  :ensure auctex
  :ensure company-auctex
  :ensure company-reftex
  :defer t
  :diminish reftex-mode
  :hook ((auctex-mode . sk/setup-ryo-latex-bindings)
		 (auctex-mode . sk/company-auctex))
  :bind* (("C-j a e" . company-auctex-environments)
		  ("C-j a l" . company-auctex-labels)
		  ("C-j a b" . company-auctex-bibs)
		  ("C-j a s" . company-auctex-symbols)
		  ("C-j a m" . company-auctex-macros))
  
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
		 ("\\.xtx\\'" . latex-mode)
		 ("\\.bib\\'" . bibtex-mode))

  :bind (:map TeX-mode-map
			  ("C-d" . company-auctex-symbols))
  ;; :bind (:map latex-mode-map
  ;; 			  ("C-d" . company-auctex-symbols))
  ;; :bind (:map LaTeX-mode-map
  ;; 			  ("C-d" . company-auctex-symbols))

  :init
  (setq reftex-plug-into-AUCTeX t)
  (setq reftex-default-bibliography
		'("~/Dropbox/Kumar/Conferences/ACC2019/references.bib"
		  "~/Dropbox/Kumar/Conferences/GNC2018/references.bib"
		  "~/Dropbox/Kumar/Conferences/GNC2019/references.bib"
		  "~/Dropbox/Kumar/Conferences/ICSSA 2017/paper/references.bib"
		  "~/Dropbox/Kumar/Journals/journal2018-applications/references.bib"
		  "~/Dropbox/Kumar/Journals/journal2018-method/references.bib"
		  "~/Dropbox/Kumar/Proposal/presentation/references.bib"
		  "~/Dropbox/Kumar/Proposal/references.bib"))
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
  (cond
   ((eq system-type 'darwin)
	(setq TeX-view-program-list
		  '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b"))))
   ((eq system-type 'gnu/linux)
	(setq TeX-view-program-list
		  '(("PDF Viewer" "evince")))))

  ;; turn on reftex
  (reftex-mode))
(add-hook 'bibtex-mode-hook 'sk/enable-ryo-modal-mode)

;; latex mode bindings
(ryo-modal-major-mode-keys
 'latex-mode
 ("m x" preview-at-point :name "preview at point")
 ("m a" preview-buffer :name "preview buffer")
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
(which-key-add-key-based-replacements
  "m q" "fill")

;; provide latex configuration
(provide 'sk-latex)
