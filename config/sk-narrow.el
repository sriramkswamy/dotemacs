;; Flex for fuzzy matching
(use-package flx-ido	; Flx matching for ido (includes flx)
  :ensure t)

;; Smex for a better M-x
(use-package smex	; much improved M-x
  :ensure t)

;; dumb jump selector
(setq dumb-jump-selector 'ivy)

;; ivy for everything
(use-package ivy
  :ensure t
  :demand t
  :commands
  (sk/enable-ryo-modal-mode)
  :diminish ivy-mode
  :init
  ;; set ivy height
  (setq ivy-height 10)
  ;; make sure it always stays that high
  (setq ivy-fixed-height-minibuffer t)
  ;; if flipped, the candidates are above the input
  ;; (setq ivy-flip t)
  ;; virtual buffers - combines recentf and bookmarks to ivy-switch-buffer
  ;; (setq ivy-use-virtual-buffers t)
  ;; full file names - useful when multiple files have same names
  (setq ivy-virtual-abbreviate 'full)
  ;; fuzzy everywhere except when searching for something
  (setq ivy-re-builders-alist
		'((swiper					. ivy--regex-plus)
		  (counsel-rg				. ivy--regex-plus)
		  (counsel-ag				. ivy--regex-plus)
		  (counsel-grep-or-swiper	. ivy--regex-plus)
		  (counsel-git-log			. ivy--regex-plus)
		  (counsel-set-variable		. ivy--regex-plus)
		  (counsel-package			. ivy--regex-plus)
		  (t						. ivy--regex-fuzzy)))
  :bind (("C-x b" . ivy-switch-buffer))
  :bind (:map ivy-minibuffer-map
			  ("C-w"		. backward-kill-word)
			  ("RET"		. ivy-alt-done)
			  ("TAB"		. ivy-dispatching-done-hydra)
			  ("C-x >"		. ivy-previous-line-and-call)
			  ("C-x <"		. ivy-next-line-and-call)
			  ("C-r"		. ivy-immediate-done)
			  ("C-SPC"		. ivy-toggle-calling)
			  ("C-c g"	    . ivy-avy)
			  ("C-s"		. ivy-restrict-to-matches))

  :config
  (define-key read-expression-map (kbd "C-r") 'counsel-minibuffer-history)
  ;; the hydra in ivy
  (use-package ivy-hydra
	:ensure t)
  (ivy-mode 1))

;; counsel in any comint derivatives
(use-package comint
  :bind (:map comint-mode-map
			  ("C-r" . counsel-minibuffer-history)))

;; create maps to eshell with the help of hook
(defun sk/eshell-counsel-map ()
  "creates a map for C-r to counsel-esh-history"
  (bind-key "C-r" 'counsel-minibuffer-history))
(add-hook 'eshell-mode-hook 'sk/eshell-counsel-map)

;; wrapper around ivy for many functions
(use-package counsel
  :ensure t
  :diminish counsel-mode
  :bind (("M-x" . counsel-M-x)
		 ("M-y" . counsel-yank-pop)
		 ("C-x C-f" . counsel-find-file)
		 ("C-h j" . counsel-describe-face)
		 ("C-h h" . counsel-set-variable)
		 ("C-h v" . counsel-describe-variable)
		 ("C-h a" . counsel-apropos)
		 ("C-h f" . counsel-describe-function)
		 ("C-x 8" . counsel-unicode-char))
  :commands (counsel-faces
			 counsel-recentf
			 counsel-package
			 counsel-find-file-extern
			 counsel-mark-ring
             counsel-set-variable)
  :init
  (setq counsel-grep-base-command
		"rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  :config
  (ivy-mode 1)
  (counsel-mode 1))

(defun sk/counsel-imenu-org-goto ()
  "use imenu in everything except org-mode"
  (interactive)
  (if (eq major-mode 'org-mode)
	  (counsel-org-goto)
	(counsel-imenu)))

(defun sk/counsel-ag-directory ()
  "use counsel ag to search for the input in the current directory"
  (interactive)
  (if (eq (file-remote-p default-directory) nil)
	  (counsel-ag)
	(call-interactively #'rgrep)))

(defun sk/counsel-ag-directory-selection (start end)
  "use counsel ag to search for the selection in the current directory"
  (interactive "r")
  (if (eq (file-remote-p default-directory) nil)
	  (counsel-ag (buffer-substring-no-properties start end)
				  default-directory)
	(call-interactively #'rgrep)))

(defun sk/counsel-org-folder ()
  "use counsel ag to search for the the current directory"
  (interactive)
  (if (eq (file-remote-p default-directory) nil)
	  (counsel-ag "" org-directory)
	(call-interactively #'rgrep)))

;; search word under cursor
(defun sk/swiper-at-point ()
  "use swiper to search for a word at point"
  (interactive)
  (swiper (thing-at-point 'symbol)))

;; search in all files
(defun sk/swiper-all-at-point ()
  "use swiper to search for a word at point in all buffers"
  (interactive)
  (swiper-all (thing-at-point 'symbol)))

;; search the buffer or all buffer
(use-package swiper
  :ensure t
  :bind (("C-s" . swiper))
  :commands
  (swiper-all
   sk/swiper-at-point)
  :config
  (ivy-mode 1)
  (counsel-mode 1))

;; correct spellings
(use-package flyspell-correct-ivy
  :ensure t
  :commands
  (flyspell-correct-previous-word-generic)
  :config
  (require 'flyspell-ivy)
  (ivy-mode 1)
  (counsel-mode 1))

;; search using spotlight
(use-package spotlight
  :ensure t
  :commands
  (spotlight)
  :config
  (ivy-mode 1)
  (counsel-mode 1))

;; bibliography and citations
(use-package ivy-bibtex
  :ensure t
  :commands
  (ivy-bibtex
   sk/default-bib
   sk/current-bib)
  :init
  ;; where are the bib files
  (setq bibtex-completion-bibliography
		'("~/Dropbox/PhD/articles/tensors/tensors.bib"
		  "~/Dropbox/PhD/articles/datatensors/datatensors.bib"
		  "~/Dropbox/PhD/articles/association/association.bib"
		  "~/Dropbox/PhD/articles/lorenz/lorenz.bib"
		  "~/Dropbox/PhD/articles/multiphysics/multiphysics.bib"))
  ;; where are the pdfs
  (setq bibtex-completion-library-path
		'("~/Dropbox/PhD/articles/tensors"
		  "~/Dropbox/PhD/articles/datatensors"
		  "~/Dropbox/PhD/articles/association"
		  "~/Dropbox/PhD/articles/lorenz"
		  "~/Dropbox/PhD/articles/multiphysics"))
  ;; where is the notes
  (setq bibtex-completion-notes-path "~/Dropbox/PhD/articles/articles.org")
  ;; what is the default action
  (setq ivy-bibtex-default-action 'bibtex-completion-insert-key)
  ;; some handy visual markers
  (setq bibtex-completion-pdf-symbol "⌘")
  (setq bibtex-completion-notes-symbol "✎")
  :config
  (defun sk/current-bib ()
	"Set the bibliography and library to the current directory"
	(interactive)
	;; where are the bib files
	(setq bibtex-completion-bibliography
		  (concat (vc-root-dir) "references.bib"))
	;; where are the pdfs
	(setq bibtex-completion-library-path
		  (concat (vc-root-dir) "references"))
	;; do the same thing for org-ref
	(setq org-ref-default-bibliography
		  (concat (vc-root-dir) "references.bib"))
	(setq org-ref-pdf-directory
		  (concat (vc-root-dir) "references"))
	(message (concat "Bib file is " (concat (vc-root-dir))
					 "references.bib")))
  (defun sk/default-bib ()
	"Set the bibliography and library to the defaults"
	(interactive)
	;; where are the bib files
	(setq bibtex-completion-bibliography
		  '("~/Dropbox/PhD/articles/tensors/tensors.bib"
			"~/Dropbox/PhD/articles/datatensors/datatensors.bib"
			"~/Dropbox/PhD/articles/association/association.bib"
			"~/Dropbox/PhD/articles/lorenz/lorenz.bib"
			"~/Dropbox/PhD/articles/multiphysics/multiphysics.bib"))
	;; where are the pdfs
	(setq bibtex-completion-library-path
		  '("~/Dropbox/PhD/articles/tensors"
			"~/Dropbox/PhD/articles/datatensors"
			"~/Dropbox/PhD/articles/association"
			"~/Dropbox/PhD/articles/lorenz"
			"~/Dropbox/PhD/articles/multiphysics"))
	;; same thing for org-ref
	(setq org-ref-default-bibliography
		  '("~/Dropbox/PhD/articles/tensors/tensors.bib"
			"~/Dropbox/PhD/articles/datatensors/datatensors.bib"
			"~/Dropbox/PhD/articles/association/association.bib"
			"~/Dropbox/PhD/articles/lorenz/lorenz.bib"
			"~/Dropbox/PhD/articles/multiphysics/multiphysics.bib"))
	(setq org-ref-pdf-directory
		  '("~/Dropbox/PhD/articles/tensors"
			"~/Dropbox/PhD/articles/datatensors"
			"~/Dropbox/PhD/articles/association"
			"~/Dropbox/PhD/articles/lorenz"
			"~/Dropbox/PhD/articles/multiphysics"))
	(message "Default bib files set"))
  (ivy-mode 1)
  (counsel-mode 1))

;; to navigate tags instead of helm-gtags
(use-package ggtags
  :ensure t
  :diminish (ggtags-mode)
  :diminish (ggtags-navigation-mode)
  :commands (ggtags-find-tag-regexp
			 ggtags-find-reference
			 ggtags-find-other-symbol
			 ggtags-find-definition
             ggtags-create-tags
             ggtags-update-tags
			 ggtags-find-tag-dwim)
  :config
  (ggtags-mode)
  (ivy-mode 1)
  (counsel-mode 1))

;; editing grep results
(use-package wgrep
  :ensure t
  :commands (wgrep-change-to-wgrep-mode
			 wgrep-abort-changes
			 wgrep-save-all-buffers
			 wgrep-finish-edit)
  :config
  (ivy-mode 1)
  (counsel-mode 1))

;; project based grep at point
(defun sk/counsel-ag-project-at-point ()
  "use counsel ag to search for the word at point in the project"
  (interactive)
  (if (eq (file-remote-p default-directory) nil)
      (counsel-ag (thing-at-point 'symbol) (vc-root-dir))
    (counsel-git-grep (thing-at-point 'symbol))))

;; project based grep at point
(defun sk/counsel-ag-project-selection (start end)
  "use counsel ag to search for the selection in the project"
  (interactive "r")
  (if (eq (file-remote-p default-directory) nil)
	  (if (region-active-p)
		  (counsel-ag (buffer-substring-no-properties start end)
					  (vc-root-dir))
		(counsel-ag (thing-at-point 'symbol) (vc-root-dir)))
    (counsel-git-grep (thing-at-point 'symbol))))

;; vc based project file search
(defun sk/counsel-file-jump-project-git (arg)
  "use counsel file jump or counsel git to search the project files"
  (interactive "P")
  (if arg
	  (if (eq (file-remote-p default-directory) nil)
		  (counsel-file-jump "" (vc-root-dir))
		(project-find-file))
	(if (eq (file-remote-p default-directory) nil)
		(counsel-git)
	  (project-find-file))))

;; vc based project file search
(defun sk/counsel-file-jump-project-fzf (arg)
  "use counsel file jump or counsel git to search the project files"
  (interactive "P")
  (if arg
	  (if (eq (file-remote-p default-directory) nil)
		  (counsel-file-jump "" (vc-root-dir))
		(project-find-file))
	(if (eq (file-remote-p default-directory) nil)
		(counsel-fzf "" (car (project-roots (project-current t))))
	  (project-find-file))))

;; project based grep
(defun sk/counsel-ag-project ()
  "use counsel ag to search the project"
  (interactive)
  (if (eq (file-remote-p default-directory) nil)
      (counsel-ag "" (vc-root-dir))
    (counsel-git-grep)))

;; redefine ivy's dispatching hydra
(defun ivy-dispatching-done-hydra ()
  "Select one of the available actions and call `ivy-done'."
  (interactive)
  (let ((actions (ivy-state-action ivy-last)))
    (if (null (ivy--actionp actions))
        (ivy-done)
      (funcall
       (eval
        `(defhydra ivy-read-action (:color teal)
           "action"
           ,@(mapcar (lambda (x)
                       (list (nth 0 x)
                             `(progn
                                (ivy-set-action ',(nth 1 x))
                                (ivy-done))
                             (nth 2 x)))
                     (cdr actions))
           ("q" nil "back")))))))

;; completion mappings
(bind-key "C-r" 'counsel-company)

;; save ivy-views in desktop variables
(require 'desktop)
(add-to-list 'desktop-globals-to-save 'ivy-views)

;; bookmarks
(defun sk/counsel-bookmarks (arg)
  "post the gist. Toggle privacy with ARG"
  (interactive "p")
  (if (equal arg '(4))
	  (counsel-bookmark)
	(bookmark-set)))

;; provide the narrowing configuration
(provide 'sk-narrow)
