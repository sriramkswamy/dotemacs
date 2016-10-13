;; Flex for fuzzy matching
(use-package flx-ido	; Flx matching for ido (includes flx)
  :ensure t)

;; Smex for a better M-x
(use-package smex	; much improved M-x
  :ensure t)

;; ivy for everything
(use-package ivy
  :ensure t
  :defer 1
  :diminish ivy-mode
  :init
  ;; set ivy height
  (setq ivy-height 10)
  ;; make sure it always stays that high
  ;; (setq ivy-fixed-height-minibuffer t)
  ;; if flipped, the candidates are above the input
  ;; (setq ivy-flip t)
  ;; virtual buffers - combines many good things into one command
  (setq ivy-use-virtual-buffers t)
  ;; full file names - useful when multiple files have same names
  (setq ivy-virtual-abbreviate 'full)
  ;; fuzzy everywhere except when searching for something
  (setq ivy-re-builders-alist
		'((swiper . ivy--regex-plus)
		  (counsel-ag . ivy--regex-plus)
		  (counsel-grep-or-swiper . ivy--regex-plus)
		  (t . ivy--regex-fuzzy)))
  :bind (("C-x C-b" . ivy-switch-buffer))
  :bind (:map ivy-minibuffer-map
			  ("C-w" . backward-kill-word)
			  ("RET" . ivy-alt-done)
			  ("TAB" . ivy-dispatching-done)
			  ("C-c C-j" . ivy-immediate-done)
			  ("C-c C-t" . ivy-avy)
			  ("C-S-m" . ivy-restrict-to-matches)
			  ("S-<return>" . ivy-restrict-to-matches))
  :bind* (("C-c r" . ivy-resume)
		  ("M-s i" . ivy-wgrep-change-to-wgrep-mode)
		  ("M-s C-i" . ivy-wgrep-change-to-wgrep-mode))
  :config
  ;; the hydra in ivy
  (use-package ivy-hydra
	:ensure t)
  (ivy-mode 1)
  (counsel-mode 1))

;; wrapper around ivy for many functions
(use-package counsel
  :ensure t
  :diminish counsel-mode
  :bind (("M-x" . counsel-M-x)
		 ("M-y" . counsel-yank-pop)
		 ("C-x C-f" . counsel-find-file)
		 ("C-h v" . counsel-describe-variable)
		 ("C-h f" . counsel-describe-function)
		 ("C-x 8" . counsel-unicode-char))
  :bind* (("M-s c" . counsel-ag)
		  ("M-s C-c" . counsel-ag)
		  ("C-c t" . counsel-imenu)
		  ("C-c ." . counsel-load-theme))
  :config
  (ivy-mode 1)
  (counsel-mode 1))

;; project files
(use-package counsel-projectile
  :ensure t
  :bind* (("M-s p" . sk/counsel-ag-project)
		  ("M-s C-p" . sk/counsel-ag-project)
		  ("M-s j" . sk/counsel-ag-project-at-point)
		  ("M-s C-j" . sk/counsel-ag-project-at-point)
		  ("C-c p" . counsel-projectile-find-file-or-buffer)
		  ("C-|" . projectile-find-other-file-other-window))
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (defun sk/counsel-ag-project-at-point ()
	"use counsel ag to search for the word at point in the project"
	(interactive)
	(counsel-ag (thing-at-point 'symbol) (projectile-project-root)))
  (defun sk/counsel-ag-project ()
	"use counsel ag to search the project"
	(interactive)
	(counsel-ag "" (projectile-project-root)))
  (ivy-mode 1)
  (counsel-mode 1))

;; search the buffer or all buffer
(use-package swiper
  :ensure t
  :bind* (("M-s s" . swiper-all)
		  ("M-s C-s" . swiper-all)
		  ("C-s" . counsel-grep-or-swiper)
		  ("M-s r" . sk/swiper-at-point)
		  ("M-s C-r" . sk/swiper-at-point))
  :config
  (defun sk/swiper-at-point ()
	"use swiper to search for a word at point"
	(interactive)
	(swiper (thing-at-point 'word)))
  (ivy-mode 1)
  (counsel-mode 1))

;; correct spellings
(use-package flyspell-correct-ivy
  :ensure t
  :bind* (("C-c /" . flyspell-correct-previous-word-generic))
  :config
  (require 'flyspell-ivy)
  (ivy-mode 1)
  (counsel-mode 1))

;; search using spotlight
(use-package spotlight
  :ensure t
  :bind* (("C-c f" . spotlight))
  :config
  (ivy-mode 1)
  (counsel-mode 1))

;; to narrow down org files - because helm has helm-org-rifle. This is the closest
(use-package deft
  :ensure t
  :init
  (setq deft-extensions '("org" "txt"))
  (setq deft-directory "~/Dropbox/org")
  :bind* (("C-c d" . deft))
  :config
  (ivy-mode 1)
  (counsel-mode 1))

;; bibliography and citations
(use-package ivy-bibtex
  :ensure t
  :bind* (("C-c n" . ivy-bibtex)
		  ("C-c , d" . sk/default-bib)
		  ("C-c , c" . sk/current-bib))
  :init
  ;; where is the notes
  (setq bibtex-completion-notes-path "~/Dropbox/org/articles.org")
  ;; what is the default action
  (setq ivy-bibtex-default-action 'bibtex-completion-insert-key)
  ;; some handy visual markers
  (setq bibtex-completion-pdf-symbol "⌘")
  (setq bibtex-completion-notes-symbol "✎")
  :config
  (defun sk/current-bib ()
	"Set the bibliography and library to the current directory"
	(interactive)
	(setq bibtex-completion-bibliography
		  (concat (file-name-directory buffer-file-name) "references/references.bib"))
	;; where are the pdfs
	(setq bibtex-completion-library-path
		  (concat (file-name-directory buffer-file-name) "references"))
	(message (concat "Bib file is " (file-name-directory
									 buffer-file-name) "references/references.bib")))
  (defun sk/default-bib ()
	"Set the bibliography and library to the defaults"
	(interactive)
	;; where are the bib files
	(setq bibtex-completion-bibliography
		  '("~/Dropbox/PhD/articles/tensors/tensors.bib"
			"~/Dropbox/PhD/articles/machinelearning/machinelearning.bib"
			"~/Dropbox/PhD/articles/association/association.bib"
			"~/Dropbox/PhD/articles/lorenz/lorenz.bib"
			"~/Dropbox/PhD/articles/multiphysics/multiphysics.bib"))
	;; where are the pdfs
	(setq bibtex-completion-library-path
		  '("~/Dropbox/PhD/articles/tensors"
			"~/Dropbox/PhD/articles/machinelearning"
			"~/Dropbox/PhD/articles/association"
			"~/Dropbox/PhD/articles/lorenz"
			"~/Dropbox/PhD/articles/multiphysics"))
	(message "Default bib files set"))
  (ivy-mode 1)
  (counsel-mode 1))

;; Redefine ivy's hydra
(defhydra hydra-ivy (:hint nil :color pink)
  "
^ ^ ^ ^ ^ ^ | ^Call^      ^ ^  | ^Cancel^ | ^Options^ | Action _w_/_s_/_a_: %-14s(ivy-action-name)
^-^-^-^-^-^-+-^-^---------^-^--+-^-^------+-^-^-------+-^^^^^^^^^^^^^^^^^^^^^^^^^^^^^---------------------------
^ ^ _k_ ^ ^ | _f_ollow _o_ccur | _i_nsert | _c_: calling %-5s(if ivy-calling \"on\" \"off\") _C_ase-fold: %-10`ivy-case-fold-search
_h_ ^+^ _l_ | _d_one   _p_roj  | _q_uit   | _m_: matcher %-5s(ivy--matcher-desc)^^^^^^^^^^^^ _t_runcate: %-11`truncate-lines
^ ^ _j_ ^ ^ | _g_o     _D_efun | _n_arrow | _<_/_>_: shrink/grow^^^^^^^^^^^^^^^^^^^^^^^^^^^^ _v_/_V_: scroll up or down
"
  ;; arrows
  ("h" ivy-beginning-of-buffer)
  ("j" ivy-next-line)
  ("k" ivy-previous-line)
  ("l" ivy-end-of-buffer)
  ;; actions
  ("q" keyboard-escape-quit :exit t)
  ("C-g" keyboard-escape-quit :exit t)
  ("<escape>" nil)
  ("i" nil)
  ("p" counsel-projectile-drop-to-switch-project :color blue)
  ("C-o" nil)
  ("RET" ivy-alt-done :exit nil)
  ("f" ivy-alt-done :exit nil)
  ("C-j" ivy-alt-done :exit nil)
  ("d" ivy-done :exit t)
  ("g" ivy-call)
  ("o" ivy-occur :exit t)
  ("n" ivy-restrict-to-matches :color blue)
  ("C-m" ivy-done :exit t)
  ("c" ivy-toggle-calling)
  ("m" ivy-toggle-fuzzy)
  (">" ivy-minibuffer-grow)
  ("<" ivy-minibuffer-shrink)
  ("v" ivy-scroll-up-command)
  ("V" ivy-scroll-down-command)
  ("w" ivy-prev-action)
  ("s" ivy-next-action)
  ("a" ivy-read-action)
  ("TAB" ivy-dispatching-done)
  ("t" (setq truncate-lines (not truncate-lines)))
  ("C" ivy-toggle-case-fold)
  ("D" (ivy-exit-with-action
	(lambda (_) (find-function 'hydra-ivy/body)))
       :exit t))

;; provide the configuration
(provide 'sk-ivy)
