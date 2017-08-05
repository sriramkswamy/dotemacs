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
		  (counsel-grep-or-swiper	. ivy--regex-plus)
		  (counsel-git-log			. ivy--regex-plus)
		  (counsel-set-variable		. ivy--regex-plus)
		  (counsel-package			. ivy--regex-plus)
		  (t						. ivy--regex-fuzzy)))
  :bind (("C-x C-b" . ivy-switch-buffer))
  :bind (:map ivy-minibuffer-map
			  ("C-w"		. backward-kill-word)
			  ("RET"		. ivy-alt-done)
			  ("TAB"		. ivy-dispatching-done-hydra)
			  ("C-x >"		. ivy-previous-line-and-call)
			  ("C-x <"		. ivy-next-line-and-call)
			  ("C-r"		. ivy-immediate-done)
			  ("C-SPC"		. ivy-toggle-calling)
			  ("C-c b"	    . ivy-avy)
			  ("C-s"		. ivy-restrict-to-matches))
  :bind* (("C-c r" . ivy-resume)
		  ("M-s i" . ivy-wgrep-change-to-wgrep-mode))
  :config
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
  ;; the hydra in ivy
  (use-package ivy-hydra
	:ensure t)
  (ivy-mode 1))

;; counsel in any comint derivatives
(use-package comint
  :bind (:map comint-mode-map
			  ("C-r" . counsel-shell-history)))

;; create maps to eshell with the help of hook
(defun sk/eshell-counsel-map ()
  "creates a map for C-r to counsel-esh-history"
  (bind-key "C-r" 'counsel-esh-history))
(add-hook 'eshell-mode-hook 'sk/eshell-counsel-map)

;; wrapper around ivy for many functions
(use-package counsel
  :ensure t
  :diminish counsel-mode
  :bind (("M-x" . counsel-M-x)
		 ("M-y" . counsel-yank-pop)
		 ("C-x C-f" . counsel-find-file)
		 ("C-h j" . counsel-describe-face)
		 ("C-h u" . counsel-set-variable)
		 ("C-h v" . counsel-describe-variable)
		 ("C-h a" . counsel-apropos)
		 ("C-h f" . counsel-describe-function)
		 ("C-x 8" . counsel-unicode-char))
  :bind* (("M-s c" . counsel-rg)
		  ("C-c t" . sk/counsel-imenu-org-goto))
  :commands (counsel-faces
			 counsel-recentf
			 counsel-package
			 counsel-find-file-extern
			 counsel-mark-ring
             counsel-set-variable)
  :config
  (ivy-mode 1)
  (counsel-mode 1))

(defun sk/counsel-imenu-org-goto ()
	"use imenu in everything except org-mode"
	(interactive)
	(if (eq major-mode 'org-mode)
		(counsel-org-goto)
	  (counsel-imenu)))

(defun sk/counsel-rg-directory ()
	"use counsel ag to search for the the current directory"
	(interactive)
	(if (eq (file-remote-p default-directory) nil)
		(counsel-rg)
	  (call-interactively #'rgrep)))

(defun sk/counsel-org-folder ()
	"use counsel ag to search for the the current directory"
	(interactive)
	(if (eq (file-remote-p default-directory) nil)
		(counsel-rg "" org-directory)
	  (call-interactively #'rgrep)))

;; project files
(use-package counsel-projectile
  :ensure t
  :diminish (projectile-mode . " π")
  :bind* (("M-s p" . sk/counsel-rg-project)
		  ("M-s s" . sk/counsel-rg-project-at-point)
		  ("C-c p" . counsel-projectile))
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (defun sk/counsel-rg-project-at-point ()
	"use counsel ag to search for the word at point in the project"
	(interactive)
	(if (eq (file-remote-p default-directory) nil)
		(counsel-rg (thing-at-point 'symbol) (vc-root-dir))
	  (counsel-git-grep (thing-at-point 'symbol))))
  (defun sk/counsel-rg-project ()
	"use counsel ag to search the project"
	(interactive)
	(if (eq (file-remote-p default-directory) nil)
		(counsel-rg "" (vc-root-dir))
	  (counsel-git-grep)))
  (projectile-mode)
  (ivy-mode 1)
  (counsel-mode 1))

;; projectile hook
(defun sk/diminish-projectile ()
  (interactive)
  (diminish 'projectile-mode " π"))
(add-hook 'projectile-mode-hook 'sk/diminish-projectile)

;; search the buffer or all buffer
(use-package swiper
  :ensure t
  :bind (("C-s" . counsel-grep-or-swiper))
  :bind* (("M-s l" . swiper-all)
		  ("M-s j" . sk/swiper-at-point))
  :config
  (defun sk/swiper-at-point ()
	"use swiper to search for a word at point"
	(interactive)
	(swiper (thing-at-point 'symbol)))
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

;; bibliography and citations
(use-package ivy-bibtex
  :ensure t
  :bind* (("C-c n" . ivy-bibtex)
		  ("C-c B d" . sk/default-bib)
		  ("C-c B c" . sk/current-bib))
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

;; counsel based navigation functions
(defun sk/find-file (arg)
  "Normal find file or counsel's find file jump"
  (interactive "P")
  (if arg
	  (counsel-file-jump)
	(counsel-find-file)))
(bind-key "C-x C-f" 'sk/find-file)

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

;; map ivy/counsel functions
(ryo-modal-keys
 ("t" sk/counsel-imenu-org-goto)
 ("T" ggtags-find-tag-regexp)
 ("B" sk/default-bib)
 ("E" ivy-occur)
 ("*" sk/swiper-at-point)
 ("y m" projectile-run-project)
 ("c u" counsel-projectile-switch-project)
 ("d c" counsel-projectile-find-dir)
 ("d u" projectile-commander)
 ("d m" compile)
 ("v u" projectile-run-async-shell-command-in-root)
 ("v w" sk/frame-jump)
 ("v c" ivy-push-view)
 ("v d" ivy-pop-view)
 ("y u" counsel-bookmark)
 ("v o x" set-frame-font))

;; completion mappings
(bind-key "C-r" 'counsel-company)

;; wgrep saving
(ryo-modal-key "#" 'wgrep-save-all-buffers)

;; mapping with global prefix
(ryo-modal-key "g"
			   '(("m" counsel-mark-ring)
				 ("B" sk/current-bib)
				 ("s" sk/counsel-rg-directory)
				 ("e" sk/counsel-rg-project-at-point)
				 ("O" counsel-package)
				 ("x" wgrep-save-all-buffers)
				 ("X" wgrep-abort-changes)
				 ("E" ivy-wgrep-change-to-wgrep-mode)
				 ("z" flyspell-correct-previous-word-generic)
				 ("*" counsel-colors-emacs)
				 ("#" counsel-colors-web)
				 ("[" xref-pop-marker-stack)
				 ("]" ggtags-find-reference)
				 ("(" ggtags-create-tags)
				 (")" ggtags-update-tags)
				 (";" projectile-find-file-dwim)
				 ("&" projectile-run-eshell)
				 ("G" counsel-git-log)))

;; mapping with leader
(ryo-modal-key "SPC"
			   '(("i" ivy-resume)
				 ("s" spotlight)
				 ("r" counsel-recentf)
				 ("/" swiper-all)
				 ("b" ivy-bibtex)
				 ("p" sk/counsel-rg-project)
				 ("d" counsel-projectile)
				 ("o" sk/counsel-org-folder)
				 ("\\" counsel-list-processes)
				 ("," counsel-descbinds)
				 ("." counsel-load-theme)
				 ("x" counsel-info-lookup-symbol)
				 ("TAB" projectile-find-other-file)
				 ("t" ggtags-find-other-symbol)
				 ("DEL" projectile-find-test-file)))

;; use locate or spotlight depending on the system
(cond
 ((eq system-type 'darwin)
  (ryo-modal-key "SPC s" 'spotlight))
 ((eq system-type 'gnu/linux)
  (ryo-modal-key "SPC s" 'counsel-locate)))

;; provide the configuration
(provide 'sk-ivy)
