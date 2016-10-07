;; Flex for fuzzy matching
(use-package flx-ido	; Flx matching for ido (includes flx)
  :ensure t)

;; Smex for a better M-x
(use-package smex	; much improved M-x
  :ensure t)

;; ivy for everything
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :demand t
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
  :general
  ;; similar to helm - because I got used to that kind of TAB
  (general-define-key :keymaps 'ivy-minibuffer-map
					  "C-w" 'backward-kill-word
					  "RET" 'ivy-alt-done
					  "C-j" 'ivy-immediate-done
					  "TAB" 'ivy-dispatching-done
					  "C-t" 'ivy-avy
					  "S-<return>" 'ivy-restrict-to-matches
					  "C-S-m" 'ivy-restrict-to-matches)
  ;; resume ivy
  (general-nvmap :prefix sk--evil-global-leader
				 "i" '(ivy-resume :which-key "resume ivy")
				 "=" '(ivy-push-view :which-key "add window config")
				 "-" '(ivy-pop-view :which-key "del window config"))
  ;; wgrep in ivy occur - invoked from swiper/counsel-grep-or-swiper/counsel-grep
  (general-nvmap :prefix sk--evil-local-leader
				 "E" '(ivy-wgrep-change-to-wgrep-mode :which-key "edit search results"))

  :config
  (ivy-mode 1)
  ;; the hydra in ivy
  (use-package ivy-hydra
    :ensure t)

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
    :general
    ;; imenu - maybe use imenu everywhere?
	(general-nmap "t" '(counsel-imenu :which-key "tags in file"))
    ;; man pages - completing read
    (general-nvmap "M" '(woman :which-key "man pages"))
    ;; search in directory
    (general-nvmap :prefix sk--evil-global-leader
		   "/" '(counsel-ag :which-key "search in dir"))
    ;; key bindings
    (general-nvmap :prefix sk--evil-global-leader
		   "," '(counsel-descbinds :which-key "search bindings"))
	;; color themes
	(general-nvmap :prefix sk--evil-global-leader
		   "." '(counsel-load-theme :which-key "search bindings"))
    ;; unicode char in evil-insert-state
    (general-imap "C-v" '(counsel-unicode-char :which-key "unicode char"))
    :config
    (counsel-mode 1))

  ;; search the buffer or all buffer
  (use-package swiper
    :ensure t
    :general
    ;; swiper all buffers
    (general-nvmap "g/" '(swiper-all :which-key "search in all buffers"))
    ;; replace normal evil search with swiper
    (general-nvmap "/" '(counsel-grep-or-swiper :which-key "search in buffer"))
    ;; swiper with word-at-point
    (general-nvmap "#" '(sk/swiper-at-point :which-key "search word in buffer"))
    ;; bind C-s too
    (general-define-key "C-s" '(counsel-grep-or-swiper :which-key "search"))
    :config
    (defun sk/swiper-at-point ()
      "use swiper to search for a word at point"
      (interactive)
      (swiper (thing-at-point 'word))))

  ;; project files
  (use-package counsel-projectile
    :ensure t
    :general
    ;; map to some handy wrapper functions based on projectile
    (general-nvmap :prefix sk--evil-global-leader
		   "d" '(counsel-projectile-find-file-or-buffer :which-key "project files")
		   "a" '(sk/counsel-ag-project-at-point :which-key "search symbol in proj")
		   "p" '(sk/counsel-ag-project :which-key "search in project"))
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
      (counsel-ag "" (projectile-project-root))))

  ;; correct spellings
  (use-package flyspell-correct-ivy
    :ensure t
    :general
	(general-imap "C-," #'(flyspell-correct-previous-word-generic :which-key "correct spelling mistake"))
    :config
    (require 'flyspell-ivy))

  ;; search using spotlight
  (use-package spotlight
    :ensure t
    :general
    (general-nvmap :prefix sk--evil-global-leader
		   "s" '(spotlight :which-key "search desktop")))

  ;; to narrow down org files - because helm has helm-org-rifle. This is the closest
  (use-package deft
    :ensure t
    :init
    (setq deft-extensions '("org" "txt"))
    (setq deft-directory "~/Dropbox/org")
    :general
    (general-nvmap :prefix sk--evil-global-leader
		   "of" '(deft :which-key "find in org files")))

  ;; bibliography and citations
  (use-package ivy-bibtex
	:ensure t
	:general
	(general-nvmap :prefix sk--evil-global-leader
				   "b" '(ivy-bibtex :which-key "bibliography")
				   "B" '(nil :which-key "switch bib")
				   "Bd" '(sk/default-bib :which-key "default")
				   "Bc" '(sk/current-bib :which-key "current dir"))
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
	  (message "Default bib files set"))))

;; Redefine ivy's hydra
(defhydra hydra-ivy (:hint nil
		     :color pink)
  "
^ ^ ^ ^ ^ ^ | ^Call^      ^ ^  | ^Cancel^ | ^Options^ | Action _w_/_s_/_a_: %-14s(ivy-action-name)
^-^-^-^-^-^-+-^-^---------^-^--+-^-^------+-^-^-------+-^^^^^^^^^^^^^^^^^^^^^^^^^^^^^---------------------------
^ ^ _k_ ^ ^ | _f_ollow _o_ccur | _i_nsert | _c_: calling %-5s(if ivy-calling \"on\" \"off\") _C_ase-fold: %-10`ivy-case-fold-search
_h_ ^+^ _l_ | _d_one   _p_roj  | _q_uit   | _m_: matcher %-5s(ivy--matcher-desc)^^^^^^^^^^^^ _t_runcate: %-11`truncate-lines
^ ^ _j_ ^ ^ | _g_o     _D_efun | _n_arrow | _<_/_>_: shrink/grow^^^^^^^^^^^^^^^^^^^^^^^^^^^^ _C-f_/_C-b_: scroll up or down
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
  ("C-f" ivy-scroll-up-command)
  ("C-b" ivy-scroll-down-command)
  ("w" ivy-prev-action)
  ("s" ivy-next-action)
  ("a" ivy-read-action)
  ("TAB" ivy-dispatching-done)
  ("t" (setq truncate-lines (not truncate-lines)))
  ("C" ivy-toggle-case-fold)
  ("D" (ivy-exit-with-action
	(lambda (_) (find-function 'hydra-ivy/body)))
       :exit t))

;; hydra for ivy occur - because evil and ivy-occur-mode mess up often
(defhydra hydra-ivy-occur (:color red :hint nil)
  "
 ^Occur^                                     ^Compile^
^^^^^^^^^^----------------------------------------------------------------------------------------------
 _j_: next    _a_: action  _<_: end of buffer    _n_: next file      _h_: prev error  _q_: quit
 _k_: prev    _s_: press   _>_: start of buffer  _p_: prev file      _l_: next error
 _f_: follow  _o_: occur                       _c_: compile error  _d_: display error
"
  ("j" ivy-occur-next-line)
  ("k" ivy-occur-previous-line)
  ("a" ivy-occur-read-action)
  ("s" ivy-occur-press)
  ("f" ivy-occur-toggle-calling)
  ("o" ivy-occur-dispatch :color blue)
  ("<" beginning-of-buffer)
  (">" end-of-buffer)
  ("n" compilation-next-file)
  ("p" compilation-previous-file)
  ("h" previous-error-no-select)
  ("l" next-error-no-select)
  ("c" compilation-next-error)
  ("d" compilation-display-error)
  ("q" nil :color blue))
(general-nvmap :prefix sk--evil-local-leader
	       "y" '(hydra-ivy-occur/body :which-key "ivy occur"))

;; provide the configuration
(provide 'sk-ivy)
