;; Flx for fuzzy matching
(use-package flx-ido	; Flx matching for ido (includes flx)
  :ensure t)

;; Smex for a better M-x
(use-package smex	; much improved M-x
  :ensure t)

;; ivy for everything
(use-package ivy			; narrowing and selecting framework
  :ensure t				; make sure this package is installed
  :diminish ivy-mode			; don't clutter the mode-line
  :demand t
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-virtual-abbreviate 'full)
  (setq ivy-re-builders-alist
	'((counsel-M-x . ivy--regex-fuzzy)
	  (t . ivy--regex-plus)))
  :general
  (general-define-key :keymaps 'ivy-minibuffer-map
		      "C-w" 'backward-kill-word
		      "C-m" 'ivy-alt-done
		      "C-j" 'ivy-immediate-done
		      "TAB" 'ivy-dispatching-done
		      "C-t" 'ivy-avy
		      "S-<return>" 'ivy-restrict-to-matches
		      "C-S-m" 'ivy-restrict-to-matches)
  (general-evil-define-key '(normal visual) ivy-occur-mode-map
    "j" (general-simulate-keys "j" t "next line")
    "k" (general-simulate-keys "k" t "prev line")
    "h" (general-simulate-keys "h" t "prev char")
    "l" (general-simulate-keys "l" t "next char")
    "a" (general-simulate-keys "a" t "read action")
    )
  (general-nvmap :prefix sk--evil-global-leader
		 "i" 'ivy-resume)
  (general-nvmap :prefix sk--evil-local-leader
		 "q" '(wgrep-change-to-wgrep-mode :which-key "refactor")
		 "=" '(wgrep-save-all-buffers :which-key "save refactored files"))
  (general-nvmap "gt" 'ivy-push-view)
  (general-nvmap "gT" 'ivy-pop-view)
  :config				; settings once the package is loaded
  (ivy-mode 1)

  ;; the hydra in ivy
  (use-package ivy-hydra
    :ensure t)

  ;; wgrep for refactoring
  (use-package wgrep
    :ensure t)

  ;; wrapper around ivy for many functions
  (use-package counsel
    :ensure t
    :diminish counsel-mode
    :general				; `general.el' maps
    (general-nvmap "t" 'counsel-imenu)
    (general-nvmap "M" 'woman)
    (general-nvmap :prefix sk--evil-global-leader "/" 'counsel-ag)
    (general-nvmap :prefix sk--evil-global-leader "?" 'counsel-descbinds)
    (general-imap "C-v" 'counsel-unicode-char)
    :config
    (counsel-mode 1))

  ;; search the buffer or all buffer
  (use-package swiper
    :ensure t
    :general
    (general-nvmap "g/" 'swiper-all)
    (general-nvmap "/" 'counsel-grep-or-swiper)
    (general-nvmap "#" 'sk/swiper-at-point)
    (general-define-key "C-s" 'counsel-grep-or-swiper)
    :config
    (defun sk/swiper-at-point ()
      "use swiper to search for a word at point"
      (interactive)
      (swiper (thing-at-point 'word))))

  ;; project files
  (use-package counsel-projectile
    :ensure t
    :general
    (general-nmap :prefix sk--evil-global-leader
		  "d" '(counsel-projectile-find-file-or-buffer :which-key "find in project")
		  "TAB" '(projectile-find-other-file :which-key "project other file")
		  "p" '(sk/counsel-ag-project-at-point :which-key "search word in project")
		  "P" '(sk/counsel-ag-project :which-key "search in project"))
    :init
    (setq projectile-completion-system 'ivy)
    :config
    (defun sk/counsel-ag-project-at-point ()
      "use counsel ag to search for the word at point in the project"
      (interactive)
      (counsel-ag (thing-at-point 'word) (projectile-project-root)))
    (defun sk/counsel-ag-project ()
      "use counsel ag to search the project"
      (interactive)
      (counsel-ag "" (projectile-project-root))))

  ;; correct spellings
  (use-package flyspell-correct-ivy
    :ensure t
    :general
    (general-imap "C-z" #'flyspell-correct-previous-word-generic)
    :config
    (require 'flyspell-ivy))

  ;; search spotlight
  (use-package spotlight
    :ensure t
    :general
    (general-nmap :prefix sk--evil-global-leader "s" 'spotlight))

  ;; to narrow down org files
  (use-package deft
    :ensure t
    :init
    (setq deft-extensions '("org" "txt"))
    (setq deft-directory "~/Dropbox/org")
    :general
    (general-nvmap :prefix sk--evil-global-leader
		   "o" '(deft :which-key "narrow down org")))

  ;; bibliography and citations
  (use-package ivy-bibtex
    :ensure t
    :general
    (general-nmap :prefix sk--evil-global-leader "b" 'ivy-bibtex)
    :init
    (setq bibtex-completion-bibliography
	  '("~/Dropbox/PhD/articles/tensors/tensors.bib"
	    "~/Dropbox/PhD/articles/machinelearning/machinelearning.bib"
	    "~/Dropbox/PhD/articles/association/association.bib"
	    "~/Dropbox/PhD/articles/lorenz/lorenz.bib"
	    "~/Dropbox/PhD/articles/multiphysics/multiphysics.bib"))
    (setq bibtex-completion-library-path
	  '("~/Dropbox/PhD/articles/tensors"
	    "~/Dropbox/PhD/articles/machinelearning"
	    "~/Dropbox/PhD/articles/association"
	    "~/Dropbox/PhD/articles/lorenz"
	    "~/Dropbox/PhD/articles/multiphysics"))
    (setq bibtex-completion-notes-path "~/Dropbox/org/articles.org")
    (setq bibtex-completion-pdf-symbol "⌘")
    (setq bibtex-completion-notes-symbol "✎")))

;; Redefine ivy's hydra
(defun ivy--matcher-desc ()
  (if (eq ivy--regex-function
	  'ivy--regex-fuzzy)
      "fuzzy"
    "ivy"))

(defhydra hydra-ivy (:hint nil
		     :color pink)
  "
^ ^ ^ ^ ^ ^ | ^Call^      ^ ^  | ^Cancel^ | ^Options^ | Action _w_/_s_/_a_: %-14s(ivy-action-name)
^-^-^-^-^-^-+-^-^---------^-^--+-^-^------+-^-^-------+-^^^^^^^^^^^^^^^^^^^^^^^^^^^^^---------------------------
^ ^ _k_ ^ ^ | _f_ollow _o_ccur | _i_nsert | _c_: calling %-5s(if ivy-calling \"on\" \"off\") _C_ase-fold: %-10`ivy-case-fold-search
_h_ ^+^ _l_ | _d_one      ^ ^  | _q_uit   | _m_: matcher %-5s(ivy--matcher-desc)^^^^^^^^^^^^ _t_runcate: %-11`truncate-lines
^ ^ _j_ ^ ^ | _g_o        ^ ^  | _p_roj   | _<_/_>_: shrink/grow^^^^^^^^^^^^^^^^^^^^^^^^^^^^ _D_efinition of this menu
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
  ("C-m" ivy-done :exit t)
  ("c" ivy-toggle-calling)
  ("m" ivy-toggle-fuzzy)
  (">" ivy-minibuffer-grow)
  ("<" ivy-minibuffer-shrink)
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
