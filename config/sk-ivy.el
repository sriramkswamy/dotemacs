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
  (setq ivy-re-builders-alist
	'((swiper . ivy--regex-plus)
	  (t . ivy--regex-fuzzy)))
  :general
  (general-define-key :keymaps 'ivy-minibuffer-map
		      "C-w" 'backward-kill-word
		      "C-m" 'ivy-alt-done
		      "C-j" 'ivy-immediate-done
		      "TAB" 'ivy-dispatching-done
		      "C-t" 'ivy-avy
		      "C-S-m" 'ivy-restrict-to-matches)
  (general-nmap :prefix sk--evil-global-leader
		"i" 'ivy-resume)
  (general-nmap "gt" 'ivy-push-view)
  (general-nmap "gT" 'ivy-pop-view)
  :config				; settings once the package is loaded
  (ivy-mode 1)
  ;; the hydra in ivy
  (use-package ivy-hydra
    :ensure t)
  ;; wrapper around ivy for many functions
  (use-package counsel
    :ensure t
    :diminish counsel-mode
    :general				; `general.el' maps
    (general-nmap "t" 'counsel-imenu)
    (general-imap "C-k" 'counsel-unicode-char)
    (general-nmap "g/" 'counsel-ag)
    :config
    (counsel-mode 1))
  ;; search the buffer or all buffer
  (use-package swiper
    :ensure t
    :general
    (general-nmap :prefix sk--evil-global-leader "/" 'swiper-all)
    (general-nmap "/" 'counsel-grep-or-swiper)
    (general-define-key "C-s" 'counsel-grep-or-swiper))
  ;; counsel wrapper around projectile
  (use-package counsel-projectile
    :ensure t
    :general
    (general-nmap :prefix sk--evil-global-leader
		  "p" '(counsel-projectile :which-key "switch projects")))
  ;; open any OSX app
  (use-package counsel-osx-app
    :ensure t
    :general
    (general-nmap "g!" 'counsel-osx-app))
  ;; search spotlight
  (use-package spotlight
    :ensure t
    :general
    (general-nmap :prefix sk--evil-global-leader "s" 'spotlight))
  ;; bibliography and citations
  (use-package ivy-bibtex
    :ensure t
    :general
    (general-nmap :prefix sk--evil-global-leader "b" 'ivy-bibtex)
    :init
    (setq bibtex-completion-bibliography '("~/Dropbox/PhD/articles/tensors/tensors.bib"))
    (setq bibtex-completion-library-path '("~/Dropbox/PhD/articles/tensors"))
    (setq bibtex-completion-notes-path "~/Dropbox/Phd/articles/articles.org")
    (setq bibtex-completion-pdf-symbol "⌘")
    (setq bibtex-completion-notes-symbol "✎")))

;; install projectile itself
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (setq projectile-file-exists-remote-cache-expire (* 10 60))
  (setq projectile-completion-system 'ivy)
  :general
  (general-nmap :prefix sk--evil-global-leader
		"d" '(projectile-find-file :which-key "project files")
		"TAB" '(projectile-find-other-file :which-key "project other file"))
  :config
  (projectile-global-mode))

;; provide the configuration
(provide 'sk-ivy)
