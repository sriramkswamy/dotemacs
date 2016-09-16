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
	'((counsel-M-x . ivy--regex-fuzzy)
	  (t . ivy--regex-plus)))
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
    (general-imap "C-v" 'counsel-unicode-char)
    (general-nmap :prefix sk--evil-global-leader "/" 'counsel-ag)
    :config
    (counsel-mode 1))
  ;; search the buffer or all buffer
  (use-package swiper
    :ensure t
    :general
    (general-nmap "g/" 'swiper-all)
    (general-nmap "/" 'counsel-grep-or-swiper)
    (general-define-key "C-s" 'counsel-grep-or-swiper))
  ;; project files
  (use-package find-file-in-project
    :ensure t
    :general
    (general-nmap :prefix sk--evil-global-leader
		  "d" '(ffip :which-key "find in project")))
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
^ ^ _j_ ^ ^ | _g_o        ^ ^  | ^ ^      | _<_/_>_: shrink/grow^^^^^^^^^^^^^^^^^^^^^^^^^^^^ _D_efinition of this menu
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
