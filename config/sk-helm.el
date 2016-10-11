;; Flx for fuzzy matching
(use-package flx-ido	; Flx matching for ido (includes flx)
  :ensure t)

;; Smex for a better M-x
(use-package smex	; much improved M-x
  :ensure t)

;; Add support for flx
(use-package helm-flx
  :ensure t
  :commands (helm-flx-mode)
  :config
  (helm-flx-mode 1))

;; Make helm fuzzier
(use-package helm-fuzzier
  :ensure t
  :commands (helm-fuzzier-mode)
  :config
  (helm-fuzzier-mode 1))

;;; snippets taken from https://github.com/compunaut/helm-ido-like-guide
;;; define some functions to be used in helm later
;; initialize
(defun helm-ido-like-activate-helm-modes ()
  (helm-mode 1)
  (helm-flx-mode 1)
  (helm-fuzzier-mode 1))

;; hide the minibuffer when helm is active
(defun helm-hide-minibuffer-maybe ()
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
			      `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))

;; always at the bottom buffer
(defun helm-ido-like-load-ido-like-bottom-buffer ()
  ;; popup helm-buffer at the bottom
  (setq helm-split-window-in-side-p t)
  (add-to-list 'display-buffer-alist
               '("\\`\\*helm.*\\*\\'"
                 (display-buffer-in-side-window)
                 (window-height . 0.2)))
  (add-to-list 'display-buffer-alist
               '("\\`\\*helm help\\*\\'"
                 (display-buffer-pop-up-window)))
  ;; dont display the header line
  (setq helm-display-header-line nil)
  ;; input in header line
  (setq helm-echo-input-in-header-line t))

;; hide modelines
(defvar helm-ido-like-bottom-buffers nil
  "List of bottom buffers before helm session started.
Its element is a pair of `buffer-name' and `mode-line-format'.")


(defun helm-ido-like-bottom-buffers-init ()
  (setq-local mode-line-format (default-value 'mode-line-format))
  (setq helm-ido-like-bottom-buffers
        (cl-loop for w in (window-list)
                 when (window-at-side-p w 'bottom)
                 collect (with-current-buffer (window-buffer w)
                           (cons (buffer-name) mode-line-format)))))


(defun helm-ido-like-bottom-buffers-hide-mode-line ()
  (mapc (lambda (elt)
          (with-current-buffer (car elt)
            (setq-local mode-line-format nil)))
        helm-ido-like-bottom-buffers))


(defun helm-ido-like-bottom-buffers-show-mode-line ()
  (when helm-ido-like-bottom-buffers
    (mapc (lambda (elt)
            (with-current-buffer (car elt)
              (setq-local mode-line-format (cdr elt))))
          helm-ido-like-bottom-buffers)
    (setq helm-ido-like-bottom-buffers nil)))


(defun helm-ido-like-helm-keyboard-quit-advice (orig-func &rest args)
  (helm-ido-like-bottom-buffers-show-mode-line)
  (apply orig-func args))

(defun helm-ido-like-hide-modelines ()
  ;; hide The Modelines while Helm is active
  (add-hook 'helm-before-initialize-hook #'helm-ido-like-bottom-buffers-init)
  (add-hook 'helm-after-initialize-hook #'helm-ido-like-bottom-buffers-hide-mode-line)
  (add-hook 'helm-exit-minibuffer-hook #'helm-ido-like-bottom-buffers-show-mode-line)
  (add-hook 'helm-cleanup-hook #'helm-ido-like-bottom-buffers-show-mode-line)
  (advice-add 'helm-keyboard-quit :around #'helm-ido-like-helm-keyboard-quit-advice))

;; hide the header line if just one source
(defvar helm-ido-like-source-header-default-background nil)
(defvar helm-ido-like-source-header-default-foreground nil)
(defvar helm-ido-like-source-header-default-box nil)

(defun helm-ido-like-toggle-header-line ()
  ;; Only Show Source Headers If More Than One
  (if (> (length helm-sources) 1)
      (set-face-attribute 'helm-source-header
                          nil
                          :foreground helm-ido-like-source-header-default-foreground
                          :background helm-ido-like-source-header-default-background
                          :box helm-ido-like-source-header-default-box
                          :height 1.0)
    (set-face-attribute 'helm-source-header
                        nil
                        :foreground (face-attribute 'helm-selection :background)
                        :background (face-attribute 'helm-selection :background)
                        :box nil
                        :height 0.1)))

(defun helm-ido-like-header-lines-maybe ()
  (setq helm-ido-like-source-header-default-background (face-attribute 'helm-source-header :background))
  (setq helm-ido-like-source-header-default-foreground (face-attribute 'helm-source-header :foreground))
  (setq helm-ido-like-source-header-default-box (face-attribute 'helm-source-header :box))
  (add-hook 'helm-before-initialize-hook 'helm-ido-like-toggle-header-line))

;; proper find file behavior
(defun helm-ido-like-find-files-up-one-level-maybe ()
  (interactive)
  (if (looking-back "/" 1)
      (call-interactively 'helm-find-files-up-one-level)
    (delete-char -1)))


(defun helm-ido-like-find-files-navigate-forward (orig-fun &rest args)
  "Adjust how helm-execute-persistent actions behaves, depending on context."
  (let ((sel (helm-get-selection)))
    (if (file-directory-p sel)
        ;; the current dir needs to work to
        ;; be able to select directories if needed
        (cond ((and (stringp sel)
                    (string-match "\\.\\'" (helm-get-selection)))
               (helm-maybe-exit-minibuffer))
              (t
               (apply orig-fun args)))
      (helm-maybe-exit-minibuffer))))

(defun helm-ido-like-load-file-nav ()
  (advice-add 'helm-execute-persistent-action :around #'helm-ido-like-find-files-navigate-forward)
    ;; <return> is not bound in helm-map by default
  (define-key helm-map (kbd "S-<return>") 'helm-maybe-exit-minibuffer)
  (define-key helm-map (kbd "S-RET") 'helm-maybe-exit-minibuffer)
  (define-key helm-map (kbd "C-S-m") 'helm-maybe-exit-minibuffer)
  (define-key helm-map (kbd "C-w") 'backward-kill-word)
  (with-eval-after-load 'helm-files
    (define-key helm-read-file-map (kbd "<backspace>") 'helm-ido-like-find-files-up-one-level-maybe)
    (define-key helm-read-file-map (kbd "DEL") 'helm-ido-like-find-files-up-one-level-maybe)
    (define-key helm-find-files-map (kbd "<backspace>") 'helm-ido-like-find-files-up-one-level-maybe)
    (define-key helm-find-files-map (kbd "DEL") 'helm-ido-like-find-files-up-one-level-maybe)

    (define-key helm-read-file-map (kbd "C-w") 'backward-kill-word)
    (define-key helm-find-files-map (kbd "C-w") 'backward-kill-word)

    (define-key helm-find-files-map (kbd "<return>") 'helm-execute-persistent-action)
    (define-key helm-read-file-map (kbd "<return>") 'helm-execute-persistent-action)
    (define-key helm-find-files-map (kbd "RET") 'helm-execute-persistent-action)
    (define-key helm-read-file-map (kbd "RET") 'helm-execute-persistent-action)))

;; improve flx speed
(defvar helm-ido-like-user-gc-setting nil)

(defun helm-ido-like-higher-gc ()
  (setq helm-ido-like-user-gc-setting gc-cons-threshold)
  (setq gc-cons-threshold most-positive-fixnum))


(defun helm-ido-like-lower-gc ()
  (setq gc-cons-threshold helm-ido-like-user-gc-setting))

(defun helm-ido-like-helm-make-source (f &rest args)
  (let ((source-type (cadr args)))
    (unless (or (memq source-type '(helm-source-async helm-source-ffiles))
                (eq (plist-get args :filtered-candidate-transformer)
                    'helm-ff-sort-candidates)
                (eq (plist-get args :persistent-action)
                    'helm-find-files-persistent-action))
      (nconc args '(:fuzzy-match t))))
  (apply f args))

(defun helm-ido-like-load-fuzzy-enhancements ()
  (add-hook 'minibuffer-setup-hook #'helm-ido-like-higher-gc)
  (add-hook 'minibuffer-exit-hook #'helm-ido-like-lower-gc)
  (advice-add 'helm-make-source :around 'helm-ido-like-helm-make-source))

;; improve helm-fuzzier integration
(defun helm-ido-like-fuzzier-deactivate (&rest _)
  (helm-fuzzier-mode -1))

(defun helm-ido-like-fuzzier-activate (&rest _)
  (unless helm-fuzzier-mode
    (helm-fuzzier-mode 1)))

(defun helm-ido-like-fix-fuzzy-files ()
  (add-hook 'helm-find-files-before-init-hook #'helm-ido-like-fuzzier-deactivate)
  (advice-add 'helm--generic-read-file-name :before #'helm-ido-like-fuzzier-deactivate)
  (add-hook 'helm-exit-minibuffer-hook #'helm-ido-like-fuzzier-activate)
  (add-hook 'helm-cleanup-hook #'helm-ido-like-fuzzier-activate)
  (advice-add 'helm-keyboard-quit :before #'helm-ido-like-fuzzier-activate))

;;;###autoload
(defun helm-ido-like ()
  "Configure and activate `helm', `helm-fuzzier' and `helm-flx'."
  (interactive)
  (helm-ido-like-activate-helm-modes)
  ;; (helm-ido-like-load-ido-like-bottom-buffer)
  (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)
  (helm-ido-like-hide-modelines)
  (helm-ido-like-header-lines-maybe)
  (helm-ido-like-load-file-nav)
  (helm-ido-like-load-fuzzy-enhancements)
  (helm-ido-like-fix-fuzzy-files))

;; Search through my org files
(defun sk/helm-org-rifle ()
  "Searches throughout the directory where my org files are kept"
  (interactive)
  (helm-org-rifle-directories '("~/Dropbox/org")))

;; Search using ag
(defun sk/helm-do-ag-project-root ()
  "Searches the project with ag"
  (interactive)
  (setq helm-ag-insert-at-point nil)
  (helm-do-ag-project-root))
(defun sk/helm-do-ag ()
  "Searches the specified directory with ag"
  (interactive)
  (setq helm-ag-insert-at-point nil)
  (helm-do-ag))
(defun sk/helm-do-ag-project-root-at-point ()
  "Searches the project for the symbol at point with ag"
  (interactive)
  (setq helm-ag-insert-at-point 'symbol)
  (helm-do-ag-project-root))

;; helm for narrowing
(use-package helm
  :ensure t
  :defer 1
  :diminish helm-mode
  :commands (helm-mode
			 helm-ido-like
			 persp-mode
			 describe-function
			 describe-variable)
  :bind* (("C-c '" . helm-man-woman)
		  ("C-c r" . helm-resume)
		  ("C-c q" . helm-quickrun)
		  ("C-c f" . helm-for-files)
		  ("C-c t" . helm-semantic-or-imenu)
		  ("C-c #" . helm-colors)
		  ("C-c `" . helm-all-mark-rings)
		  ("C-c C-\\" . helm-top))
  :bind (("M-x"     . helm-M-x)
		 ("M-y"     . helm-show-kill-ring)
		 ("C-x C-f" . helm-find-files)
		 ("C-h a"   . helm-apropos)
		 ("C-x C-b"   . helm-mini)
		 ("C-x 8"   . helm-ucs))
  :bind (:map helm-map
			  ("C-o" . hydra-helm/body)
			  ("<return>"   . helm-maybe-exit-minibuffer)
			  ("RET"        . helm-maybe-exit-minibuffer)
			  ("C-w"        . backward-kill-word)
			  ("<tab>"      . helm-select-action)
			  ("C-i"        . helm-select-action))
  :bind (:map helm-find-files-map
			  ("C-o" . hydra-helm/body)
			  ("<return>"    . helm-execute-persistent-action)
			  ("RET"         . helm-execute-persistent-action)
			  ("C-w"         . backward-kill-word)
			  ("<tab>"       . helm-select-action)
			  ("C-i"         . helm-select-action))
  :bind (:map helm-read-file-map
			  ("C-o" . hydra-helm/body)
			  ("<return>"    . helm-execute-persistent-action)
			  ("RET"         . helm-execute-persistent-action)
			  ("C-w"         . backward-kill-word)
			  ("<tab>"       . helm-select-action)
			  ("C-i"         . helm-select-action))

  :init
  ;; use silver searcher when available
  (when (executable-find "ag-grep")
    (setq helm-grep-default-command "ag-grep -Hn --no-group --no-color %e %p %f"
          helm-grep-default-recurse-command "ag-grep -H --no-group --no-color %e %p %f"))
  ;; Fuzzy matching for everything
  (setq helm-M-x-fuzzy-match t
        helm-recentf-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-locate-fuzzy-match nil
        helm-mode-fuzzy-match t)
  ;; Work with Spotlight on OS X instead of the regular locate
  (setq helm-locate-command "mdfind -name -onlyin ~ %s %s")
  ;; Make sure helm always pops up in bottom
  (setq helm-split-window-in-side-p t)
  ;; provide input in the header line and hide the mode lines above
  (setq helm-echo-input-in-header-line t)
  ;; remove header lines if only a single source
  (setq helm-display-header-line nil)
  ;; window height
  (setq helm-autoresize-max-height 20
        helm-autoresize-min-height 20)

  :config
  ;; set height and stuff
  (helm-autoresize-mode 1)

  ;; require basic config
  (helm-mode 1)
  (helm-ido-like))

;; better smex integration
(use-package helm-smex
  :ensure t
  :bind (("M-x" . helm-smex)
		 ("M-X" . helm-smex-major-mode-commands))
  :config
  (helm-mode 1)
  (helm-ido-like))

;; Add helm-bibtex
(use-package helm-bibtex
  :ensure t
  :bind* (("C-c n" . helm-bibtex)
		  ("C-c , d" . sk/default-bib)
		  ("C-c , c" . sk/current-bib))
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
  (setq helm-bibtex-full-frame nil)
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
  (helm-mode 1)
  (helm-ido-like))

;; to search in projects - the silver searcher
(use-package helm-ag
  :ensure t
  :bind* (("M-s p" . sk/helm-do-ag-project-root)
		  ("M-s C-p" . sk/helm-do-ag-project-root)
		  ("M-s j" . sk/helm-do-ag-project-root-at-point)
		  ("M-s C-j" . sk/helm-do-ag-project-root-at-point)
		  ("M-s c" . sk/helm-do-ag)
		  ("M-s C-c" . sk/helm-do-ag))
  :config
  (helm-mode 1)
  (helm-ido-like))

;; to search in files
(use-package helm-swoop
  :ensure t
  :bind* (("C-s" . helm-swoop-without-pre-input)
		  ("M-s s" . helm-multi-swoop-all)
		  ("M-s C-s" . helm-multi-swoop-all)
		  ("M-s r" . helm-swoop)
		  ("M-s C-r" . helm-swoop)
		  ("C-c X" . helm-multi-swoop--edit-cancel))
  :init
  (setq helm-swoop-split-with-multiple-windows nil
		helm-swoop-split-direction 'split-window-vertically
		helm-swoop-split-window-function 'helm-default-display-buffer)
  :config
  (helm-mode 1)
  (helm-ido-like))

;; to help with projectile
(use-package helm-projectile
  :ensure t
  :bind* (("C-c p" . helm-projectile))
  :init
  (setq projectile-completion-system 'helm)
  :config
  (helm-mode 1)
  (helm-ido-like))

;; to describe bindings
(use-package helm-descbinds
  :ensure t
  :bind (("C-h b" . helm-descbinds))
  :config
  (helm-mode 1)
  (helm-ido-like))

;; correct spellings
(use-package flyspell-correct-helm
  :ensure t
  :bind* (("C-c /" . flyspell-correct-previous-word-generic))
  :config
  (require 'flyspell-helm)
  (helm-mode 1)
  (helm-ido-like))

;; Select snippets with helm
(use-package helm-c-yasnippet
  :ensure t
  :bind* (("M-u" . helm-yas-complete))
  :config
  (helm-mode 1)
  (helm-ido-like))

;; for awesome org navigation
(use-package helm-org-rifle
  :ensure t
  :bind* (("C-c d" . sk/helm-org-rifle))
  :config
  (helm-mode 1)
  (helm-ido-like))

;; for theme selection
(use-package helm-themes
  :ensure t
  :bind* (("C-c ." . helm-themes))
  :config
  (helm-mode 1)
  (helm-ido-like))

;; helm hydra
(defhydra hydra-helm (:hint nil :color pink)
        "
										 ╭ ───────┐
   Navigation   Other  Sources     Mark             Do             Help          │  Helm  │
  ╭──────────────────────────────────────────────────────────────────────────────┴────────╯
	^_k_^         _K_       _p_   [_v_] mark         [_y_] yank         [_H_] helm help    [_q_] quit
        ^^↑^^         ^↑^       ^↑^   [_V_] toggle all   [_d_] delete       [_s_] source help
    _h_ ←   → _l_     _c_       ^ ^   [_u_] unmark all   [_f_] follow: %(helm-attr 'follow)
        ^^↓^^         ^↓^       ^↓^    ^ ^               [_w_] toggle windows
	^_j_^         _J_       _n_    ^ ^               [_e_] swoop edit
  --------------------------------------------------------------------------------
        "
        ("<tab>" helm-select-action)
        ("<escape>" nil "quit")
        ("<return>" helm-execute-persistent-action)
        ("\\" (insert "\\") "\\" :color blue)
        ("q" helm-keyboard-quit :exit t)
        ("h" helm-beginning-of-buffer)
        ("j" helm-next-line)
        ("k" helm-previous-line)
        ("l" helm-end-of-buffer)
        ("K" helm-scroll-other-window-down)
        ("J" helm-scroll-other-window)
        ("n" helm-next-source)
        ("p" helm-previous-source)
        ("c" helm-recenter-top-bottom-other-window)
        ("v" helm-toggle-visible-mark)
        ("V" helm-toggle-all-marks)
        ("u" helm-unmark-all)
        ("H" helm-help)
        ("s" helm-buffer-help)
        ("d" helm-persistent-delete-marked)
        ("y" helm-yank-selection)
	("w" helm-toggle-resplit-and-swap-windows)
	("e" helm-swoop-edit)
        ("f" helm-follow-mode))

;; provide this behemoth
(provide 'sk-helm)
