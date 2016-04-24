;;; sk-helm-config.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Huge helm config

;;; Code:

;; For google
(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

;; while grepping
(when (executable-find "ag-grep")
  (setq helm-grep-default-command "ag-grep -Hn --no-group --no-color %e %p %f"
        helm-grep-default-recurse-command "ag-grep -H --no-group --no-color %e %p %f"))

;; Window config
(setq helm-split-window-in-side-p           t  ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t  ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t  ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8  ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-autoresize-max-height            30 ; maximum height
      helm-autoresize-min-height            30 ; maximum height
      helm-ff-file-name-history-use-recentf t  ; filename
      helm-M-x-fuzzy-match                  t  ; Fuzzy in M-x
      helm-recentf-fuzzy-match              t  ; Fuzzy in recentf
      helm-buffers-fuzzy-matching           t  ; fuzzy for buffers
      helm-semantic-fuzzy-matching          t  ; semantic and imenu fuzzy matching
      helm-imenu-fuzzy-matching             t)

;; Use spotlight on a mac
(setq helm-locate-command
      (case system-type
        ('gnu/linux "locate -i -r %s")
        ('berkeley-unix "locate -i %s")
        ('windows-nt "es %s")
        ('darwin "mdfind -name %s %s")
        (t "locate %s")))

;; Make sure helm is always on the bottom
(add-to-list 'display-buffer-alist
             '("\\`\\*helm.*\\*\\'"
               (display-buffer-in-side-window)
               (inhibit-same-window . t)
               (window-height . 0.4)))

;; Make sure you type in the top of the window
(setq helm-echo-input-in-header-line t)
(defvar bottom-buffers nil
  "List of bottom buffers before helm session.
    Its element is a pair of `buffer-name' and `mode-line-format'.")
(defun bottom-buffers-init ()
  (setq-local mode-line-format (default-value 'mode-line-format))
  (setq bottom-buffers
        (cl-loop for w in (window-list)
                 when (window-at-side-p w 'bottom)
                 collect (with-current-buffer (window-buffer w)
                           (cons (buffer-name) mode-line-format)))))
(defun bottom-buffers-hide-mode-line ()
  (setq-default cursor-in-non-selected-windows nil)
  (mapc (lambda (elt)
          (with-current-buffer (car elt)
            (setq-local mode-line-format nil)))
        bottom-buffers))
(defun bottom-buffers-show-mode-line ()
  (setq-default cursor-in-non-selected-windows t)
  (when bottom-buffers
    (mapc (lambda (elt)
            (with-current-buffer (car elt)
              (setq-local mode-line-format (cdr elt))))
          bottom-buffers)
    (setq bottom-buffers nil)))
(defun helm-keyboard-quit-advice (orig-func &rest args)
  (bottom-buffers-show-mode-line)
  (apply orig-func args))
(add-hook 'helm-before-initialize-hook #'bottom-buffers-init)
(add-hook 'helm-after-initialize-hook #'bottom-buffers-hide-mode-line)
(add-hook 'helm-exit-minibuffer-hook #'bottom-buffers-show-mode-line)
(add-hook 'helm-cleanup-hook #'bottom-buffers-show-mode-line)
(advice-add 'helm-keyboard-quit :around #'helm-keyboard-quit-advice)

;; Add header line only if there is one
(setq helm-display-header-line nil)
(defvar helm-source-header-default-background (face-attribute 'helm-source-header :background))
(defvar helm-source-header-default-foreground (face-attribute 'helm-source-header :foreground))
(defvar helm-source-header-default-box (face-attribute 'helm-source-header :box))
(defun helm-toggle-header-line ()
  (if (> (length helm-sources) 1)
      (set-face-attribute 'helm-source-header
                          nil
                          :foreground helm-source-header-default-foreground
                          :background helm-source-header-default-background
                          :box helm-source-header-default-box
                          :height 1.0)
    (set-face-attribute 'helm-source-header
                        nil
                        :foreground (face-attribute 'helm-selection :background)
                        :background (face-attribute 'helm-selection :background)
                        :box nil
                        :height 0.1)))
(add-hook 'helm-before-initialize-hook 'helm-toggle-header-line)

;; Hide minibuffer when typing
(defun helm-hide-minibuffer-maybe ()
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                              `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))
(add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

;; Find files act like ido
(defun dwim-helm-find-files-up-one-level-maybe ()
  (interactive)
  (if (looking-back "/" 1)
      (call-interactively 'helm-find-files-up-one-level)
    (delete-backward-char 1)))
(define-key helm-read-file-map (kbd "<backspace>") 'dwim-helm-find-files-up-one-level-maybe)
(define-key helm-read-file-map (kbd "DEL") 'dwim-helm-find-files-up-one-level-maybe)
(define-key helm-find-files-map (kbd "<backspace>") 'dwim-helm-find-files-up-one-level-maybe)
(define-key helm-find-files-map (kbd "DEL") 'dwim-helm-find-files-up-one-level-maybe)
(defun dwim-helm-find-files-navigate-forward (orig-fun &rest args)
  "Adjust how helm-execute-persistent actions behaves, depending on context"
  (if (file-directory-p (helm-get-selection))
      (apply orig-fun args)
    (helm-maybe-exit-minibuffer)))
(define-key helm-map (kbd "<return>") 'helm-maybe-exit-minibuffer)
(define-key helm-map (kbd "RET") 'helm-maybe-exit-minibuffer)
(define-key helm-find-files-map (kbd "<return>") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "<return>") 'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "RET") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "RET") 'helm-execute-persistent-action)
(advice-add 'helm-execute-persistent-action :around #'dwim-helm-find-files-navigate-forward)

;; Remove the dots
(require 'cl-lib)
(with-eval-after-load 'helm-files
  (advice-add 'helm-ff-filter-candidate-one-by-one
              :before-while 'no-dots-display-file-p))
(defvar no-dots-whitelist nil
  "List of helm buffers in which to show dots.")
(defun no-dots-in-white-listed-helm-buffer-p ()
  (member helm-buffer no-dots-whitelist))
(defun no-dots-display-file-p (file)
  ;; in a whitelisted buffer display the file regardless of its name
  (or (no-dots-in-white-listed-helm-buffer-p)
      ;; not in a whitelisted buffer display all files
      ;; which does not end with /. /..
      (not (string-match "\\(?:/\\|\\`\\)\\.\\{1,2\\}\\'" file))))

;; Adaptive helm
(with-eval-after-load 'helm-adaptive
  (defcustom helm-adaptive-enabled-sources  '()
    "List of Helm Source names for which helm-adaptive will remember history."
    :type '(repeat string)
    :group 'helm-adapt)
  ;; Remember history for these sources add more sources here if you like
  (add-to-list 'helm-adaptive-enabled-sources "describe-function")
  (add-to-list 'helm-adaptive-enabled-sources "describe-variable")
  ;; Clobber helm's implementation
  (defun helm-adapt-use-adaptive-p (&optional source-name)
    "Return current source only if it use adaptive history, nil otherwise."
    (when helm-adaptive-mode
      (let* ((source (or source-name (helm-get-current-source)))
	     (adapt-source (when (listp source)
			     (or (assoc-default 'filtered-candidate-transformer
						(assoc (assoc-default 'type source)
						       helm-type-attributes))
				 (assoc-default 'candidate-transformer
						(assoc (assoc-default 'type source)
						       helm-type-attributes))
				 (assoc-default 'filtered-candidate-transformer source)
				 (assoc-default 'candidate-transformer source)))))
	(cond
	 ((member (cdr (assoc 'name source)) helm-adaptive-enabled-sources)
	  source)
	 ((listp adapt-source)
	  (and (member 'helm-adaptive-sort adapt-source) source))
	 ((eq adapt-source 'helm-adaptive-sort)         
	  source)))))
  (require 'dash)
  (setq helm-fuzzy-sort-fn
        (lambda (candidates source &optional use-real)
          (-> candidates
              (helm-flx-fuzzy-matching-sort source use-real)
              (helm-adaptive-sort source)
              ))
        helm-fuzzy-matching-highlight-fn #'helm-flx-fuzzy-highlight-match))
(helm-adaptive-mode 1)

;; Flx everywhere!
(defun my-helm-make-source (f &rest args)
  (nconc args '(:fuzzy-match t))
  (apply f args))
(advice-add 'helm-make-source :around 'my-helm-make-source)

;; Improve performace with smex
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))
(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))
(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; Helm swoop configuration
(setq helm-swoop-split-with-multiple-windows nil
      helm-swoop-split-direction 'split-window-vertically
      helm-swoop-split-window-function 'helm-default-display-buffer)
(define-key helm-swoop-map (kbd "C-w") 'backward-kill-word)

;; Bind stuff
(define-key helm-map (kbd "C-w") 'backward-kill-word)

;; Diminish
(defun sk/diminish-helm ()
  (interactive)
  (diminish 'helm-mode "λ"))
(add-hook 'helm-mode-hook 'sk/diminish-helm)
(add-hook 'after-init-hook 'sk/diminish-helm)

(provide 'sk-helm-config)
;;; sk-helm-config.el ends here
