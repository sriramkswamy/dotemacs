;;; sk-navigation.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; For all kinds of navigation

;;; Code:

;; Don't lose the cursor
(sk/require-package 'beacon)
(add-hook 'after-init-hook 'beacon-mode)

;; Undo tree
(sk/require-package 'undo-tree)
(add-hook 'after-init-hook 'undo-tree-mode)

;; Avy for on-screen motion
(sk/require-package 'avy)
(setq avy-keys-alist
      `((avy-goto-char-2 . (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))
        (avy-goto-char-in-line . (?j ?k ?l ?f ?s ?d))
        (avy-goto-line . (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))))
(setq avy-style 'pre)

;; Helm
(sk/require-package 'helm)
(require 'helm)
(require 'helm-config)
(helm-autoresize-mode 1)
(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))
(when (executable-find "ag-grep")
  (setq helm-grep-default-command "ag-grep -Hn --no-group --no-color %e %p %f"
        helm-grep-default-recurse-command "ag-grep -H --no-group --no-color %e %p %f"))
(setq helm-split-window-in-side-p           t  ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t  ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t  ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8  ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-autoresize-max-height            30 ; maximum height
      helm-autoresize-min-height            30 ; maximum height
      helm-ff-file-name-history-use-recentf t
      ; Fuzzy matching
      helm-M-x-fuzzy-match                  t
      helm-recentf-fuzzy-match              t
      helm-buffers-fuzzy-matching           t
      helm-semantic-fuzzy-matching          t
      helm-imenu-fuzzy-matching             t)
(setq helm-locate-command
      (case system-type
        ('gnu/linux "locate -i -r %s")
        ('berkeley-unix "locate -i %s")
        ('windows-nt "es %s")
        ('darwin "mdfind -name %s %s")
        (t "locate %s")))
(helm-mode 1)
;; Advice helm to user <return> like ido
(defun sk/helm-find-files-navigate-forward (orig-fun &rest args)
  (if (file-directory-p (helm-get-selection))
      (apply orig-fun args)
    (helm-maybe-exit-minibuffer)))
(advice-add 'helm-execute-persistent-action :around #'sk/helm-find-files-navigate-forward)
;; Advice helm to user <bs> like ido
(defun sk/helm-find-files-navigate-back (orig-fun &rest args)
  (if (= (length helm-pattern) (length (helm-find-files-initial-input)))
      (helm-find-files-up-one-level 1)
    (apply orig-fun args)))
(advice-add 'helm-ff-delete-char-backward :around #'sk/helm-find-files-navigate-back)

;; Flx matching
(sk/require-package 'flx)
(sk/require-package 'helm-flx)
(helm-flx-mode +1)

;; Helm ag
(sk/require-package 'helm-ag)

;; Helm descbinds
(sk/require-package 'helm-descbinds)

;; Helm theme selection
(sk/require-package 'helm-themes)

;; Swoop
(sk/require-package 'helm-swoop)

;; Ag and Wgrep
(sk/require-package 'ag)
(sk/require-package 'wgrep-ag)

;; Projectile
(sk/require-package 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-file-exists-remote-cache-expire (* 10 60))
(setq projectile-require-project-root nil)
(setq projectile-completion-system 'helm)
;; Helm projectile
(sk/require-package 'helm-projectile)
(setq projectile-switch-project-action 'helm-projectile)

;; Helm bibtex
(sk/require-package 'helm-bibtex)
(setq helm-bibtex-bibliography "~/Dropbox/references/references.bib")
(setq helm-bibtex-library-path "~/Dropbox/references/bibtex-pdfs")
(setq helm-bibtex-notes-path "~/Dropbox/references/helm-bibtex-notes")

;; Improve dired
(sk/require-package 'dired+)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;; Neotree
(sk/require-package 'neotree)
(setq neo-smart-open t)

;; GTags
(sk/require-package 'ggtags)
(add-hook 'prog-mode-hook 'ggtags-mode)
(setq-local imenu-create-index-function #'ggtags-build-imenu-index)

;; Back button mode
(sk/require-package 'back-button)
(add-hook 'after-init-hook 'back-button-mode)

;; Perspective mode
(sk/require-package 'perspective)
(add-hook 'after-init-hook 'persp-mode)

;; aux requirements
(require 'sk-navigation-bindings)
(require 'sk-navigation-functions)
(require 'sk-navigation-hydra)
(require 'sk-navigation-diminish)

(provide 'sk-navigation)
;;; sk-navigation.el ends here
