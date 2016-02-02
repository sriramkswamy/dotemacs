;; Swiper
(sk/require-package 'swiper)
(require 'ivy)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-r") 'swiper)

;; Ivy
(setq ivy-display-style 'fancy
      ivy-height 15)
(setq ivy-re-builders-alist
      '((ivy-switch-buffer . ivy--regex-fuzzy)
        (ivy-recentf . ivy--regex-fuzzy)
        (ivy-completion-in-region . ivy--regex-fuzzy)
        (t . ivy--regex-plus)))
(setq completion-in-region-function 'ivy-completion-in-region)
(defun sk/diminish-ivy ()
  (interactive)
  (diminish 'ivy-mode ""))
(add-hook 'ivy-mode-hook 'sk/diminish-ivy)
(ivy-mode 1)

;; Ivy maps
(define-key ivy-minibuffer-map (kbd "C-t") 'ivy-toggle-fuzzy)
(define-key ivy-minibuffer-map (kbd "C-j") 'ivy-done)
(define-key ivy-minibuffer-map (kbd "C-i") 'ivy-alt-done)
(define-key ivy-minibuffer-map (kbd "C-m") 'ivy-alt-done)
(define-key ivy-minibuffer-map (kbd "C-S-m") 'ivy-immediate-done)

;; Counsel
(sk/require-package 'counsel)
(setq counsel-yank-pop-truncate t)
(setq ivy-re-builders-alist
      '((counsel-M-x . ivy--regex-fuzzy)
        (counsel-imenu . ivy--regex-fuzzy)
        (counsel-find-file . ivy--regex-fuzzy)
        (counsel-find-symbol . ivy--regex-fuzzy)
        (counsel-info-lookup-symbol . ivy--regex-fuzzy)
        (counsel-describe-variable . ivy--regex-fuzzy)
        (counsel-describe-function . ivy--regex-fuzzy)
        (t . ivy--regex-plus)))

;; ag and wgrep
(sk/require-package 'ag)
(sk/require-package 'wgrep-ag)

;; Hydra for wgrep
(defhydra sk/hydra-wgrep (:color red
                          :hint nil)
  "
 ^Refactor^                        | ^Menu^
 ^^^^^^^^^--------------------------------|---------------
 _w_grep  _f_inish    _r_emove    _k_ill | _H_ome   e_x_ecute
        _s_ave-all  _R_emove-all     |        _q_uit
  "
  ("w" wgrep-change-to-wgrep-mode :color blue)
  ("s" wgrep-save-all-buffers)
  ("f" wgrep-finish-edit)
  ("r" wgrep-remove-change)
  ("R" wgrep-remove-all-change)
  ("k" wgrep-abort-changes)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("x" counsel-M-x :color blue)
  ("q" nil :color blue))

;; Spotlight
(sk/require-package 'spotlight)

;; Visual regexp
(sk/require-package 'visual-regexp)
(sk/require-package 'visual-regexp-steroids)

;; Google
(sk/require-package 'google-this)

(provide 'sk-search)

;;; sk-search.el ends here
