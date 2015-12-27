;; Swiper, Ivy and Counsel
(sk/require-package 'swiper)
(sk/require-package 'counsel)
(setq ivy-display-style 'fancy
      ivy-height 15
      counsel-yank-pop-truncate t)
;; Fuzzy for M-x
(setq ivy-re-builders-alist
      '((counsel-M-x . ivy--regex-fuzzy)
        (t . ivy--regex-plus)))
(defun sk/diminish-ivy ()
  (interactive)
  (diminish 'ivy-mode ""))
(add-hook 'ivy-mode-hook 'sk/diminish-ivy)
(ivy-mode 1)

;; Swoop
(sk/require-package 'swoop)

;; ag and wgrep
(sk/require-package 'ag)
(sk/require-package 'wgrep-ag)

;; Hydra for wgrep
(defhydra sk/hydra-wgrep (:color blue
                          :hint nil)
  "
 ^Refactor^                        | ^Menu^
 ^^^^^^^^^--------------------------------|---------------
 _w_grep  _f_inish    _r_emove    _k_ill | _H_ome   e_x_ecute
        _s_ave-all  _R_emove-all     |        _q_uit
  "
  ("w" wgrep-change-to-wgrep-mode)
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
