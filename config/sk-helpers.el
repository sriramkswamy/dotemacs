;; Very large file viewing
(sk/require-package 'vlf)

;; Highlight stuff
(sk/require-package 'volatile-highlights)
(require 'volatile-highlights)
(defun sk/diminish-volatile-highlights ()
  (interactive)
  (diminish 'volatile-highlights-mode ""))
(add-hook 'volatile-highlights-mode-hook 'sk/diminish-volatile-highlights)
(volatile-highlights-mode t)

;; Smart tabs
(sk/require-package 'smart-tab)
(defun sk/diminish-smart-tab ()
  (interactive)
  (diminish 'smart-tab-mode ""))
(add-hook 'smart-tab-mode-hook 'sk/diminish-smart-tab)
(global-smart-tab-mode)

;; Aggressive indent
(sk/require-package 'aggressive-indent)
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)
(add-hook 'ruby-mode-hook #'aggressive-indent-mode)
(add-hook 'cc-mode-hook #'aggressive-indent-mode)

;; Fill column indicator
(sk/require-package 'fill-column-indicator)
(setq fci-rule-width 5
      fci-rule-column 79)

;; Column enforce column that highlights if I go over 100 characters
(sk/require-package 'column-enforce-mode)
(require 'column-enforce-mode)
(setq column-enforce-column 99)
(defun sk/diminish-column-enforce ()
  (interactive)
  (diminish 'column-enforce-mode ""))
(add-hook 'column-enforce-mode-hook 'sk/diminish-column-enforce)
(add-hook 'prog-mode-hook 'column-enforce-mode)

;; Enable smartparens-mode
(sk/require-package 'smartparens)
(defun sk/diminish-smartparens ()
  (interactive)
  (diminish 'smartparens-mode ""))
(add-hook 'smartparens-mode-hook 'sk/diminish-smartparens)
(add-hook 'prog-mode-hook 'smartparens-global-mode)
(add-hook 'org-mode-hook 'smartparens-global-mode)

;; Delete trailing whitespace on save
(sk/require-package 'ws-butler)
(defun sk/diminish-ws-butler ()
  (interactive)
  (diminish 'ws-butler-mode ""))
(add-hook 'ws-butler-mode-hook 'sk/diminish-ws-butler)
(ws-butler-global-mode)

;; Region information
(sk/require-package 'region-state)
(region-state-mode)

;; Origami - Code folding
(sk/require-package 'vimish-fold)

;; Hydra for vimish-fold
(defhydra sk/hydra-vimish-fold (:color red
                                :hint nil)
  "
 ^Vimish-Fold^                          | ^Fold^     | ^Menu^
 ^^^^^^^^^-------------------------------------|----------|---------------
 _f_old  _u_nfold  _r_efold  _t_oggle  _d_elete | _n_ext     | _H_ome   e_x_ecute
       _U_nfold  _R_efold  _T_oggle  _D_elete | _p_revious |        _q_uit
  "
  ("f" vimish-fold)
  ("u" vimish-fold-unfold)
  ("r" vimish-fold-refold)
  ("t" vimish-fold-toggle)
  ("d" vimish-fold-delete)
  ("U" vimish-fold-unfold-all)
  ("R" vimish-fold-refold-all)
  ("T" vimish-fold-toggle-all)
  ("D" vimish-fold-delete-all)
  ("n" vimish-fold-next-fold)
  ("p" vimish-fold-previous-fold)
  ("H" vimish-foldsk/hydra-of-hydras/body :exit t)
  ("x" counsel-M-x :color blue)
  ("q" nil :color blue))

;; Restart Emacs from Emacs
(sk/require-package 'restart-emacs)

;; Profiler
(sk/require-package 'esup)

;; Key frequency
(sk/require-package 'keyfreq)
(add-hook 'after-init-hook 'keyfreq-mode)

(provide 'sk-helpers)

;;; sk-helpers.el ends here
