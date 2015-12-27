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
(smartparens-global-mode)

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
(sk/require-package 'origami)

;; Hydra for origami
(defhydra sk/hydra-origami (:color red
                            :hint nil)
  "
 ^Origami^             | ^Fold^     | ^Menu^
 ^^^^^^^^^--------------------|----------|---------------
 _o_pen  _c_lose  _t_oggle | _n_ext     | _H_ome   e_x_ecute
 _O_pen  _C_lose  _T_oggle | _p_revious |        _q_uit
  "
  ("o" origami-open-node)
  ("c" origami-close-node)
  ("t" origami-toggle-node)
  ("O" origami-open-all-nodes)
  ("C" origami-close-all-nodes)
  ("T" origami-toggle-all-nodes)
  ("n" origami-next-fold)
  ("p" origami-previous-fold)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("x" counsel-M-x :color blue)
  ("q" nil :color blue))

;; Restart Emacs from Emacs
(sk/require-package 'restart-emacs)

;; Profiler
(sk/require-package 'esup)

(provide 'sk-helpers)

;;; sk-helpers.el ends here
