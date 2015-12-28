;; Hydra of jump
(defhydra sk/hydra-of-jump (:color red
                            :hint nil)
  "
 ^Diff^       | ^Errors^     | ^Spell^      | ^Mark^        | ^Fold^     | ^Menu^
 ^^^^^^^^^-----------|------------|------------|-------------|----------|--------------
 _j_ next     | _l_ next     | _>_ next     | _]_ loc next  | _n_ext     | _H_ome  e_x_ecute
 _k_ previous | _h_ previous | _<_ previous | _[_ loc prev  | _p_revious |       _Q_uit
 _g_oto       | _a_ll        | _c_orrect    | _}_ glob next |          |
 _r_evert     | _f_irst      |            | _{_ glob prev |          |
 "
  ("j" diff-hl-next-hunk)
  ("k" diff-hl-previous-hunk)
  ("g" diff-hl-diff-goto-hunk)
  ("r" diff-hl-revert-hunk)
  ("l" flycheck-next-error)
  ("h" flycheck-previous-error)
  ("a" flycheck-list-errors :color blue)
  ("f" flycheck-first-error)
  (">" flyspell-goto-next-error)
  ("<" flyspell-goto-previous-error)
  ("]" back-button-local-forward)
  ("[" back-button-local-backward)
  ("}" back-button-global-forward)
  ("{" back-button-global-backward)
  ("n" origami-next-fold)
  ("p" origami-previous-fold)
  ("c" ispell-word)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("x" counsel-M-x :color blue)
  ("Q" nil :color blue))

;; Hydra of languages
(defhydra sk/hydra-of-langs (:color red
                             :hint nil)
  "
 ^Languages^                  | ^Errors^           | ^Eshell^       | ^Terminal^     | ^Tmux^  | ^Menu^
 ^^^^^^^^^---------------------------|------------------|--------------|--------------|-------|----------------
 _c_++    _j_ulia   _e_lisp   _d_oc | _h_ previous _a_ll   | _+_ vertical   | _|_ vertical   | _s_tart | _H_ome    _Q_uit
 _p_ython _r_       _m_atlab      | _l_ next     _f_irst | _-_ horizontal | ___ horizontal |       |         e_x_ecute
"
  ("c" sk/hydra-for-cpp/body :exit t)
  ("p" sk/hydra-for-python/body :exit t)
  ("j" sk/hydra-for-julia/body :exit t)
  ("r" sk/hydra-for-r/body :exit t)
  ("e" sk/hydra-for-elisp/body :exit t)
  ("m" sk/hydra-for-matlab/body :exit t)
  ("s" sk/hydra-for-emamux/body :exit t)
  ("d" dash-at-point-with-docset :color blue)
  ("+" sk/eshell-vertical)
  ("-" sk/eshell-horizontal)
  ("|" sk/multi-term-vertical)
  ("_" sk/multi-term-horizontal)
  ("h" flycheck-next-error)
  ("l" flycheck-previous-error)
  ("f" flycheck-first-error)
  ("a" flycheck-list-errors :color blue)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("x" counsel-M-x :color blue)
  ("Q" nil :color blue))

;; Hydra of search
(defhydra sk/hydra-of-search (:color blue
                              :hint nil)
  "
 ^Buffer^          | ^Project^  | ^Desktop^ | ^Google^ | ^Menu^
 ^^^^^^^^^----------------|----------|---------|--------|--------------
 _s_earch  _w_ord    | _p_roject  | _d_esktop | _g_oogle | _H_ome  e_x_ecute
 _a_ll     _r_eplace | refac_t_or | _f_ast    | _c_ursor |       _Q_uit
"
  ("s" swiper)
  ("a" swiper-all)
  ("w" swoop-pcre-regexp)
  ("r" vr/query-replace)
  ("p" counsel-ag)
  ("t" ag)
  ("d" spotlight)
  ("f" spotlight-fast)
  ("g" google-this-search)
  ("c" google-this)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("x" counsel-M-x :color blue)
  ("Q" nil :color blue))

;; Activate modes
(defhydra sk/hydra-of-activate (:color red
                                :hint nil)
  "
 ^(De)Activate^                 | ^Packages^   | ^Minor mode^                                  | ^Menu^
 ^^^^^^^^^-----------------------------|------------|---------------------------------------------|--------
 _b_attery   _n_umber   scrollb_a_r | _p_aradox    | _c_ompany    _i_ndentation  _W_hich-key  col_f_orce | _H_ome
 _t_ime      _w_rap     toolba_r_   | instal_l_    | _y_asnippet  f_C_i          _j_abber    _o_rg       | e_x_ecute
 _F_ont      _s_pell    _v_ theme   | _I_nitialize | _e_lpy       f_O_ld         _g_gtags              | _Q_uit
"
  ("b" display-battery-mode)
  ("t" display-time-mode)
  ("F" set-frame-font :color blue)
  ("n" linum-mode :color blue)
  ("w" toggle-truncate-lines :color blue)
  ("s" flyspell-mode :color blue)
  ("r" tool-bar-mode)
  ("a" scroll-bar-mode)
  ("v" load-theme :color blue)
  ("p" paradox-list-packages :color blue)
  ("l" package-install :color blue)
  ("I" package-initialize :color blue)
  ("c" company-mode :color blue)
  ("f" column-enforce-mode :color blue)
  ("y" yas-global-mode :color blue)
  ("e" elpy-enable :color blue)
  ("i" highlight-indentation-mode :color blue)
  ("C" fci-mode)
  ("j" jabber-connect :color blue)
  ("o" sk/org-custom-load :color blue)
  ("g" ggtags-mode :color blue)
  ("O" global-origami-mode :color blue)
  ("W" which-key-mode :color blue)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("x" counsel-M-x :color blue)
  ("Q" nil :color blue))

;; Hydra of undo
(defhydra sk/hydra-of-undo (:color red
                            :hint nil)
  "
 ^Undo^             | ^Menu^
 ^^^^^^^^^-----------------|---------------------
 _u_ndo  _r_edo  _v_iew | _H_ome   e_x_ecute  _Q_uit
"
  ("u" undo-tree-undo)
  ("r" undo-tree-redo)
  ("v" undo-tree-visualize :color blue)
  ("E" sk/hydra-of-edits/body :exit t)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("x" counsel-M-x :color blue)
  ("Q" nil :color blue))

;; Hydra of repeat
(defhydra sk/hydra-of-repeat (:color red
                              :hint nil)
  "
 ^Repeat^  | ^Menu^
 ^^^^^^^^^--------|----------------------
 _i_vy     | _H_ome   e_x_ecute   _Q_uit
  "
  ("i" ivy-resume)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("x" counsel-M-x :color blue)
  ("Q" nil :color blue))

;; Hydra of hydras
(defhydra sk/hydra-of-hydras (:color blue
                              :hint nil)
  "
 ^Menu^                              | ^Emacs^
 ^^^^^^^^^----------------------------------|---------------
 _n_avigate  _a_ctiavte  _o_rg   _e_dit    | _S_ave   e_x_ecute
 _w_indow    mo_v_e      _l_ang  _s_earch  | _C_lose  _Q_uit
 _h_elp      _m_ark      _g_it   _c_ursors |
"
  ("n" sk/hydra-of-navigation/body :exit t)
  ("w" sk/hydra-of-windows/body :exit t)
  ("h" sk/hydra-of-help/body :exit t)
  ("a" sk/hydra-of-activate/body :exit t)
  ("v" sk/hydra-of-motion/body :exit t)
  ("m" sk/hydra-of-marks/body :exit t)
  ("o" sk/hydra-of-org/body :exit t)
  ("l" sk/hydra-of-langs/body :exit t)
  ("g" sk/hydra-of-git/body :exit t)
  ("e" sk/hydra-of-edits/body :exit t)
  ("s" sk/hydra-of-search/body :exit t)
  ("c" sk/hydra-of-multiple-cursors/body :exit t)
  ("S" save-buffer :exit t)
  ("C" save-buffers-kill-terminal :exit t)
  ("x" counsel-M-x :color blue)
  ("Q" nil :color blue))

(provide 'sk-hydras)

;;; sk-hydras.el ends here
