;;; sk-visual-hydra.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Visual aids

;;; Code:

;; Activate modes
(defhydra sk/hydra-of-activate (:color red
			        :hint nil)
  "
 _b_: battery   _n_: number       _v_: wrap        _c_: column    _i_: indent   _k_: which-key   _l_: length
 _t_: time      _w_: weather      _y_: yasnippet   _m_: margin    _j_: jabber   _o_: org-load    _s_: pairs
 _f_: flyspell   _a_: auto-comp   _d_: fold        _g_: ggtags    _p_: paradox  _e_: error       _q_: quit
"
  ("b" fancy-battery-mode)
  ("t" display-time-mode)
  ("n" linum-mode)
  ("w" wttrin :color blue)
  ("f" flyspell-mode :color blue)
  ("v" visual-line-mode :color blue)
  ("p" paradox-list-packages :color blue)
  ("c" column-enforce-mode :color blue)
  ("l" column-enforce-mode :color blue)
  ("y" yas-global-mode :color blue)
  ("a" company-mode :color blue)
  ("i" highlight-indentation-mode)
  ("m" fci-mode)
  ("j" jabber-connect :color blue)
  ("o" sk/org-custom-load :color blue)
  ("g" ggtags-mode :color blue)
  ("d" global-origami-mode :color blue)
  ("k" which-key-mode :color blue)
  ("s" smartparens-strict-mode :color blue)
  ("e" global-flycheck-mode :color blue)
  ("q" nil :color blue))

;; Hydra for vimish-fold
(defhydra sk/hydra-vimish-fold (:color red
                                :hint nil)
  "
 _f_: fold  _u_: unfold  _r_: refold  _t_: toggle  _d_: delete    _n_: next      _q_: quit
         _U_: Unfold  _R_: Refold  _T_: Toggle  _D_: Delete    _p_: previous
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
  ("q" nil :color blue))

;; bindings
(require 'sk-visual-hydra-bindings)

(provide 'sk-visual-hydra)
;;; sk-visual-hydra.el ends here