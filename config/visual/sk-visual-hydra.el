;;; sk-visual-hydra.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Visual aids

;;; Code:

;; Activate modes
(defhydra sk/hydra-of-activate (:color blue
			        :hint nil)
  "
 _b_: battery   _n_: number       _v_: wrap        _c_: column    _i_: indent        _k_: which-key   _l_: talk
 _t_: time      _w_: weather      _y_: yasnippet   _m_: margin    _s_: smartparens   _o_: org-load    _j_: jabber
 _f_: flyspell  _a_: auto-comp    _d_: fold        _g_: ggtags    _p_: paradox       _e_: error       _q_: quit
"
  ("b" fancy-battery-mode :color red)
  ("t" display-time-mode :color red)
  ("n" linum-mode :color red)
  ("w" wttrin)
  ("f" flyspell-mode)
  ("v" visual-line-mode)
  ("p" paradox-list-packages)
  ("c" column-enforce-mode)
  ("y" yas-global-mode)
  ("a" company-mode)
  ("i" highlight-indentation-mode)
  ("m" fci-mode :color red)
  ("j" jabber-connect :color red)
  ("l" jabber-chat-with)
  ("o" sk/org-custom-load :color blue)
  ("g" ggtags-mode)
  ("d" global-origami-mode)
  ("k" which-key-mode)
  ("s" smartparens-strict-mode)
  ("e" global-flycheck-mode)
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
