;;; sk-hydra.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; For all kinds of navigation

;;; Code:

(sk/require-package 'hydra)

;; Apropos
(defhydra sk/hydra-apropos (:color blue
                            :hint nil)
  "
 _a_: all   _d_: doc    _v_: var  _c_: cmd
 _u_: user  _e_: value  _l_: lib  _q_: quit
"
  ("a" apropos)
  ("d" apropos-documentation)
  ("v" apropos-variable)
  ("c" apropos-command)
  ("l" apropos-library)
  ("u" apropos-user-option)
  ("e" apropos-value)
  ("q" nil :color blue))

;; Help
(defhydra sk/hydra-of-help (:color blue
                            :hint nil)
  "
 _b_: binding    _i_: info     _t_: tutorial   _a_: apropos
 _f_: function   _s_: symbol   _p_: package    _l_: lang-env
 _v_: variable   _e_: emacs    _h_: help       _q_: quit
 _m_: mode       _x_: syntax   _k_: key
"
  ("b" describe-bindings)
  ("f" counsel-describe-function)
  ("v" counsel-describe-variable)
  ("m" describe-mode)
  ("i" info)
  ("s" info-lookup-symbol)
  ("e" info-emacs-manual)
  ("x" describe-syntax)
  ("t" help-with-tutorial)
  ("p" describe-package)
  ("h" help-for-help)
  ("k" describe-key)
  ("a" sk/hydra-apropos/body :exit t)
  ("l" describe-language-environment)
  ("q" nil :color blue))

;; Activate modes
(defhydra sk/hydra-of-activate (:color red
                                :hint nil)
  "
 _b_: battery   _n_: number   _v_: visual      _c_: company   _i_: indentation   _k_: which-key    _l_: length
 _t_: time      _w_: wrap     _y_: yasnippet   _m_: margin    _j_: jabber        _o_: org          _q_: quit
 _f_: font      _s_: spell    _a_: anaconda    _d_: fold      _g_: ggtags        _p_: paradox
"
  ("b" display-battery-mode)
  ("t" display-time-mode)
  ("f" set-frame-font :color blue)
  ("n" linum-mode)
  ("w" toggle-truncate-lines)
  ("s" flyspell-mode :color blue)
  ("v" visual-line-mode :color blue)
  ("p" paradox-list-packages :color blue)
  ("c" company-mode :color blue)
  ("l" column-enforce-mode :color blue)
  ("y" yas-minor-mode :color blue)
  ("a" anaconda-mode :color blue)
  ("i" highlight-indentation-mode)
  ("m" fci-mode)
  ("j" jabber-connect :color blue)
  ("o" sk/org-custom-load :color blue)
  ("g" ggtags-mode :color blue)
  ("d" global-origami-mode :color blue)
  ("k" which-key-mode :color blue)
  ("q" nil :color blue))

;; bindings for hydras
(require 'sk-hydra-bindings)

(provide 'sk-hydra)
;;; sk-hydra.el ends here
