;;; sk-hydra.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; For all kinds of navigation

;;; Code:

(sk/require-package 'hydra)

;; Rectangle marks
(defhydra sk/hydra-rectangle (:color red
                              :hint nil)
 "
 ^Rectangle^
 ^^^^^^------------------------
 _p_ paste    _R_ replace
 _C_ copy     _I_ insert
 _c_ kill     _q_ quit
"
  ("h" backward-char nil)
  ("l" forward-char nil)
  ("k" previous-line nil)
  ("j" next-line nil)
  ("C" copy-rectangle-as-kill)
  ("c" kill-rectangle nil)
  ("p" yank-rectangle)
  ("R" string-rectangle)
  ("I" string-insert-rectangle)
  ("q" nil :color blue))

;; Apropos
(defhydra sk/hydra-apropos (:color blue
                            :hint nil)
  "
 ^Apropos - Search anything^
 ^^^^^^^^^-----------------------------
 _a_ all   _d_ doc    _v_ var  _c_ cmd
 _u_ user  _e_ value  _l_ lib  _q_ quit
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
 ^Help^
 ^^^^^^^^^---------------------------------------------
 _b_ binding    _i_ info     _t_ tutorial   _a_ apropos
 _f_ function   _s_ symbol   _p_ package    _l_ lang-env
 _v_ variable   _e_ emacs    _h_ help       _q_ quit
 _m_ mode       _x_ syntax   _k_ key
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

;; aux requirements
(require 'sk-hydra-bindings)
(require 'sk-hydra-modalka)

(provide 'sk-hydra)
;;; sk-hydra.el ends here
