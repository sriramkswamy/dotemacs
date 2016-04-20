;;; sk-navigation-hydra.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Hydras for navigation

;;; Code:

;; Hydra of window
(defhydra sk/hydra-of-windows (:color red
                               :hint nil)
  "
 ^Move^    ^Size^    ^Change^           ^Split^          ^Frame^                      ^Text^
 ^^^^^^^^^^^----------------------------------------------------------------------------------------
 ^ ^ _k_ ^ ^   ^ ^ _{_ ^ ^   _u_ winner-undo    _v_ ertical      _f_ fullscreen  _m_ maximize   _+_ zoom in
 _h_ ^+^ _l_   _<_ ^+^ _>_   _r_ winner-redo    _s_ horizontal   _d_ delete      _p_ suspend    _-_ zoom out
 ^ ^ _j_ ^ ^   ^ ^ _}_ ^ ^   _c_ close          _z_ zoom         _e_ select      _n_ name       _q_ quit
"
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)
  ("<" shrink-window-horizontally)
  ("{" shrink-window)
  ("}" enlarge-window)
  (">" enlarge-window-horizontally)
  ("v" sk/split-right-and-move)
  ("s" sk/split-below-and-move)
  ("c" delete-window)
  ("f" sk/toggle-frame-fullscreen-non-native :color blue)
  ("z" delete-other-windows)
  ("u" (progn
         (winner-undo)
         (setq this-command 'winner-undo)))
  ("r" winner-redo)
  ("m" toggle-frame-maximized :color blue)
  ("p" suspend-frame :color blue)
  ("d" delete-frame)
  ("e" select-frame-by-name :color blue)
  ("n" set-frame-name :color blue)
  ("+" text-scale-increase)
  ("-" text-scale-decrease)
  ("q" nil :color blue))

;; Bookmarks - hydra
(defhydra sk/hydra-bookmarks (:color red
                              :hint nil)
  "
 ^Bookmarks^
 ^^^^^^^^^------------------------------------
 _s_et  _b_ookmark   _j_ump   _d_elete   _q_uit
  "
  ("s" bookmark-set)
  ("b" bookmark-save)
  ("j" bookmark-jump)
  ("d" bookmark-delete)
  ("q" nil :color blue))

;; global and modalka bindings for hydras
(require 'sk-navigation-hydra-bindings)
(require 'sk-navigation-hydra-modalka)

(provide 'sk-navigation-hydra)
;;; sk-navigation-hydra.el ends here
