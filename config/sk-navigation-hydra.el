;;; sk-navigation-hydra.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Hydras for navigation

;;; Code:

;; Hydra of window
(defhydra sk/hydra-of-windows (:color red
                               :hint nil)
  "
 ^Move^    ^Size^    ^Change^           ^Split^          ^Text^
 ^^^^^^^^^^^------------------------------------------------------------------
 ^ ^ _k_ ^ ^   ^ ^ _{_ ^ ^   _u_: winner-undo   _v_: vertical     _+_: zoom in
 _h_ ^+^ _l_   _<_ ^+^ _>_   _r_: winner-redo   _s_: horizontal   _-_: zoom out
 ^ ^ _j_ ^ ^   ^ ^ _}_ ^ ^   _c_: close         _z_: zoom         _q_: quit
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
  ("+" text-scale-increase)
  ("-" text-scale-decrease)
  ("q" nil :color blue))

;; Bookmarks - hydra
(defhydra sk/hydra-bookmarks (:color blue
                              :hint nil)
  "
 _s_: set  _b_: bookmark   _j_: jump   _d_: delete   _q_: quit
  "
  ("s" bookmark-set)
  ("b" bookmark-save)
  ("j" bookmark-jump)
  ("d" bookmark-delete)
  ("q" nil :color blue))

;; bindings for hydras
(require 'sk-navigation-hydra-bindings)

(provide 'sk-navigation-hydra)
;;; sk-navigation-hydra.el ends here
