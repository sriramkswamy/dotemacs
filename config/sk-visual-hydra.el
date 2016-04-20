;;; sk-visual-hydra.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Visual aids

;;; Code:

;; Hydra for vimish-fold
(defhydra sk/hydra-vimish-fold (:color red
                                :hint nil)
  "
 ^Vimish-Fold^                             ^Fold^
 ^^^^^^^^^--------------------------------------------------------------
 _f_old  _u_ unfold  _r_ refold  _t_ toggle  _d_ delete    _n_ next      _q_ quit
       _U_ Unfold  _R_ Refold  _T_ Toggle  _D_ Delete    _p_ previous
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
