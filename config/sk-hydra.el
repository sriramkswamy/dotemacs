;; hydra
(use-package hydra
  :ensure t
  :defer t)

;; smartparens hydra
(defhydra hydra-smartparens (:color red :hint nil)
  "
 ^Move^              ^Edit^                                              ^Splice^
^^^^^^^^^^^^^--------------------------------------------------------------------------------------------------
 ^ ^ _k_ ^ ^    ^ ^ _p_ ^ ^    _<_: barf backward    _u_: unwrap       _x_: transpose  _S_: splice   _q_: quit
 _h_ ^+^ _l_    _b_ ^+^ _f_    _>_: barf forward     _U_: unwrap back  _c_: convolute  _F_: forward
 ^ ^ _j_ ^ ^    ^ ^ _n_ ^ ^    _)_: slurp forward    _d_: delete       _r_: raise      _B_: backward
 _a_: start _e_: end   _(_: slurp backward   _y_: copy         _s_: split      _A_: around
"
  ("h" sp-backward-sexp)
  ("l" sp-forward-sexp)
  ("j" sp-next-sexp)
  ("k" sp-previous-sexp)
  ("p" sp-backward-down-sexp)
  ("n" sp-up-sexp)
  ("f" sp-down-sexp)
  ("b" sp-backward-up-sexp)
  ("a" sp-beginning-of-sexp)
  ("e" sp-end-of-sexp)
  ("<" sp-backward-barf-sexp)
  (">" sp-forward-barf-sexp)
  ("(" sp-backward-slurp-sexp)
  (")" sp-forward-slurp-sexp)
  ("u" sp-unwrap-sexp)
  ("U" sp-backward-unwrap-sexp)
  ("d" sp-kill-sexp)
  ("y" sp-copy-sexp)
  ("x" sp-transpose-sexp)
  ("c" sp-convolute-sexp)
  ("r" sp-raise-sexp)
  ("s" sp-split-sexp)
  ("S" sp-splice-sexp)
  ("F" sp-splice-sexp-killing-forward)
  ("B" sp-splice-sexp-killing-backward)
  ("A" sp-splice-sexp-killing-around)
  ("q" nil :color blue))

;; hydra for debugging
(defhydra hydra-debug (:pre (sk/gud-mode) :color red :hint nil)
  "
 ^Debuggers^   ^Breakpoints^    ^Navigation^             ^Quit^
^^^^^^^^^^^^^--------------------------------------------------------------------------
 _g_: gdb      _b_: toggle      _j_: next  _i_: step in    _k_: kill debug
 _p_: pdb      _r_: remove      _s_: step  _o_: step out   _q_: quit hydra
                            _c_: cont  _f_: finish
"
  ;; debuggers
  ("g" realgud:gdb)
  ("p" realgud:pdb)
  ;; breakpoints
  ("b" sk/gud-break)
  ("r" sk/gud-remove)
  ;; navigation
  ("j" sk/gud-next)
  ("s" sk/gud-step)
  ("i" sk/gud-down)
  ("o" sk/gud-up)
  ("c" sk/gud-cont)
  ("f" sk/gud-finish)
  ;; quit
  ("k" gdb-exit :color blue)
  ("q" nil :color blue))

;; provide hydra
(provide 'sk-hydra)
