;; Rectangle marks
(defhydra sk/hydra-rectangle (:color red
                              :hint nil)
 "
 ^Move^   | ^Edit^                    | ^Menu^
 ^^^^^^-------|-------------------------|--------------
 ^ ^ _k_ ^ ^  | _s_et       _r_eset _S_tring  | _H_ome  _q_uit
 _h_ ^+^ _l_  | ex_c_hange  cop_y_  _R_eplace | _M_ark  e_x_ecute
 ^ ^ _j_ ^ ^  | _d_elete    _p_aste         |
"
  ("h" backward-char nil)
  ("l" forward-char nil)
  ("k" previous-line nil)
  ("j" next-line nil)
  ("s" (rectangle-mark-mode 1))
  ("c" exchange-point-and-mark)
  ("y" copy-rectangle-as-kill)
  ("d" kill-rectangle nil)
  ("r" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)) nil)
  ("p" yank-rectangle)
  ("S" string-rectangle)
  ("R" replace-rectangle)
  ("M" sk/hydra-of-marks/body :exit t)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("x" counsel-M-x :color blue)
  ("q" nil :color blue))

;; Multiple cursors
(sk/require-package 'multiple-cursors)

;; Hydra for multiple-cursors
(defhydra sk/hydra-of-multiple-cursors (:pre (require 'multiple-cursors)
                                        :color red
                                        :hint nil)
  "
^Mark^       | ^Unmark^   | ^Lines^                     | ^Menu^
^^^^^^^^^^^^-----------|----------|---------------------------|---------
^ ^ _k_ ^ ^  _a_ll | _p_revious | _c_hange  _#_ numbers  _c_hange | _H_ome
_h_ ^+^ _l_      | _n_ext     | _a_ppend  le_t_ters           | e_x_ecute
^ ^ _j_ ^ ^      |          | _i_nsert  _s_top              | _q_uit
"
  ("j" mc/mark-next-like-this)
  ("k" mc/mark-previous-like-this)
  ("h" mc/skip-to-next-like-this)
  ("l" mc/skip-to-previous-like-this)
  ("a" mc/mark-all-like-this)
  ("p" mc/unmark-previous-like-this)
  ("n" mc/unmark-next-like-this)
  ("c" mc/edit-lines :color blue)
  ("i" mc/edit-beginnings-of-lines :color blue)
  ("e" mc/edit-ends-of-lines :color blue)
  ("#" mc/insert-numbers :color blue)
  ("t" mc/insert-letters :color blue)
  ("s" mc/keyboard-quit)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("x" counsel-M-x :color blue)
  ("q" nil :color blue))

;; Expand regions
(sk/require-package 'expand-region)

;; Hydra of edits
(defhydra sk/hydra-of-edits (:pre (require 'expand-region)
                             :color red
                             :hint nil)
  "
 ^Line^  | ^Blank^  | ^Move^   | ^Select^                    | ^Lang^                 | ^Edit^                               | ^Menu^
 ^^^^^^^------|--------|--------|---------------------------|----------------------|------------------------------------|-----------------------
 _a_bove | _[_ up   | _{_ up   | _i_ncrease _p_ara  _o_ in()     | _b_lock-py    _s_ c-stat | _d_el-region         _K_ill-whole-line | _M_ark   _L_ang    _H_ome
 b_e_low | _]_ down | _}_ down | _r_educe   _f_unc  q_u_otes     | _h_ doc-py    _v_ c-var  | _k_ill-rest-of-line                  | mo_V_e   _M_atlab  e_x_ecute
 _j_oin  |        |        | _@_ mail   _w_ord  _-_ org-code | _l_ line-py            | cop_y_                               | _P_ython _R_       _q_uit
 _S_plit |        |        | _:_ link   la_t_ex comme_n_t    | _m_ julia-fun          | _c_omment                            | _J_ulia  _E_lisp
  "
  ("a" sk/open-line-above :color blue)
  ("e" sk/open-line-below :color blue)
  ("j" sk/join-line)
  ("S" electric-newline-and-maybe-indent)
  ("[" sk/blank-line-up)
  ("]" sk/blank-line-down)
  ("{" sk/move-text-up)
  ("}" sk/move-text-down)
  ("i" er/expand-region)
  ("r" er/contract-region)
  ("p" er/mark-text-paragraph)
  ("f" er/mark-defun)
  ("w" er/mark-symbol)
  ("@" er/mark-email)
  (":" er/mark-url)
  ("t" er/mark-LaTeX-math)
  ("n" er/mark-comment)
  ("o" er/mark-inside-pairs)
  ("u" er/mark-inside-quotes)
  ("-" er/mark-org-code-block)
  ("b" er/mark-python-block)
  ("h" er/mark-python-string)
  ("l" er/mark-python-statement)
  ("m" er/mark-ruby-block-up)
  ("s" er/c-mark-statement)
  ("v" er/c-mark-fully-qualified-name)
  ("d" kill-region :color blue)
  ("K" kill-whole-line :color blue)
  ("k" kill-line :color blue)
  ("y" kill-ring-save)
  ("c" 'evilnc-comment-or-uncomment-lines)
  ("P" sk/hydra-for-python/body :exit t)
  ("M" sk/hydra-for-matlab/body :exit t)
  ("R" sk/hydra-for-r/body :exit t)
  ("E" sk/hydra-for-elisp/body :exit t)
  ("J" sk/hydra-for-julia/body :exit t)
  ("L" sk/hydra-of-langs/body :exit t)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("V" sk/hydra-of-motion/body :exit t)
  ("M" sk/hydra-of-marks/body :exit t)
  ("x" counsel-M-x :color blue)
  ("q" nil :color blue))

;; Commenting
(sk/require-package 'evil-nerd-commenter)

;; Hydra for commenting
(defhydra sk/hydra-comments (:color red
                             :hint nil)
  "
 ^Comments^              | ^Menu^
 ^^^^^^^^^----------------------|----------------------
 _c_omment  _p_ara  _r_egion | _H_ome   e_x_ecute   _q_uit
  "
  ("c" evilnc-comment-or-uncomment-lines)
  ("p" evilnc-comment-or-uncomment-paragraphs)
  ("r" comment-or-uncomment-region)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("x" counsel-M-x :color blue)
  ("q" nil :color blue))

(provide 'sk-editing)

;;; sk-editing.el ends here
