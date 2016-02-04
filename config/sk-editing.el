;; Rectangle marks
(defhydra sk/hydra-rectangle (:color red
                              :hint nil)
 "
 ^Move^   | ^Edit^                    | ^Menu^
 ^^^^^^-------|-------------------------|--------------
 ^ ^ _k_ ^ ^  | _s_et       _r_eset _S_tring  | _H_ome  _q_uit
 _h_ ^+^ _l_  | ex_c_hange  cop_y_  _R_eplace | _M_ove  e_x_ecute
 ^ ^ _j_ ^ ^  | _d_elete    _p_aste         | _C_ua
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
  ("C" sk/hydra-cua-selection/body :exit t)
  ("M" sk/hydra-of-motion/body :exit t)
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
(defun sk/start-expand-region ()
  (interactive)
  (require 'expand-region))
(add-hook 'prog-mode-hook 'sk/start-expand-region)
(add-hook 'text-mode-hook 'sk/start-expand-region)

;; Hydra of edits
(defhydra sk/hydra-of-edits (:pre (require 'expand-region)
                             :color red
                             :hint nil)
  "
 ^Line^  | ^Blank^  | ^Move^   | ^Select^                               |  ^Edit^                              | ^Menu^
 ^^^^^^^------|--------|--------|--------------------------------------|------------------------------------|-----------------------
 _a_bove | _[_ up   | _{_ up   | _i_ncrease _p_ara  _b_lock     _l_ine        | _d_el-region         _K_ill-whole-line | _M_ove   _L_ang    _H_ome
 b_e_low | _]_ down | _}_ down | _r_educe   _f_unc  q_u_otes    _v_ar         | _k_ill-rest-of-line  _s_et-again       | _E_lisp  _M_atlab  e_x_ecute
 _J_oin  |        |        | _h_unk     _w_ord  _o_rg-code  _m_ py-block  | cop_y_                               | _P_ython _R_       _q_uit
 _S_plit |        |        | _:_ link   la_t_ex comme_n_t   _j_ julia-fun | _c_omment                            |
  "
  ("a" sk/open-line-above :color blue)
  ("e" sk/open-line-below :color blue)
  ("J" sk/join-line)
  ("S" electric-newline-and-maybe-indent)
  ("[" sk/blank-line-up)
  ("]" sk/blank-line-down)
  ("{" sk/move-text-up)
  ("}" sk/move-text-down)
  ("i" er/expand-region)
  ("r" er/contract-region)
  ("p" er/mark-text-paragraph)
  ("f" er/mark-defun)
  ("w" er/mark-word)
  ("h" diff-hl-mark-hunk)
  (":" er/mark-url)
  ("t" er/mark-LaTeX-math)
  ("n" er/mark-comment)
  ("b" er/mark-inside-pairs)
  ("u" er/mark-inside-quotes)
  ("l" sk/select-current-line)
  ("o" er/mark-org-code-block)
  ("m" er/mark-python-block)
  ("j" er/mark-ruby-block-up)
  ("s" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)) nil)
  ("v" er/mark-symbol)
  ("d" kill-region :color blue)
  ("K" kill-whole-line :color blue)
  ("k" kill-line :color blue)
  ("y" kill-ring-save)
  ("c" comment-dwim-2)
  ("P" sk/hydra-for-python/body :exit t)
  ("M" sk/hydra-for-matlab/body :exit t)
  ("R" sk/hydra-for-ess/body :exit t)
  ("E" sk/hydra-for-elisp/body :exit t)
  ("L" sk/hydra-of-langs/body :exit t)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("M" sk/hydra-of-motion/body :exit t)
  ("x" counsel-M-x :color blue)
  ("q" nil :color blue))

;; Commenting
(sk/require-package 'comment-dwim-2)
(global-set-key (kbd "M-;") 'comment-dwim-2)

(provide 'sk-editing)

;;; sk-editing.el ends here
