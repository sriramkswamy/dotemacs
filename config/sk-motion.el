;; Don't lose the cursor
(sk/require-package 'beacon)
(defun sk/diminish-beacon ()
  (interactive)
  (diminish 'beacon-mode ""))
(add-hook 'beacon-mode-hook 'sk/diminish-beacon)
(beacon-mode 1)

;; Avy
(sk/require-package 'avy)

;; Hydra of motion
(defhydra sk/hydra-of-motion (:color red
                              :hint nil)
  "
 ^Move^   | ^Line^    | ^File^  | ^Mark^     | ^Para^   | ^Screen^ | ^Word^     | ^Edit^      | ^Menu^
 ^^^^^^-------|---------|-------|----------|--------|--------|----------|-----------|---------------
 ^ ^ _k_ ^ ^  | _a_ start | _<_ beg | _s_et      | _}_ next | _]_ next | for_w_ard  | _d_el       | _P_ython _H_ome
 _h_ ^+^ _l_  | _e_nd     | _>_ end | _r_eset    | _{_ prev | _[_ prev | _b_ackward | cop_y_      | _J_ulia  e_x_ecute
 ^ ^ _j_ ^ ^  | _g_oto    |       | ex_c_hange |        |        |          | _R_ectangle | _L_ang   _q_uit
"
  ("h" backward-char)
  ("j" next-line)
  ("k" previous-line)
  ("l" forward-char)
  ("a" move-beginning-of-line)
  ("e" move-end-of-line)
  ("g" avy-goto-line)
  ("r" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)) nil)
  ("s" set-mark-command)
  ("c" exchange-point-and-mark)
  ("<" beginning-of-buffer)
  (">" end-of-buffer)
  ("}" forward-paragraph)
  ("{" backward-paragraph)
  ("]" scroll-up)
  ("[" scroll-down)
  ("w" forward-word)
  ("b" backward-word)
  ("d" delete-region)
  ("y" copy-region-as-kill)
  ("R" sk/hydra-rectangle/body :exit t)
  ("P" sk/hydra-for-python/body :exit t)
  ("J" sk/hydra-for-julia/body :exit t)
  ("L" sk/hydra-of-langs/body :exit t)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("x" counsel-M-x :color blue)
  ("q" nil :color blue))

;; Hydra of windows
(defhydra sk/hydra-of-windows (:color red
                               :hint nil)
  "
 ^Move^   | ^Size^   | ^Change^        | ^Split^        | ^Frame^                | ^Text^       | ^Config^   | ^Menu^
 ^^^^^^^^^^^-------|--------|---------------|--------------|----------------------|------------|----------|--------------
 ^ ^ _k_ ^ ^  | ^ ^ _{_ ^ ^  | _u_ winner-undo | _v_ertical     | _f_ullscreen  _m_aximize | _+_ zoom in  | _i_ config | _H_ome  e_x_ecute
 _h_ ^+^ _l_  | _<_ ^+^ _>_  | _r_ winner-redo | _s_ horizontal | _d_elete      _M_inimize | _-_ zoom out |          | mo_V_e  _q_uit
 ^ ^ _j_ ^ ^  | ^ ^ _}_ ^ ^  | _c_lose         | _z_oom         | _S_elect      _n_ame     |            |          |
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
  ("f" sk/toggle-frame-fullscreen-non-native)
  ("z" delete-other-windows)
  ("u" (progn
         (winner-undo)
         (setq this-command 'winner-undo)))
  ("r" winner-redo)
  ("m" toggle-frame-maximized)
  ("M" suspend-frame)
  ("d" delete-frame)
  ("S" select-frame-by-name)
  ("n" set-frame-name)
  ("+" text-scale-increase)
  ("-" text-scale-decrease)
  ("i" sk/hydra-for-eyebrowse/body :exit t)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("V" sk/hydra-of-motion/body :exit t)
  ("x" counsel-M-x :color blue)
  ("q" nil :color blue))

(provide 'sk-motion)

;;; sk-motion.el ends here
