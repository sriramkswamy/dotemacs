;; ESS - Emacs speaks statistics
(sk/require-package 'ess)
(add-to-list 'auto-mode-alist '("\\.R$" . R-mode))

;; Vertical split julia REPL
(defun sk/julia-shell-here ()
  "opens up a new julia REPL in the directory associated with the current buffer's file."
  (interactive)
  (require 'ess-site)
  (split-window-right)
  (other-window 1)
  (julia)
  (other-window 1))

;; Vertical split r REPL
(defun sk/r-shell-here ()
  "opens up a new r REPL in the directory associated with the current buffer's file."
  (interactive)
  (require 'ess-site)
  (split-window-right)
  (other-window 1)
  (R)
  (other-window 1))

;; Hydra - for ESS
(defhydra sk/hydra-for-ess (:color red
                              :hint nil)
  "
 ^Send^     | ^Shell^  | ^Menu^
 ^^^^^^^^^---------|--------|----------------
 _f_unction | _J_ulia  | _H_ome    e_x_ecute
 _l_ine     | _R_      | _L_ang    _q_uit
 _r_egion   | s_w_itch |
 _a_ll      |        |
"
  ("f" ess-eval-function)
  ("l" ess-eval-line)
  ("r" ess-eval-region)
  ("a" ess-eval-buffer)
  ("J" sk/julia-shell-here)
  ("R" sk/r-shell-here)
  ("w" ess-switch-to-ESS)
  ("L" sk/hydra-of-langs/body :exit t)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("x" counsel-M-x :color blue)
  ("q" nil :color blue))

(provide 'sk-ess)

;;; sk-ess.el ends here
