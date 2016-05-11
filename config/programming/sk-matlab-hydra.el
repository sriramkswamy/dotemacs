;;; sk-matlab-hydra.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Matlab hydra

;;; Code:

;; Make hydra
(defhydra sk/hydra-for-matlab (:color blue
                               :hint nil)
  "
 _c_: cell   _r_: region    _s_: start    _q_: quit
 _l_: line   _C_: command   _S_: switch
"
  ("c" matlab-shell-run-cell)
  ("l" matlab-shell-run-region-or-line)
  ("r" matlab-shell-run-region)
  ("C" matlab-shell-run-command)
  ("s" sk/matlab-shell-here)
  ("S" matlab-show-matlab-shell-buffer)
  ("q" nil :color blue))

;; Binding
(global-set-key (kbd "C-c h c m") 'sk/hydra-for-matlab/body)

;; Modal binding
(modalka-define-kbd "c m" "C-c h c m")

;; Which key explanation
(which-key-add-key-based-replacements
  "c m" "matlab code")

(provide 'sk-matlab-hydra)
;;; sk-matlab-hydra.el ends here
