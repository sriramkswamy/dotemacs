;;; sk-ess-hydra.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; For Python

;;; Code:

;; Send hydra
(defhydra sk/hydra-for-ess (:color red
                            :hint nil)
  "
 _f_: func        _l_: line    _j_: Julia  _S_: switch
 _s_: selection   _b_: buffer  _r_: R      _q_: quit
"
  ("f" ess-eval-function)
  ("l" ess-eval-line)
  ("s" ess-eval-region)
  ("b" ess-eval-buffer)
  ("j" sk/julia-shell-here)
  ("r" sk/r-shell-here)
  ("S" ess-switch-to-ESS)
  ("q" nil :color blue))

;; Binding
(global-set-key (kbd "C-c h c s") 'sk/hydra-for-ess/body)

;; Modal binding
(modalka-define-kbd "c s" "C-c h c s")

;; Which key explanation
(which-key-add-key-based-replacements
  "c s" "stats code")

(provide 'sk-ess-hydra)
;;; sk-ess-hydra.el ends here
