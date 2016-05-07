;;; sk-python-hydra.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Hydras for Python

;;; Code:

;; Send hydra
(defhydra sk/hydra-for-py (:color blue
                           :hint nil)
  "
 ^Send^                     ^Navigate^
^^^^^^^^^^-----------------------------------------------------------------
 _r_: region    _s_: start    _d_: definition    _F_: file    _q_: quit
 _b_: buffer    _S_: switch   _a_: assignment    _B_: back
 _f_: func                  _v_: reference
"
  ("r" python-shell-send-region)
  ("b" python-shell-send-buffer)
  ("f" python-shell-send-defun)
  ("s" run-python)
  ("S" python-shell-switch-to-shell)
  ("d" anaconda-mode-find-definitions :color red)
  ("D" anaconda-mode-show-doc :color red)
  ("a" anaconda-mode-find-assignments :color red)
  ("v" anaconda-mode-find-references :color red)
  ("F" anaconda-mode-find-file :color red)
  ("B" anaconda-mode-go-back :color red)
  ("q" nil :color blue))

;; Binding
(global-set-key (kbd "C-c h c p") 'sk/hydra-for-py/body)

;; Modal binding
(modalka-define-kbd "c p" "C-c h c p")

;; Which key explanation
(which-key-add-key-based-replacements
  "c p" "python code")

(provide 'sk-python-hydra)
;;; sk-python-hydra.el ends here
