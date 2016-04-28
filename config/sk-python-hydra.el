;;; sk-python-hydra.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Hydras for Python

;;; Code:

;; Send hydra
(defhydra sk/hydra-for-py (:color blue
			   :hint nil)
  "
 _r_: region    _s_: start    _q_: quit
 _b_: buffer    _S_: switch
 _f_: func
"
  ("r" python-shell-send-region)
  ("b" python-shell-send-buffer)
  ("f" python-shell-send-defun)
  ("s" run-python)
  ("S" python-shell-switch-to-shell)
  ("q" nil :color blue))

;; Code jump hydra
(defhydra sk/hydra-for-py-jump (:color red
			        :hint nil)
  "
 _d_: definition    _a_: assignment    _f_: file    _q_: quit
 _D_: doc           _r_: reference     _b_: back
"
  ("d" anaconda-mode-find-definitions)
  ("D" anaconda-mode-show-doc)
  ("a" anaconda-mode-find-assignments)
  ("r" anaconda-mode-find-references)
  ("f" anaconda-mode-find-file)
  ("b" anaconda-mode-go-back)
  ("q" nil :color blue))

;; aux requirements
(require 'sk-python-hydra-bindings)

(provide 'sk-python-hydra)
;;; sk-python-hydra.el ends here
