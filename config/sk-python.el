;; Highlight indentation
(sk/require-package 'highlight-indentation)

;; Elpy
(sk/require-package 'elpy)
(add-hook 'python-mode-hook 'elpy-enable)
(defun diminish-elpy ()
  (interactive)
  (elpy-use-ipython)
  (diminish 'elpy-mode ""))
(add-hook 'elpy-mode-hook 'diminish-elpy)

;; Cython mode
(sk/require-package 'cython-mode)

;; Virtualenv for python
(sk/require-package 'virtualenvwrapper)
(setq venv-location "~/Py34/")

;; Hydra - for python
(defhydra sk/hydra-for-python (:color red
                               :hint nil)
  "
 ^Send^     | ^Shell^  | ^Navigate^   |
 ^^^^^^^^^---------|--------|------------|--------------
 _f_unction | _s_tart  | _d_efinition | _H_ome  e_x_ecute
 _l_ine     | s_w_itch | l_o_cation   | _L_ang  _Q_uit
 _r_egion   |        |            |
 _a_ll      |        |            |
"
  ("f" python-shell-send-defun)
  ("l" elpy-shell-send-current-statement)
  ("r" elpy-shell-send-region)
  ("a" elpy-shell-send-buffer)
  ("s" elpy-shell-switch-to-shell)
  ("w" elpy-shell-switch-to-shell)
  ("d" elpy-goto-definition)
  ("o" elpy-goto-location)
  ("L" sk/hydra-of-langs/body :exit t)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("x" counsel-M-x :color blue)
  ("Q" nil :color blue))

(provide 'sk-python)

;;; sk-python.el ends here
