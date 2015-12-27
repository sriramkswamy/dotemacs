;; General
(sk/require-package 'paredit)
(defun sk/diminish-paredit ()
  (interactive)
  (diminish 'paredit-mode ""))
(add-hook 'paredit-mode-hook 'sk/diminish-paredit)

;; Modal lisp editing
(sk/require-package 'lispy)
(defun sk/diminish-lispy ()
  (interactive)
  (diminish 'lispy-mode ""))
(add-hook 'lispy-mode-hook 'sk/diminish-lispy)
(add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))

;; Common lisp
(sk/require-package 'slime)
(defun sk/diminish-slime ()
  (interactive)
  (diminish 'slime-mode ""))
(add-hook 'slime-mode-hook 'sk/diminish-slime)

;; Elisp hydra
(defhydra sk/hydra-for-elisp (:color red
                              :hint nil)
  "
 ^Send^     | ^Menu^
 ^^^^^^^^^-- ------|--------------
 _f_unction | _L_ang  e_x_ecute
 _e_xp      | _H_ome  _q_uit
 _r_egion   |
 _b_uffer   |
"
  ("f" eval-defun)
  ("e" eval-expression)
  ("r" eval-region)
  ("b" eval-buffer)
  ("L" hydra-langs/body :exit t)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("x" counsel-M-x :color blue)
  ("q" nil :color blue))

(provide 'sk-lisp)

;;; sk-lisp.el ends here
