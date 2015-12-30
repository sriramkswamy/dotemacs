;; Magit
(sk/require-package 'magit)

;; Hydra for blame
(defhydra sk/hydra-git-blame (:color red
                              :hint nil)
  "
 ^Blame^  | ^Commit^     | ^Same commit^ | ^Heading^  | ^Hash-copy^ | ^Menu^
^^^^^^^^^----------|----------|-------------|----------|-----------|--------------
 _b_lame  | _j_ next     | _l_ next      | _t_oggle   | cop_y_      | _G_it   e_x_ecute
        | _k_ previous | _h_ previous  |          |           | _H_ome  _q_uit
"
  ("b" magit-blame)
  ("j" magit-blame-next-chunk)
  ("k" magit-blame-previous-chunk)
  ("l" magit-blame-next-chunk-same-commit)
  ("h" magit-blame-previous-chunk-same-commit)
  ("t" magit-blame-toggle-headings)
  ("y" magit-blame-copy-hash)
  ("G" sk/hydra-of-git/body :exit t)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("x" counsel-M-x :color blue)
  ("q" magit-blame-quit :color blue))

;; Diff-hl
(sk/require-package 'diff-hl)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
(add-hook 'prog-mode-hook 'diff-hl-mode)
(add-hook 'html-mode-hook 'diff-hl-mode)
(add-hook 'text-mode-hook 'diff-hl-mode)
(add-hook 'org-mode-hook 'diff-hl-mode)
(add-hook 'diff-hl-mode-hook 'diff-hl-margin-mode)
(add-hook 'diff-hl-mode-hook 'diff-hl-flydiff-mode)

;; Git time-machine
(sk/require-package 'git-timemachine)

;; Hydra for timemachine
(defhydra sk/hydra-git-timemachine (:color red
                                    :hint nil)
  "
 ^Timemachine^ | ^Navigate^            | ^Hash-copy^ | ^Menu^
^^^^^^^^^-------------|---------------------|-----------|---------------
 _s_tart       | _j_ next      _g_oto    | _b_rief     | _G_it   e_x_ecute
             | _k_ previous  _c_urrent | _f_ull      | _H_ome  _q_uit
"
  ("s" git-timemachine)
  ("j" git-time-machine-next-revision)
  ("k" git-time-machine-previous-revision)
  ("g" git-timemachine-nth-revision)
  ("c" git-timemachine-current-revision)
  ("b" git-timemachine-kill-abbreviated-revision)
  ("f" git-timemachine-kill-revision)
  ("G" sk/hydra-of-git/body :exit t)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("x" counsel-M-x :color blue)
  ("q" git-timemachine-quit :color blue))

;; Gist creation
(sk/require-package 'yagist)

;; Edit git config files
(sk/require-package 'gitconfig-mode)

;; Edit yaml files
(sk/require-package 'yaml-mode)

;; Hydra of git
(defhydra sk/hydra-of-git (:color red
                           :hint nil)
  "
 ^Git^                          | ^Diff^               | ^Gist^   | ^Menu^
^^^^^^^^^------------------------------|--------------------|--------|--------------
 _s_tatus   _b_lame   _t_imemachine | _j_ next      _g_oto   | _u_pload | _H_ome  e_x_ecute
                              | _k_ previous  _r_evert |        |       _q_uit
"
  ("s" magit-status :color blue)
  ("d" sk/hydra-diff/body :exit t)
  ("b" sk/hydra-git-blame/body :exit t)
  ("t" sk/hydra-git-timemachine/body :exit t)
  ("j" diff-hl-next-hunk)
  ("k" diff-hl-previous-hunk)
  ("g" diff-hl-goto-hunk)
  ("r" diff-hl-revert-hunk)
  ("u" yagist-region-or-buffer :color blue)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("x" counsel-M-x :color blue)
  ("q" nil :color blue))

(provide 'sk-vcs)

;;; sk-vcs.el ends here
