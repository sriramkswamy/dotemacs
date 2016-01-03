;; Improve dired
(sk/require-package 'dired+)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;; Which key
(sk/require-package 'which-key)
(add-hook 'which-key-mode-hook 'which-key-setup-side-window-bottom)
(defun sk/diminish-which-key ()
  (interactive)
  (diminish 'which-key-mode ""))
(add-hook 'which-key-mode-hook 'sk/diminish-which-key)

;; OS X finder
(sk/require-package 'reveal-in-osx-finder)

;; Undo tree
(sk/require-package 'undo-tree)
(defun sk/diminish-undo-tree ()
  (interactive)
  (diminish 'undo-tree-mode ""))
(add-hook 'undo-tree-mode-hook 'sk/diminish-undo-tree)
(defun sk/start-undo-tree ()
  (interactive)
  (undo-tree-mode 1))
(add-hook 'after-init-hook 'sk/start-undo-tree)

;; Neotree
(sk/require-package 'neotree)

;; Find file in project
(sk/require-package 'find-file-in-project)

;; Dash documentation
(sk/require-package 'dash-at-point)

;;; GTags
(sk/require-package 'ggtags)
(defun sk/diminish-ggtags ()
  (interactive)
  (diminish 'ggtags-mode ""))
(add-hook 'ggtags-mode-hook 'sk/diminish-ggtags)
(add-hook 'prog-mode-hook 'sk/diminish-ggtags)
(add-hook 'text-mode-hook 'sk/diminish-ggtags)
(add-hook 'prog-mode-hook 'ggtags-mode)

;; Add exec-path for Gtags
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/Cellar/global/6.5/bin"))
(setq exec-path (append exec-path '("/usr/local/Cellar/global/6.5/bin")))
(setq-local imenu-create-index-function #'ggtags-build-imenu-index)

;; Back button mode
(sk/require-package 'back-button)
(defun sk/diminish-back-button ()
  (interactive)
  (diminish 'back-button-mode ""))
(add-hook 'back-button-mode-hook 'sk/diminish-back-button)
(add-hook 'after-init-hook 'back-button-mode)

;; Hydra tags
(defhydra sk/hydra-tags (:color blue
                         :hint nil)
  "
 ^Tags^    | ^Jump^      |  ^Menu^
 ^^^^^^^^^--------|-----------|---------
 _c_reate  | _r_eference |  _H_ome
 _u_pdate  | _t_ag       |  e_x_ecute
 _f_ind    |           |  _q_uit
"
  ("c" ggtags-create-tags)
  ("u" ggtags-update-tags)
  ("f" ggtags-find-tag-regexp)
  ("r" ggtags-find-reference :color red)
  ("t" ggtags-find-tag-dwim :color red)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("x" counsel-M-x :color blue)
  ("q" nil :color blue))

;; Apropos
(defhydra sk/hydra-apropos (:color blue
                            :hint nil)
  "
 ^Apropos - Search anything^    | ^Menu^
 ^^^^^^^^^-----------------------------|--------------
 _a_ll   _d_oc   _v_ar   _c_md   _l_ib  | _H_ome  _q_uit
 _u_ser  valu_e_                  | _h_elp  e_x_ecute
"
  ("a" apropos)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("d" apropos-documentation)
  ("v" apropos-variable)
  ("c" apropos-command)
  ("l" apropos-library)
  ("u" apropos-user-option)
  ("e" apropos-value)
  ("h" sk/hydra-help/body :exit t)
  ("x" counsel-M-x :color blue)
  ("q" nil :color blue))

;; Help
(defhydra sk/hydra-of-help (:color blue
                            :hint nil)
  "
 ^Help^                                   | ^Menu^
 ^^^^^^^^^---------------------------------------|---------
 _b_inding    _i_nfo     _t_utorial  _a_ll      | _H_ome
 _f_unction   _s_ymbol   _p_ackage   _l_ang-env | e_x_ecute
 _v_ariable   _e_macs    _h_elp               | _q_uit
 _m_ode       synta_x_   _k_ey                |
"
  ("b" describe-bindings)
  ("f" describe-function)
  ("v" describe-variable)
  ("m" describe-mode)
  ("i" info)
  ("s" info-lookup-symbol)
  ("e" info-emacs-manual)
  ("x" describe-syntax)
  ("t" help-with-tutorial)
  ("p" describe-package)
  ("h" help-for-help)
  ("k" describe-key)
  ("a" sk/hydra-apropos/body :exit t)
  ("l" describe-language-environment)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("x" counsel-M-x :color blue)
  ("q" nil :color blue))

;; Bookmarks - hydra
(defhydra sk/hydra-bookmarks (:color red
                              :hint nil)
  "
 ^Bookmarks^                   | ^Menu^
 ^^^^^^^^^----------------------------|-----------------------
 _s_et  _b_ookmark  _j_ump  _d_elete | _H_ome   e_x_ecute   _q_uit
  "
  ("s" bookmark-set)
  ("b" bookmark-save)
  ("j" bookmark-jump)
  ("d" bookmark-delete)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("x" counsel-M-x :color blue)
  ("q" nil :color blue))

;; Hydra of Navigation
(defhydra sk/hydra-of-navigation (:color blue
                                  :hint nil)
  "
 ^Files^     | ^Buffers^   | ^Project^   | ^Snippets^ | ^Menu^
 ^^^^^^^^^----------|-----------|-----------|----------|--------
 _f_iles     | _b_uffers   | _p_roject   | _s_nippets | _H_ome
 _r_ecent    | book_m_arks | _a_lternate | _c_reate   | e_x_ecute
 read-_o_nly | _i_ndex     | _t_ags      |          | _q_uit
 _n_eotree   |           |           |          |
 _d_esktop   |           |           |          |
  "
  ("b" switch-to-buffer)
  ("f" find-file)
  ("r" ivy-recentf)
  ("o" find-file-literally)
  ("n" neotree-toggle)
  ("d" reveal-in-osx-finder)
  ("m" sk/hydra-bookmarks/body :exit t)
  ("i" imenu)
  ("p" find-file-in-project)
  ("a" ff-find-other-file)
  ("t" sk/hydra-tags/body :exit t)
  ("s" yas-insert-snippet)
  ("c" yas-new-snippet)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("x" counsel-M-x :color blue)
  ("q" nil :color blue))

;; Eyebrowse mode
(sk/require-package 'eyebrowse)
(setq eyebrowse-wrap-around t
      eyebrowse-switch-back-and-forth t)
(defun sk/diminish-eyebrowse ()
  (interactive)
  (require 'eyebrowse)
  (diminish 'eyebrowse-mode ""))
(add-hook 'eyebrowse-mode-hook 'sk/diminish-eyebrowse)
(add-hook 'prog-mode-hook 'eyebrowse-mode)
(add-hook 'text-mode-hook 'eyebrowse-mode)

;; Hydra for eyebrowse
(defhydra sk/hydra-for-eyebrowse (:color blue
                                  :hint nil)
  "
 ^Eyebrowse^                | ^Menu^
 ^^^^^^^^^-------------------------|--------------
 _0_   _4_  _8_   sw_i_tch   _l_ast | _H_ome  e_x_ecute
 _1_   _5_  _9_   _r_ename        | _W_in   _q_uit
 _2_   _6_      _n_ext          |
 _3_   _7_      _p_revious      |
"
  ("0" eyebrowse-switch-to-window-config-0)
  ("1" eyebrowse-switch-to-window-config-1)
  ("2" eyebrowse-switch-to-window-config-2)
  ("3" eyebrowse-switch-to-window-config-3)
  ("4" eyebrowse-switch-to-window-config-4)
  ("5" eyebrowse-switch-to-window-config-5)
  ("6" eyebrowse-switch-to-window-config-6)
  ("7" eyebrowse-switch-to-window-config-7)
  ("8" eyebrowse-switch-to-window-config-8)
  ("9" eyebrowse-switch-to-window-config-9)
  ("i" eyebrowse-switch-to-window-config)
  ("r" eyebrowse-rename-window-config)
  ("n" eyebrowse-next-window-config :color red)
  ("p" eyebrowse-prev-window-config :color red)
  ("l" eyebrowse-last-window-config :color red)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("W" sk/hydra-of-windows/body :exit t)
  ("x" counsel-M-x :color blue)
  ("q" nil :color blue))

(provide 'sk-navigate)

;;; sk-navigate.el ends here
