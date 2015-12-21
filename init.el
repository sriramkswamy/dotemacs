;;;; Optimization

;;; Garbage collector - increase threshold
(setq gc-cons-threshold 100000000)

;;; Wrap init file
(let ((file-name-handler-alist nil)) "~/.emacs.d/init.el")

;;;; Packages

;;; Install
(require 'package)
(setq package-enable-at-startup nil)

;; packages based on versions
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t))
  (package-initialize)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;;; Add homebrew packages
(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;;; Define function for a required package
(defun sk/require-package (package)
  (setq-default highlight-tabs t)
  "Install given PACKAGE."
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

;;;; OSX stuff

;;; Mac modifiers
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta))

;;; Get the proper path
(sk/require-package 'exec-path-from-shell)
(exec-path-from-shell-copy-env "PYTHONPATH")
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;;;; Basic settings

;;; Diminish minor modes

;; Eldoc
(sk/require-package 'diminish)
(defun sk/diminish-eldoc ()
  (interactive)
  (diminish 'eldoc-mode ""))
(add-hook 'eldoc-mode-hook 'sk/diminish-eldoc)

;; Diminish abbrev
(defun sk/diminish-abbrev ()
  (interactive)
  (diminish 'abbrev-mode ""))
(add-hook 'abbrev-mode-hook 'sk/diminish-abbrev)
(add-hook 'prog-mode-hook 'sk/diminish-abbrev)
(add-hook 'text-mode-hook 'sk/diminish-abbrev)

;;; Emacs default changes

;; No welcome screen
(setq inhibit-startup-message t
      initial-scratch-message ";; Scratch"
      initial-major-mode 'fundamental-mode
      visible-bell nil
      inhibit-splash-screen t)

;; Enable winner-mode
(add-hook 'after-init-hook #'winner-mode)

;; Narrow to region
(put 'narrow-to-region 'disabled nil)

;; Enable recentf mode
(recentf-mode)

;; Column number mode
(column-number-mode)

;; Which function mode
(which-function-mode 1)

;; DocView Settings
(setq doc-view-continuous t
      doc-view-resolution 200)

;; Don't blink the cursor
(blink-cursor-mode -1)

;; Hide/Show mode
(defun sk/diminish-hs-minor ()
  (interactive)
  (diminish 'hs-minor-mode ""))
(add-hook 'hs-minor-mode-hook 'sk/diminish-hs-minor)
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; Enable subword mode
(defun sk/diminish-subword ()
  (interactive)
  (diminish 'subword-mode ""))
(add-hook 'subword-mode-hook 'sk/diminish-subword)
(global-subword-mode)

;; Highlight the cursor line
(global-hl-line-mode 1)

;; Backups at .saves folder in the current folder
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;;; Non-native fullscreen
(setq ns-use-native-fullscreen nil)
(defun sk/toggle-frame-fullscreen-non-native ()
  "Toggle full screen non-natively. Uses the `fullboth' frame paramerter
   rather than `fullscreen'. Useful to fullscreen on OSX w/o animations."
  (interactive)
  (modify-frame-parameters
   nil
   `((maximized
      . ,(unless (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
           (frame-parameter nil 'fullscreen)))
     (fullscreen
      . ,(if (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
             (if (eq (frame-parameter nil 'maximized) 'maximized)
                 'maximized)
           'fullboth)))))

;;;; Must have packages

;;; Evil - Vim emulation layer
(sk/require-package 'evil)
(setq evil-default-cursor t
      evil-want-C-u-scroll t
      evil-want-Y-yank-to-eol t)
(evil-mode 1)

;;; Paradox for package list
(sk/require-package 'paradox)
(setq paradox-github-token t)
(evil-set-initial-state 'paradox-menu-mode 'emacs)

;;; Improve dired
(sk/require-package 'dired+)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(evil-set-initial-state 'dired-mode 'emacs)

;;; Hydra
(sk/require-package 'hydra)

;; Split windows and move
(defun sk/split-below-and-move ()
  (interactive)
  (split-window-below)
  (other-window 1))
(defun sk/split-right-and-move ()
  (interactive)
  (split-window-right)
  (other-window 1))

;; Hydra of windows
(defhydra sk/hydra-of-windows (:color pink
                               :hint nil)
  "
 ^Move^   | ^Size^   | ^Change^        | ^Split^        | ^Frame^                | ^Text^       | ^Config^   | ^Menu^
 ^^^^^^^^^^^-------|--------|---------------|--------------|----------------------|------------|----------|--------------
 ^ ^ _k_ ^ ^  | ^ ^ _{_ ^ ^  | _u_ winner-undo | _v_ vertical   | _f_ullscreen  _m_aximize | _+_ zoom in  | _I_ config | _H_ome  e_x_ecute
 _h_ ^+^ _l_  | _<_ ^+^ _>_  | _r_ winner-redo | _s_ horizontal | _d_elete      m_i_nimize | _-_ zoom out |          | mo_V_e  _q_uit
 ^ ^ _j_ ^ ^  | ^ ^ _}_ ^ ^  | _c_lose         | _z_oom         | s_e_lect      _n_ame     |            |          |
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
  ("i" suspend-frame)
  ("d" delete-frame)
  ("e" select-frame-by-name)
  ("n" set-frame-name)
  ("+" text-scale-increase)
  ("-" text-scale-decrease)
  ("I" sk/hydra-for-eyebrowse/body :exit t)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("V" sk/hydra-of-motion/body :exit t)
  ("x" counsel-M-x :color blue)
  ("q" nil :color blue))

;; Evil maps for hydra of windows
(define-key evil-normal-state-map (kbd "w") 'sk/hydra-of-windows/body)

;;; Avy
(sk/require-package 'avy)

;; From Sacha Chua's blog
(defun sk/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))
  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'sk/smarter-move-beginning-of-line)
(defun marker-is-point-p (marker)
  "test if marker is current point"
  (and (eq (marker-buffer marker) (current-buffer))
       (= (marker-position marker) (point))))

;; Hydra of motion
(defhydra sk/hydra-of-motion (:color pink
                              :hint nil)
  "
 ^Move^   | ^Goto^    | ^Line^    | ^File^  | ^Screen^   | ^Para^   | ^Word^     | ^Edit^ | ^Menu^
 ^^^^^^-------|---------|---------|-------|----------|--------|----------|------|--------------
 ^ ^ _k_ ^ ^  | _c_har    | _a_ start | _<_ beg | _r_ecenter | _}_ next | for_w_ard  | _M_ark | _H_ome  _q_uit
 _h_ ^+^ _l_  | _s_ubword | _e_nd     | _>_ end | _]_ next   | _{_ prev | _b_ackward | _E_dit | _L_ang  e_x_ecute
 ^ ^ _j_ ^ ^  |         | _g_oto    |       | _[_ prev   |        |          |      | _W_in   _i_nsert
"
  ("h" backward-char)
  ("j" next-line)
  ("k" previous-line)
  ("l" forward-char)
  ("a" move-beginning-of-line)
  ("e" move-end-of-line)
  ("g" avy-goto-line)
  ("c" avy-goto-char)
  ("s" avy-goto-word-or-subword-1)
  ("r" recenter-top-bottom)
  ("]" scroll-up)
  ("[" scroll-down)
  ("<" beginning-of-buffer)
  (">" end-of-buffer)
  ("}" forward-paragraph)
  ("{" backward-paragraph)
  ("w" forward-word)
  ("b" backward-word)
  ("M" sk/hydra-of-marks/body :exit t)
  ("E" sk/hydra-of-edits/body :exit t)
  ("L" sk/hydra-of-langs/body :exit t)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("W" sk/hydra-of-windows/body :exit t)
  ("i" nil :color blue)
  ("x" counsel-M-x :color blue)
  ("q" nil :color blue))

;;; Undo-tree
(sk/require-package 'undo-tree)
(defun sk/diminish-undo-tree ()
  (interactive)
  (diminish 'undo-tree-mode ""))
(add-hook 'undo-tree-mode-hook 'sk/diminish-undo-tree)

;; Hydra of undo
(defhydra sk/hydra-of-undo (:color pink
                            :hint nil)
  "
 ^Undo^             | ^Menu^
 ^^^^^^^^^-----------------|---------------------
 _u_ndo  _r_edo  _v_iew | _H_ome   e_x_ecute  _q_uit
"
  ("u" undo-tree-undo)
  ("r" undo-tree-redo)
  ("v" undo-tree-visualize :color blue)
  ("E" sk/hydra-of-edits/body :exit t)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("x" counsel-M-x :color blue)
  ("q" nil :color blue))

;; Evil maps for undo-tree
(define-key evil-normal-state-map (kbd "U") 'undo-tree-visualize)

;;; Highlight stuff
(sk/require-package 'volatile-highlights)
(require 'volatile-highlights)
(defun sk/diminish-volatile-highlights ()
  (interactive)
  (diminish 'volatile-highlights-mode ""))
(add-hook 'volatile-highlights-mode-hook 'sk/diminish-volatile-highlights)
(volatile-highlights-mode t)

;;; Smart tabs
(sk/require-package 'smart-tab)
(defun sk/diminish-smart-tab ()
  (interactive)
  (diminish 'smart-tab-mode ""))
(add-hook 'smart-tab-mode-hook 'sk/diminish-smart-tab)
(global-smart-tab-mode)

;;; Enable smartparens-mode
(sk/require-package 'smartparens)
(defun sk/diminish-smartparens ()
  (interactive)
  (diminish 'smartparens-mode ""))
(add-hook 'smartparens-mode-hook 'sk/diminish-smartparens)
(smartparens-global-mode)

;;; Delete trailing whitespace on save
(sk/require-package 'ws-butler)
(defun sk/diminish-ws-butler ()
  (interactive)
  (diminish 'ws-butler-mode ""))
(add-hook 'ws-butler-mode-hook 'sk/diminish-ws-butler)
(ws-butler-global-mode)

;;; Region information
(sk/require-package 'region-state)
(region-state-mode)

;;; Restart Emacs from Emacs
(sk/require-package 'restart-emacs)

;;; Don't lose the cursor
(sk/require-package 'beacon)
(defun sk/diminish-beacon ()
  (interactive)
  (diminish 'beacon-mode ""))
(add-hook 'beacon-mode-hook 'sk/diminish-beacon)
(beacon-mode 1)

;;; Mark ring
(sk/require-package 'back-button)
(defun sk/diminish-back-button ()
  (interactive)
  (diminish 'back-button-mode ""))
(add-hook 'back-button-mode-hook 'sk/diminish-back-button)
(add-hook 'prog-mode-hook 'back-button-mode)
(add-hook 'org-mode-hook 'back-button-mode)

;;; Which key
(sk/require-package 'which-key)
(which-key-setup-side-window-bottom)
(defun diminish-which-key ()
  (interactive)
  (diminish 'which-key-mode ""))
(add-hook 'which-key-mode-hook 'diminish-which-key)

;;; Swiper (with Ivy and counsel)
(sk/require-package 'swiper)
(sk/require-package 'counsel)
(setq ivy-display-style 'fancy
      ivy-height 15
      counsel-yank-pop-truncate t)

;; Fuzzy for M-x
(setq ivy-re-builders-alist
      '((counsel-M-x . ivy--regex-fuzzy)
        (t . ivy--regex-plus)))
(defun sk/diminish-ivy ()
  (interactive)
  (diminish 'ivy-mode ""))
(add-hook 'ivy-mode-hook 'sk/diminish-ivy)
(ivy-mode 1)

;; Evil maps for Swiper, Ivy and Counsel
(define-key evil-normal-state-map (kbd "SPC d") 'counsel-M-x)
(define-key evil-normal-state-map (kbd "SPC SPC") 'swiper)
(define-key evil-normal-state-map (kbd "SPC b") 'swiper-all)
(define-key evil-normal-state-map (kbd "SPC r") 'ivy-recentf)
(define-key evil-normal-state-map (kbd "SPC u") 'ivy-switch-buffer)
(define-key evil-normal-state-map (kbd "SPC y") 'counsel-yank-pop)
(define-key evil-normal-state-map (kbd "SPC v") 'counsel-load-theme)
(define-key evil-normal-state-map (kbd "SPC .") 'ivy-resume)
(define-key evil-normal-state-map (kbd "SPC /") 'counsel-locate)
(define-key evil-normal-state-map (kbd "SPC e") 'counsel-ag)
(define-key evil-visual-state-map (kbd "SPC e") 'counsel-ag)
(define-key evil-visual-state-map (kbd "SPC d") 'counsel-M-x)
(define-key evil-insert-state-map (kbd "C-k") 'counsel-unicode-char)
(define-key evil-insert-state-map (kbd "C-d") 'ispell-word)
(define-key evil-insert-state-map (kbd "C-l") 'counsel-M-x)

;;; Swoop
(sk/require-package 'swoop)

;; Evil maps for swoop
(define-key evil-normal-state-map (kbd "*") 'swoop-pcre-regexp)
(define-key evil-normal-state-map (kbd "#") 'swoop-pcre-regexp)
(define-key evil-visual-state-map (kbd "*") 'swoop-pcre-regexp)
(define-key evil-visual-state-map (kbd "#") 'swoop-pcre-regexp)

;;; Find file in project
(sk/require-package 'find-file-in-project)

;; Evil maps for ffip
(define-key evil-normal-state-map (kbd "SPC p") 'find-file-in-project)
(define-key evil-normal-state-map (kbd "SPC TAB") 'ff-find-other-file)

;;; Visual regexp
(sk/require-package 'visual-regexp)
(sk/require-package 'visual-regexp-steroids)

;; Evil maps for visual replace
(define-key evil-normal-state-map (kbd "SPC 5") 'vr/query-replace)
(define-key evil-visual-state-map (kbd "SPC 5") 'vr/query-replace)

;;; Expand regions
(sk/require-package 'expand-region)

;; Open line above
(defun sk/open-line-above ()
  "Insert a newline above the current line and put point at beginning."
  (interactive)
  (unless (bolp)
    (beginning-of-line))
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

;; Open line below
(defun sk/open-line-below ()
  "Insert a newline below the current line and put point at beginning."
  (interactive)
  (unless (eolp)
    (end-of-line))
  (newline-and-indent))

;; Join next line with current line
(defun sk/join-line ()
  "Join the current line with the next line"
  (interactive)
  (next-line)
  (delete-indentation))

;; Move lines - from stack overflow
(defun sk/move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg)
          (when (and (eval-when-compile
                       '(and (>= emacs-major-version 24)
                             (>= emacs-minor-version 3)))
                     (< arg 0))
            (forward-line -1)))
        (forward-line -1))
      (move-to-column column t)))))
(defun sk/move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (sk/move-text-internal arg))
(defun sk/move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (sk/move-text-internal (- arg)))

;; Inserting blank lines above and below - should do some mark trickery
(defun sk/blank-line-up ()
  (interactive)
  (move-beginning-of-line nil)
  (newline))
(defun sk/blank-line-down ()
  (interactive)
  (move-end-of-line nil)
  (newline)
  (forward-line -1))

;; Hydra of edits
(defhydra sk/hydra-of-edits (:pre (require 'expand-region)
                             :color pink
                             :hint nil)
  "
 ^Line^  | ^Blank^  | ^Move^   | ^Select^                    | ^Lang^        | ^Edit^                               | ^Menu^
 ^^^^^^^------|--------|--------|---------------------------|-------------|------------------------------------|----------------------
 _a_bove | _[_ up   | _{_ up   | _i_ncrease _p_ara  _o_ in()     | _b_lock-py    | _d_el-region         _K_ill-whole-line | _M_ark   _H_ome   e_x_ecute
 b_e_low | _]_ down | _}_ down | _r_educe   _f_unc  q_u_otes     | _h_ doc-py    | _k_ill-rest-of-line                  | mo_V_e   _L_ang   _q_uit
 _j_oin  |        |        | _s_nippet  _w_ord  _-_ org-code | _l_ line-py   | cop_y_                               | _P_ython
 _S_plit |        |        |          la_t_ex comme_n_t    | _m_ julia-fun | _c_omment                            | _J_ulia
  "
  ("a" sk/open-line-above :color blue)
  ("e" sk/open-line-below :color blue)
  ("j" sk/join-line)
  ("S" electric-newline-and-maybe-indent)
  ("[" sk/blank-line-up)
  ("]" sk/blank-line-down)
  ("{" sk/move-text-up)
  ("}" sk/move-text-down)
  ("i" er/expand-region)
  ("r" er/contract-region)
  ("p" er/mark-paragraph)
  ("f" er/mark-defun)
  ("w" er/mark-symbol)
  ("t" er/mark-LaTeX-math)
  ("n" er/mark-comment)
  ("o" er/mark-inside-pairs)
  ("u" er/mark-inside-quotes)
  ("-" er/mark-org-code-block)
  ("b" er/mark-python-block)
  ("h" er/mark-python-string)
  ("l" er/mark-python-statement)
  ("m" er/mark-ruby-block-up)
  ("d" delete-region)
  ("K" kill-whole-line)
  ("k" kill-line)
  ("y" kill-ring-save)
  ("c" comment-dwim)
  ("s" yas-insert-snippet :color blue)
  ("P" sk/hydra-for-python/body :exit t)
  ("J" sk/hydra-for-julia/body :exit t)
  ("L" sk/hydra-of-langs/body :exit t)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("V" sk/hydra-of-motion/body :exit t)
  ("M" sk/hydra-of-marks/body :exit t)
  ("x" counsel-M-x :color blue)
  ("q" nil :color blue))

;; Evil maps for edit and expand regions
(define-key evil-normal-state-map (kbd "ge") 'sk/hydra-of-edits/body)
(define-key evil-visual-state-map (kbd "ge") 'sk/hydra-of-edits/body)
(define-key evil-normal-state-map (kbd "]x") 'sk/move-text-down)
(define-key evil-normal-state-map (kbd "[x") 'sk/move-text-up)
(define-key evil-normal-state-map (kbd "]n") 'sk/blank-line-down)
(define-key evil-normal-state-map (kbd "[n") 'sk/blank-line-up)

;;; Multiple cursors
(sk/require-package 'multiple-cursors)

;; Hydra for multiple-cursors
(defhydra sk/hydra-of-multiple-cursors (:pre (require 'multiple-cursors)
                                        :color pink
                                        :hint nil)
  "
^Mark^       | ^Unmark^   | ^Lines^                     | ^Menu^
^^^^^^^^^^^^-----------|----------|---------------------------|---------
^ ^ _k_ ^ ^  _a_ll | _p_revious | _c_hange  _#_ numbers  _c_hange | _H_ome
_h_ ^+^ _l_      | _n_ext     | _a_ppend  le_t_ters           | e_x_ecute
^ ^ _j_ ^ ^      |          | _i_nsert  _s_top              | _q_uit
"
  ("j" mc/mark-next-like-this)
  ("k" mc/mark-previous-like-this)
  ("h" mc/skip-to-next-like-this)
  ("l" mc/skip-to-previous-like-this)
  ("a" mc/mark-all-like-this)
  ("p" mc/unmark-previous-like-this)
  ("n" mc/unmark-next-like-this)
  ("c" mc/edit-lines :color blue)
  ("i" mc/edit-beginnings-of-lines :color blue)
  ("e" mc/edit-ends-of-lines :color blue)
  ("#" mc/insert-numbers :color blue)
  ("t" mc/insert-letters :color blue)
  ("s" mc/keyboard-quit)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("x" counsel-M-x :color blue)
  ("q" nil :color blue))
(define-key evil-normal-state-map (kbd "gm") 'sk/hydra-of-multiple-cursors/body)
(define-key evil-visual-state-map (kbd "gm") 'sk/hydra-of-multiple-cursors/body)

;;; ag and wgrep-ag
(sk/require-package 'ag)
(sk/require-package 'wgrep-ag)
(evil-set-initial-state 'ag-mode 'normal)

;; Evil maps for ag
(define-key evil-normal-state-map (kbd "W") 'wgrep-change-to-wgrep-mode)
(define-key evil-normal-state-map (kbd "S") 'wgrep-finish-edit)
(define-key evil-normal-state-map (kbd "R") 'wgrep-remove-all-change)
(define-key evil-normal-state-map (kbd "SPC 7") 'ag-project-regexp)
(define-key evil-visual-state-map (kbd "SPC 7") 'ag-project-regexp)

;;; GTags
(sk/require-package 'ggtags)
(defun sk/diminish-ggtags ()
  (interactive)
  (diminish 'ggtags-mode ""))
(add-hook 'ggtags-mode-hook 'sk/diminish-ggtags)
(add-hook 'prog-mode-hook 'ggtags-mode)

;; Add exec-path for Gtags
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/Cellar/global/6.5/bin"))
(setq exec-path (append exec-path '("/usr/local/Cellar/global/6.5/bin")))
(setq-local imenu-create-index-function #'ggtags-build-imenu-index)

;; Hydra of tags
(defhydra sk/hydra-of-tags (:color blue
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
  ("r" ggtags-find-reference)
  ("t" ggtags-find-tag-dwim)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("x" counsel-M-x :color blue)
  ("q" nil :color blue))

;; Evil maps
(define-key evil-normal-state-map (kbd "T") 'sk/hydra-of-tags/body)

;;; Spotlight
(sk/require-package 'spotlight)

;; Evil maps for spotlight
(define-key evil-normal-state-map (kbd "SPC 8") 'spotlight)

;;; Reveal in Finder
(sk/require-package 'reveal-in-osx-finder)

;; Evil maps for reveal in finder
(define-key evil-normal-state-map (kbd "gF") 'reveal-in-osx-finder)

;;; Google
(sk/require-package 'google-this)

;; Evil maps for Google
(define-key evil-normal-state-map (kbd "SPC 9") 'google-this-search)
(define-key evil-visual-state-map (kbd "SPC 9") 'google-this)

;;;; Help

;;; Hydras

;; Hydra for apropos
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

;; Hydra for help
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

;; Evil Maps for help hydra
(define-key evil-normal-state-map (kbd "SPC x") 'sk/hydra-of-help/body)

;;;; Support some emacs stuff

;;; Marks

;; Rectangle marks
(defhydra sk/hydra-rectangle (:color pink
                              :hint nil)
 "
 ^Move^   | ^Edit^           | ^Menu^
 ^^^^^^-------|----------------|--------------
 ^ ^ _k_ ^ ^  | _s_et       _r_eset | _H_ome  _q_uit
 _h_ ^+^ _l_  | ex_c_hange  cop_y_  | _M_ark  e_x_ecute
 ^ ^ _j_ ^ ^  | _d_elete    _p_aste |       _i_nsert
"
  ("h" backward-char nil)
  ("l" forward-char nil)
  ("k" previous-line nil)
  ("j" next-line nil)
  ("s" (rectangle-mark-mode 1))
  ("c" exchange-point-and-mark nil)
  ("y" copy-rectangle-as-kill nil)
  ("d" kill-rectangle nil)
  ("r" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)) nil)
  ("p" yank-rectangle nil)
  ("M" sk/hydra-of-marks/body :exit t)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("i" nil :color blue)
  ("x" counsel-M-x :color blue)
  ("q" nil :color blue))

;; Hydra of marks
(defhydra sk/hydra-of-marks (:color pink
                             :hint nil)
  "
 ^Move^   | ^Line^    | ^File^  | ^Mark^     | ^Para^   | ^Word^     | ^Edit^      | ^Menu^
 ^^^^^^-------|---------|-------|----------|--------|----------|-----------|----------------
 ^ ^ _k_ ^ ^  | _a_ start | _<_ beg | _s_et      | _}_ next | for_w_ard  | _d_el       | _P_ython _H_ome
 _h_ ^+^ _l_  | _e_nd     | _>_ end | _r_eset    | _{_ prev | _b_ackward | cop_y_      | _J_ulia  e_x_ecute
 ^ ^ _j_ ^ ^  | _g_oto    |       | ex_c_hange |        |          | _R_ectangle | _L_ang   _q_uit
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

;;; Bookmarks
(defhydra sk/hydra-bookmarks (:color pink
                              :hint nil)
  "
 ^Bookmarks^                   | ^Menu^
 ^^^^^^^^^----------------------------|-----------------------
 _s_et  _b_ookmark  _j_ump  _d_elete | _H_ead   e_x_ecute   _q_uit
  "
  ("s" bookmark-set)
  ("b" bookmark-save)
  ("j" bookmark-jump)
  ("d" bookmark-delete)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("x" counsel-M-x :color blue)
  ("q" nil :color blue))

;;;; Version control

;;; Magit
(sk/require-package 'magit)

;; Evil magit
(sk/require-package 'evil-magit)
(defun sk/magit-hook ()
  (interactive)
  (setq evil-magit-state 'motion)
  (require 'evil-magit))
(add-hook 'magit-mode-hook 'sk/magit-hook)

;; Evil maps for Magit
(define-key evil-normal-state-map (kbd "SPC g") 'magit-status)

;; Hydra for blame
(defhydra sk/hydra-git-blame (:color pink
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

;; Evil maps for blame
(define-key evil-normal-state-map (kbd "gb") 'sk/hydra-git-blame/body)

;;; Diff-hl
(sk/require-package 'diff-hl)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
(add-hook 'prog-mode-hook 'diff-hl-mode)
(add-hook 'html-mode-hook 'diff-hl-mode)
(add-hook 'text-mode-hook 'diff-hl-mode)
(add-hook 'org-mode-hook 'diff-hl-mode)
(add-hook 'diff-hl-mode-hook 'diff-hl-margin-mode)
(add-hook 'diff-hl-mode-hook 'diff-hl-flydiff-mode)

;; Evil maps for diff-hl
(evil-set-initial-state 'diff-mode 'emacs)
(define-key evil-normal-state-map (kbd "]d") 'diff-hl-next-hunk)
(define-key evil-normal-state-map (kbd "[d") 'diff-hl-previous-hunk)
(define-key evil-normal-state-map (kbd "gh") 'diff-hl-diff-goto-hunk)
(define-key evil-normal-state-map (kbd "gr") 'diff-hl-revert-hunk)

;;; Git time-machine
(sk/require-package 'git-timemachine)

;; Hydra for timemachine
(defhydra sk/hydra-git-timemachine (:color pink
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

;; Evil maps for time machine
(define-key evil-normal-state-map (kbd "gt") 'hydra-git-timemachine/body)

;;; Gists
(sk/require-package 'yagist)
(setq yagist-view-gist t)

;;; YAML mode
(sk/require-package 'yaml-mode)

;;; Editing my gitconfig
(sk/require-package 'gitconfig-mode)

;;; Hydra of git
(defhydra sk/hydra-of-git (:color pink
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

;; Evil maps for hydra of git
(define-key evil-normal-state-map (kbd "gt") 'hydra-git-timemachine/body)

;;;; REPL and Terminal

;;; Make the compilation window automatically disapper from enberg on #emacs
(setq compilation-finish-functions
      (lambda (buf str)
        (if (null (string-match ".*exited abnormally.*" str))
            ;;no errors, make the compilation window go away in a few seconds
            (progn
              (run-at-time
               "2 sec" nil 'delete-windows-on
               (get-buffer-create "*compilation*"))
              (message "No Compilation Errors!")))))

;;; Eshell
(setq eshell-glob-case-insensitive t
      eshell-scroll-to-bottom-on-input 'this
      eshell-buffer-shorthand t
      eshell-history-size 1024
      eshell-cmpl-ignore-case t
      eshell-last-dir-ring-size 512)
(add-hook 'shell-mode-hook 'goto-address-mode)

;; Aliases
(setq eshell-aliases-file (concat user-emacs-directory ".eshell-aliases"))

;; Vertical split eshell
(defun sk/eshell-vertical ()
  "opens up a new shell in the directory associated with the current buffer's file."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (name (car (last (split-string parent "/" t)))))
    (split-window-right)
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))
    (eshell-send-input)))

;; Horizontal split eshell
(defun sk/eshell-horizontal ()
  "opens up a new shell in the directory associated with the current buffer's file."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (name (car (last (split-string parent "/" t)))))
    (split-window-below)
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))
    (eshell-send-input)))

;; Evil maps for eshell
(define-key evil-normal-state-map (kbd "+") 'sk/eshell-vertical)

;;; Multi-term
(sk/require-package 'multi-term)

;; Vertical split multi-term
(defun sk/multi-term-vertical ()
  "opens up a new terminal in the directory associated with the current buffer's file."
  (interactive)
  (split-window-right)
  (other-window 1)
  (multi-term))

;; Horizonal split multi-term
(defun sk/multi-term-horizontal ()
  "opens up a new terminal in the directory associated with the current buffer's file."
  (interactive)
  (split-window-below)
  (other-window 1)
  (multi-term))

;; Evil maps for multi-term
(define-key evil-normal-state-map (kbd "-") 'sk/multi-term-horizontal)

;;; Tmux

;; Interact with Tmux
(sk/require-package 'emamux)

;; Hydra for emamux
(defhydra sk/hydra-for-emamux (:color blue
                              :hint nil)
  "
 ^Tmux^           | ^Menu^
 ^^^^^^^^^------------|-------------
 _s_end  _r_un    | _H_ome  e_x_ecute
 _l_ast  _c_lose  | _L_ang  _q_uit
"
  ("s" emamux:send-command)
  ("r" emamux:run-command)
  ("l" emamux:run-last-command)
  ("c" emamux:close-runner-pane)
  ("L" sk/hydra-of-langs/body :exit t)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("x" counsel-M-x :color blue)
  ("q" nil :color blue))

;;; Quickrun
(sk/require-package 'quickrun)

;;; Multi-compile
(sk/require-package 'multi-compile)
(setq multi-compile-alist '((c++-mode . (("cpp-omp" . "g++ %file-name -Wall -fopenmp -o -g %file-sans.out")
                                         ("cpp-mpi" . "mpic++ %file-name -o -g %file-sans.out")
                                         ("cpp-g++" . "g++ %file-name -o %file-sans.out")))
                            (c-mode . (("c-omp" . "gcc %file-name -Wall -fopenmp -o -g %file-sans.out")
                                       ("c-mpi" . "mpicc %file-name -o -g %file-sans.out")
                                       ("c-gcc" . "gcc %file-name -o %file-sans.out")))))
(setq multi-compile-completion-system 'default)

;;;; Auto completion

;;; Company

;; Flx with company
(sk/require-package 'flx)
(sk/require-package 'company-flx)
(sk/require-package 'company)
;; (with-eval-after-load 'company
;;   (company-flx-mode +1))
;; (eval-after-load 'company
;;   '(add-to-list 'company-backends 'company-files))
(setq company-idle-delay 0
      company-minimum-prefix-length 1
      company-require-match 0
      company-selection-wrap-around t
      company-dabbrev-downcase nil)

;; Company Maps
(global-set-key [(control return)] 'company-complete-common-or-cycle)
(defun sk/company-hook ()
  (interactive)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map [tab] 'company-complete-common-or-cycle))
(defun sk/diminish-company ()
   (interactive)
  (diminish 'company-mode " ς"))
(add-hook 'company-mode-hook 'sk/company-hook)
(add-hook 'company-mode-hook 'sk/diminish-company)
(add-hook 'prog-mode-hook 'company-mode)

;;;; Error checking

;;; Flycheck
(sk/require-package 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Evil maps for flycheck
(define-key evil-normal-state-map (kbd "]e") 'flycheck-next-error)
(define-key evil-normal-state-map (kbd "[e") 'flycheck-previous-error)
(define-key evil-normal-state-map (kbd "]E") 'flycheck-last-checker)
(define-key evil-normal-state-map (kbd "[E") 'flycheck-first-error)
(define-key evil-normal-state-map (kbd "SPC l") 'flycheck-list-errors)

;;;; Snippets

;;; YASnippet
(sk/require-package 'yasnippet)

;; ;; Add yasnippet support for all company backends
;; (defvar company-mode/enable-yas t
;;   "Enable yasnippet for all backends.")
;; (defun company-mode/backend-with-yas (backend)
;;   (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
;;       backend
;;     (append (if (consp backend) backend (list backend))
;;             '(:with company-yasnippet))))
;; (defun yas-company-hook ()
;;   (interactive)
;;   (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))
;; (add-hook 'company-mode-hook 'yas-company-hook)

;; Just enable helm/ivy/ido and this uses them automatically
(setq yas-prompt-functions '(yas-completing-prompt))

;; Disable in shell
(defun sk/force-yasnippet-off ()
  (yas-minor-mode -1)
  (setq yas-dont-activate t))
(add-hook 'term-mode-hook 'sk/force-yasnippet-off)
(add-hook 'shell-mode-hook 'sk/force-yasnippet-off)
(defun sk/diminish-yas ()
  (interactive)
  (diminish 'yas-minor-mode " γ"))
(add-hook 'yas-global-mode-hook 'sk/diminish-yas)
(add-hook 'prog-mode-hook 'yas-global-mode)
(define-key evil-insert-state-map (kbd "C-j") 'yas-insert-snippet)

;;;; Language support

;;; Elisp

;; IELM
(defun sk/ielm-here ()
  "opens up a new ielm REPL in the directory associated with the current buffer's file."
  (interactive)
  (require 'ess-site)
  (split-window-right)
  (other-window 1)
  (ielm)
  (other-window 1))

;; Hydra - for elisp
(defhydra sk/hydra-for-elisp (:color pink
                              :hint nil)
  "
 ^Send^     | ^Shell^  | ^Menu^
 ^^^^^^^^^---------|--------|----------------
 _f_unction | _s_tart  | _H_ome    e_x_ecute
 _e_xp      |        | _L_ang    _q_uit
 _r_egion   |        |
 _a_ll      |        |
"
  ("f" eval-defun)
  ("e" eval-expression)
  ("r" eval-region)
  ("a" eval-buffer)
  ("s" sk/ielm-here)
  ("L" sk/hydra-of-langs/body :exit t)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("x" counsel-M-x :color blue)
  ("q" nil :color blue))

;;; C/C++

;;; GDB
(setq gdb-many-windows t
      gdb-show-main t)

;;; Company C headers
(sk/require-package 'company-c-headers)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-c-headers))

;;; Irony mode for C++
(sk/require-package 'irony)
(defun sk/diminish-irony ()
  (interactive)
  (diminish 'irony-mode " Γ"))
(add-hook 'irony-mode-hook 'sk/diminish-irony)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun sk/irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'sk/irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; Company irony
(sk/require-package 'company-irony)
(sk/require-package 'company-irony-c-headers)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

;; Irony for flycheck
(sk/require-package 'flycheck-irony)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;;; Cmake ide
(sk/require-package 'cmake-ide)

;;; Hydra - for cpp
(defhydra sk/hydra-for-cpp (:color blue
                            :hint nil)
  "
 ^Compile^ | ^Eshell^       | ^Terminal^     | ^Menu^
 ^^^^^^^^^--------|--------------|--------------|---------------
 _m_ake    | _+_ vertical   | _|_ vertical   | _H_ome   e_x_ecute
 _c_ompile | _-_ horizontal | ___ horizontal | _L_ang   _q_uit
 _r_un     |              |              |
"
  ("m" compile)
  ("c" multi-compile-run)
  ("r" quickrun)
  ("+" sk/eshell-vertical)
  ("-" sk/eshell-horizontal)
  ("|" sk/multi-term-vertical)
  ("_" sk/multi-term-horizontal)
  ("L" sk/hydra-of-langs/body :exit t)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("x" counsel-M-x :color blue)
  ("q" nil :color blue))

;; Evil maps
(define-key evil-normal-state-map (kbd "SPC m") 'sk/hydra-for-cpp/body)

;;; Julia, R, and other statistic languages

;; ESS
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

;; Hydra - for julia
(defhydra sk/hydra-for-julia (:color pink
                              :hint nil)
  "
 ^Send^     | ^Shell^  | ^Menu^
 ^^^^^^^^^---------|--------|----------------
 _f_unction | _s_tart  | _H_ome    e_x_ecute
 _l_ine     | s_w_itch | _L_ang    _q_uit
 _r_egion   |        |
 _a_ll      |        |
"
  ("f" ess-eval-function)
  ("l" ess-eval-line)
  ("r" ess-eval-region)
  ("a" ess-eval-buffer)
  ("s" sk/julia-shell-here)
  ("w" ess-switch-to-ESS)
  ("L" sk/hydra-of-langs/body :exit t)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("x" counsel-M-x :color blue)
  ("q" nil :color blue))

;; Vertical split r REPL
(defun sk/r-shell-here ()
  "opens up a new r REPL in the directory associated with the current buffer's file."
  (interactive)
  (require 'ess-site)
  (split-window-right)
  (other-window 1)
  (R)
  (other-window 1))

;; Hydra - for R
(defhydra sk/hydra-for-r (:color pink
                          :hint nil)
  "
 ^Send^     | ^Shell^  | ^Menu^
 ^^^^^^^^^---------|--------|----------------
 _f_unction | _s_tart  | _H_ome    e_x_ecute
 _l_ine     | s_w_itch | _L_ang    _q_uit
 _r_egion   |        |
 _a_ll      |        |
"
  ("f" ess-eval-function)
  ("l" ess-eval-line)
  ("r" ess-eval-region)
  ("a" ess-eval-buffer)
  ("s" sk/r-shell-here)
  ("w" ess-switch-to-ESS)
  ("L" sk/hydra-of-langs/body :exit t)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("x" counsel-M-x :color blue)
  ("q" nil :color blue))

;;; Python

;; Highlight indentation
(sk/require-package 'highlight-indentation)

;; Elpy
(sk/require-package 'elpy)
(add-hook 'python-mode-hook 'elpy-enable)
(defun sk/diminish-elpy ()
  (interactive)
  (elpy-use-ipython)
  (diminish 'elpy-mode ""))
(add-hook 'elpy-mode-hook 'sk/diminish-elpy)

;; Cython mode
(sk/require-package 'cython-mode)

;; Virtualenv for python
(sk/require-package 'virtualenvwrapper)
(setq venv-location "~/Py34/")

;; Hydra - for python
(defhydra sk/hydra-for-python (:color pink
                               :hint nil)
  "
 ^Send^     | ^Shell^  | ^Navigate^   |
 ^^^^^^^^^---------|--------|------------|--------------
 _f_unction | _s_tart  | _d_efinition | _H_ome  e_x_ecute
 _l_ine     | s_w_itch | l_o_cation   | _L_ang  _q_uit
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
  ("q" nil :color blue))

;;; Lisp family

;; Lisp - General
(sk/require-package 'paredit)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'lisp-mode-hook 'paredit-mode)
(defun sk/diminish-paredit ()
  (interactive)
  (diminish 'paredit-mode ""))
(add-hook 'paredit-mode-hook 'sk/diminish-paredit)

;; Slime
(sk/require-package 'slime)
(add-hook 'emacs-lisp-mode-hook 'slime-mode)
(add-hook 'lisp-mode-hook 'slime-mode)
(defun sk/diminish-slime ()
  (interactive)
  (diminish 'slime-mode ""))
(add-hook 'slime-mode-hook 'sk/diminish-slime)

;;; Markdown
(sk/require-package 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;; Pandoc mode
(sk/require-package 'pandoc-mode)
(add-hook 'markdown-mode-hook 'pandoc-mode)

;;; LaTeX-mode
(sk/require-package 'auctex)
(sk/require-package 'auctex-latexmk)

;; Company auctex
(sk/require-package 'company-auctex)

;;; Web mode
(sk/require-package 'web-mode)
(add-hook 'html-mode-hook 'web-mode)

;; Company for web mode
(sk/require-package 'company-web)

;;; JavaScript
(sk/require-package 'js2-mode)
(sk/require-package 'skewer-mode)

;;; Applescript
(sk/require-package 'applescript-mode)
(add-to-list 'auto-mode-alist '("\\.scpt\\'" . applescript-mode))
(add-to-list 'auto-mode-alist '("\\.applescript\\'" . applescript-mode))

;;; Go lang
(sk/require-package 'go-mode)

;; Company for Go
(sk/require-package 'company-go)

;;; SQL
(sk/require-package 'emacsql)
(sk/require-package 'emacsql-mysql)
(sk/require-package 'emacsql-sqlite)
(sk/require-package 'esqlite)
(sk/require-package 'pcsv)

;;; Java
(sk/require-package 'emacs-eclim)
(defun sk/eclim-mode-hook ()
  (interactive)
  (require 'eclimd)
  (require 'company-emacs-eclim)
  (setq eclim-executable (or (executable-find "eclim") "/Applications/Eclipse/Eclipse.app/Contents/Eclipse/eclim")
        eclimd-executable (or (executable-find "eclimd") "/Applications/Eclipse/Eclipse.app/Contents/Eclipse/eclimd")
        eclimd-wait-for-process nil
        eclimd-default-workspace "~/Documents/workspace/eclipse/")
  (company-emacs-eclim-setup))
(add-hook 'eclim-mode-hook 'sk/eclim-mode-hook)
(add-hook 'java-mode-hook 'eclim-mode)

;;; Sage
(sk/require-package 'sage-shell-mode)
(setq sage-shell:sage-executable "/Applications/Sage-6.8.app/Contents/Resources/sage/sage"
      sage-shell:input-history-cache-file "~/.emacs.d/.sage_shell_input_history"
      sage-shell-sagetex:auctex-command-name "LaTeX"
      sage-shell-sagetex:latex-command "latexmk")

;;; MATLAB mode
(sk/require-package 'matlab-mode)
(eval-after-load 'matlab
  '(add-to-list 'matlab-shell-command-switches "-nosplash"))
(setq matlab-shell-command "/Applications/MATLAB_R2014a.app/bin/matlab"
      matlab-indent-function t)

;; Vertical split matlab shell
(defun sk/matlab-shell-here ()
  "opens up a new matlab shell in the directory associated with the current buffer's file."
  (interactive)
  (split-window-right)
  (other-window 1)
  (matlab-shell)
  (other-window 1))

;; Hydra - for matlab
(defhydra sk/hydra-for-matlab (:color pink
                               :hint nil)
  "
 ^Send^    | ^Shell^  | ^Menu^
 ^^^^^^^^--------|--------|----------------
 _c_ell    | _s_tart  | _H_ome    e_x_ecute
 _l_ine    | s_w_itch | _L_ang    _q_uit
 _r_egion  |        |
 co_m_mand |        |
"
  ("c" matlab-shell-run-cell)
  ("l" matlab-shell-run-region-or-line)
  ("r" matlab-shell-run-region)
  ("m" ess-eval-buffer)
  ("s" sk/matlab-shell-here)
  ("w" matlab-show-matlab-shell-buffer)
  ("L" sk/hydra-of-langs/body :exit t)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("x" counsel-M-x :color blue)
  ("q" nil :color blue))

;;;; Org mode

;;; Org settings

;; General settings
(setq org-directory "~/Dropbox/notes/"
      org-completion-use-ido nil
      ;; Indent
      org-startup-indented t
      org-hide-leading-stars t
      ;; Images
      org-image-actual-width '(300)
      ;; Source code
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      ;; Quotes
      org-export-with-smart-quotes t
      ;; Citations
      org-latex-to-pdf-process '("pdflatex %f" "biber %b" "pdflatex %f" "pdflatex %f"))

;; Tags with fast selection keys
(setq org-tag-alist (quote (("errand" . ?e)
                            ("blog" . ?b)
                            ("personal" . ?k)
                            ("report" . ?r)
                            ("thesis" . ?t) ;; temporary
                            ("accounts" . ?a)
                            ("lubby" . ?l)
                            ("movie" . ?m)
                            ("netflix" . ?N)
                            ("via" . ?v)
                            ("idea" . ?i)
                            ("project" . ?p)
                            ("job" . ?j)
                            ("work" . ?w)
                            ("home" . ?h)
                            ("noexport" . ?x)
                            ("Technial" . ?T)
                            ("Random" . ?R)
                            ("Crafts" . ?C)
                            ("story/news" . ?s)
                            ("note" . ?n))))

;; TODO Keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "HOLD(h@/!)" "|" "DONE(d!)")
        (sequence "|" "CANCELLED(c@)")))

;; Agenda settings
(setq org-agenda-files (list "~/Dropbox/notes/work.org"
                             "~/Dropbox/notes/blog.org"
                             "~/Dropbox/notes/ledger.org"
                             "~/Dropbox/notes/notes.org"))
(setq org-deadline-warning-days 7
      org-agenda-span 'fortnight
      org-agenda-skip-scheduled-if-deadline-is-shown t)

;; Links
(setq org-link-abbrev-alist
      '(("bugzilla"  . "http://10.1.2.9/bugzilla/show_bug.cgi?id=")
        ("url-to-ja" . "http://translate.google.fr/translate?sl=en&tl=ja&u=%h")
        ("google"    . "http://www.google.com/search?q=")
        ("gmaps"      . "http://maps.google.com/maps?q=%s")))

;; Capture templates
(setq org-capture-templates
      '(("n" "Note" entry (file+headline "~/Dropbox/notes/notes.org" "Notes")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("r" "Report" entry (file+headline "~/Dropbox/notes/work.org" "Thesis Notes") ;; temporary
         "* %?\nEntered on %U\n  %i\n  %a")
        ("j" "Job leads/status" entry (file+headline "~/Dropbox/notes/notes.org" "Job leads/status")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("s" "Story/News" entry (file+headline "~/Dropbox/notes/notes.org" "Story/News")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("a" "Accounts - Ledger" entry (file+datetree "~/Dropbox/notes/ledger.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("b" "Blog" entry (file+datetree "~/Dropbox/notes/blog.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("p" "Project" entry (file+headline "~/Dropbox/notes/notes.org" "Projects")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("w" "Work" entry (file+headline "~/Dropbox/notes/work.org" "Work")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("h" "Home" entry (file+headline "~/Dropbox/notes/notes.org" "Home")
         "* %?\nEntered on %U\n  %i\n  %a")))

;; LaTeX support
(sk/require-package 'cdlatex)
(defun sk/diminish-org ()
  (interactive)
  (diminish 'org-indent-mode "")
  (diminish 'org-cdlatex-mode ""))
(add-hook 'org-mode-hook 'sk/diminish-org)
(add-hook 'org-mode-hook 'org-cdlatex-mode)

;;; Custom functions

;; Correct those annoying double caps typos
(defun sk/dcaps-to-scaps ()
  "Convert word in DOuble CApitals to Single Capitals."
  (interactive)
  (and (= ?w (char-syntax (char-before)))
       (save-excursion
         (and (if (called-interactively-p)
                  (skip-syntax-backward "w")
                (= -3 (skip-syntax-backward "w")))
              (let (case-fold-search)
                (looking-at "\\b[[:upper:]]\\{2\\}[[:lower:]]"))
              (capitalize-word 1)))))
(define-minor-mode dubcaps-mode
  "Toggle `dubcaps-mode'.  Converts words in DOuble CApitals to
Single Capitals as you type."
  :init-value nil
  :lighter (" DC")
  (if dubcaps-mode
      (add-hook 'post-self-insert-hook #'sk/dcaps-to-scaps nil 'local)
    (remove-hook 'post-self-insert-hook #'sk/dcaps-to-scaps 'local)))
(defun sk/diminish-dubcaps ()
  (interactive)
  (diminish 'dubcaps-mode ""))
(add-hook 'dubcaps-mode-hook 'sk/diminish-dubcaps)
(add-hook 'org-mode-hook #'dubcaps-mode)

;; PDF pages note taking support
(defun sk/other-pdf-next ()
  "Turns the next page in adjoining PDF file"
  (interactive)
  (other-window 1)
  (doc-view-next-page)
  (other-window 1))
(defun sk/other-pdf-previous ()
  "Turns the previous page in adjoining PDF file"
  (interactive)
  (other-window 1)
  (doc-view-previous-page)
  (other-window 1))

;; Evil maps
(define-key evil-normal-state-map (kbd "]p") 'sk/other-pdf-next)
(define-key evil-normal-state-map (kbd "[p") 'sk/other-pdf-previous)

;;; Coding and external packages

;; Babel for languages
(sk/require-package 'babel)
(setq org-confirm-babel-evaluate nil)
;; Org load languages
(defun sk/org-custom-load ()
  (interactive)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     ;; (dot . t)
     ;; (ditaa . t)
     (latex . t)
     (gnuplot . t)
     (sh . t)
     ;; (C . t)
     (R . t)
     ;; (octave . t)
     (matlab . t)
     (python . t))))
(sk/require-package 'ob-ipython)

;; Export using reveal and impress-js
(sk/require-package 'ox-reveal)
(sk/require-package 'ox-impress-js)

;; Restructred text and pandoc support
(sk/require-package 'ox-rst)
(sk/require-package 'ox-pandoc)

;;; Hydras

;; Org navigation and manipulation
(defhydra sk/hydra-org-manipulate (:color pink
                                   :hint nil)
  "
^Move heading^      | ^Item^ | ^Menu^
^^^^^^^^^^^^------------------|------|--------------
^ ^ _k_ ^ ^   _<_ promote | _u_p   | _H_ome  e_x_ecute
_h_ ^+^ _l_   _>_ demote  | _d_own | _O_rg   _q_uit
^ ^ _j_ ^ ^             |      |
"
  ("h" org-metaleft)
  ("l" org-metaright)
  ("j" org-metadown)
  ("k" org-metaup)
  ("<" org-promote)
  (">" org-demote)
  ("d" org-move-item-down)
  ("u" org-move-item-up)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("O" sk/hydra-of-org/body :exit t)
  ("x" counsel-M-x :color blue)
  ("q" nil :color blue))

;; Evil maps
(define-key evil-normal-state-map (kbd "gom") 'sk/hydra-org-manipulate/body)

;; Org table manipulation
(defhydra sk/hydra-org-tables (:color pink
                               :hint nil)
  "
^Field^ | ^Shift^ | ^Insert^  | ^Delete^ | ^Field^ | ^Table^    | ^Formula^ | ^Menu^
^^^^^^^^^^^------|-------|---------|--------|-------|----------|---------|-------------
^ ^ _k_ ^ ^ | ^ ^ _p_ ^ ^ | _r_ow     | _R_ow    | _e_dit  | _a_lign    | _+_ sum   | _O_rg  e_x_ecute
_h_ ^+^ _l_ | _b_ ^+^ _f_ | _c_olumn  | _C_olumn | _b_lank | _|_ create | _=_ eval  | _H_ome _q_uit
^ ^ _j_ ^ ^ | ^ ^ _n_ ^ ^ | _-_ hline |        | _i_nfo  |          | _f_ edit  |
"
  ("a" org-table-align)
  ("l" org-table-next-field)
  ("h" org-table-previous-field)
  ("j" org-table-end-of-field)
  ("k" org-table-beginning-of-field)
  ("r" org-table-insert-row)
  ("c" org-table-insert-column)
  ("-" org-table-insert-hline)
  ("n" org-table-move-row-down)
  ("p" org-table-move-row-up)
  ("b" org-table-move-column-left)
  ("f" org-table-move-column-right)
  ("R" org-table-kill-row)
  ("C" org-table-delete-column)
  ("b" org-table-blank-field)
  ("e" org-table-edit-field)
  ("i" org-table-field-info)
  ("+" org-table-sum)
  ("=" org-table-eval-formula)
  ("f" org-table-edit-formulas)
  ("|" org-table-create-or-convert-from-region)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("O" sk/hydra-of-org/body :exit t)
  ("x" counsel-M-x :color blue)
  ("q" nil :color blue))

;; Evil maps
(define-key evil-normal-state-map (kbd "go|") 'sk/hydra-org-tables/body)

;; Org clock manipulation
(defhydra sk/hydra-org-clock (:color pink
                              :hint nil)
  "
^Clock^              | ^Timer^ | ^Stamp^    | ^Menu^
^^^^^^^^^-------------------|-------|----------|---------
_i_n       _z_ resolve | _b_egin | _s_tamp    | _O_rg
_o_ut      _l_ast      | _e_nd   | _I_nactive | _H_ome
_r_eport   _c_ancel    | _t_imer |          | e_x_ecute
_d_isplay  _g_oto      | _T_ set |          | _q_uit
"
  ("i" org-clock-in)
  ("o" org-clock-out)
  ("r" org-clock-report)
  ("z" org-resolve-clocks)
  ("c" org-clock-cancel)
  ("d" org-clock-display)
  ("l" org-clock-in-last)
  ("g" org-clock-goto)
  ("t" org-timer)
  ("T" org-timer-set-timer)
  ("b" org-timer-start)
  ("e" org-timer-stop)
  ("s" org-time-stamp)
  ("I" org-time-stamp-inactive)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("O" sk/hydra-of-org/body :exit t)
  ("x" counsel-M-x :color blue)
  ("q" nil :color blue))

;; Evil maps
(define-key evil-normal-state-map (kbd "goc") 'sk/hydra-org-clock/body)

;; Org tags and todo manipulation
(defhydra sk/hydra-org-tag-todo (:color pink
                                 :hint nil)
  "
^tags^        | ^TODO^     | ^Checkbox^     | ^Priority^   | ^Menu^
^^^^^^^^^^^^^------------|----------|--------------|------------|---------
_t_ags        | _T_ODO     | _c_heckbox     | _#_ priority | _O_rg
_v_iew        | _d_eadline | t_o_ggle       | _+_ increase | _H_ome
_m_atch       | _C_lose    | _u_pdate stats | _-_ decrease | e_x_ecute
s_p_arse-tree | _s_chedule | _r_eset        |            | _q_uit
            | _a_genda   | _U_pdate count |            |
"
  ("t" org-set-tags-command :color blue)
  ("v" org-tags-view)
  ("m" org-match-sparse-tree :color blue)
  ("p" org-sparse-tree :color blue)
  ("#" org-priority)
  ("+" org-priority-up)
  ("-" org-priority-down)
  ("T" org-todo :color blue)
  ("d" org-deadline :color blue)
  ("C" org-deadline-close :color blue)
  ("s" org-schedule :color blue)
  ("a" org-check-deadlines)
  ("c" org-checkbox)
  ("o" org-toggle-checkbox)
  ("U" org-update-checkbox-count-maybe)
  ("r" org-reset-checkbox-state-subtree)
  ("u" org-update-statistics-cookies)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("O" sk/hydra-of-org/body :exit t)
  ("x" counsel-M-x :color blue)
  ("q" nil :color blue))

;; Evil maps
(define-key evil-normal-state-map (kbd "got") 'sk/hydra-org-tag-todo/body)

;; Org drawer
(defhydra sk/hydra-org-drawer (:color pink
                               :hint nil)
  "
 ^Drawer^ | ^Property^ | ^Menu^
^^^^^^^^^^--------|----------|--------
 _i_nsert | _p_roperty | _O_rg
        | _s_et      | _H_ome
        | _d_elete   | e_x_ecute
        | _t_oggle   | _q_uit
"
  ("i" org-insert-drawer)
  ("p" org-insert-property-drawer)
  ("s" org-set-property)
  ("d" org-delete-property)
  ("t" org-toggle-ordered-property)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("O" sk/hydra-of-org/body :exit t)
  ("x" counsel-M-x :color blue)
  ("q" nil :color blue))

;; Evil maps
(define-key evil-normal-state-map (kbd "gow") 'sk/hydra-org-drawer/body)

;; Hydra of org-mode
(defhydra sk/hydra-of-org (:color pink
                           :hint nil)
  "
 ^Outline^       | ^Item^  | ^Table^ | ^Block^  | ^Link^    | ^State^     | ^Toggle^              | ^Subtree^ | ^Org and Menu^
 ^^^^^^^^^^^--------------|-------|-------|--------|---------|-----------|---------------------|---------|--------------------
 ^ ^ _k_ ^ ^  ^ ^ _p_ ^ ^  | ^ ^ _u_ ^ ^ | ^ ^ ^ ^ ^ ^ | ^ ^ _[_ ^ ^  | ^ ^ _{_ ^ ^   | _t_ag/_T_odo  | n_o_te      lat_e_x     | _s_ubtree | _c_apture  _R_eorganize
 _h_ ^+^ _l_  ^ ^ ^+^ ^ ^  | ^ ^ ^+^ ^ ^ | _b_ ^+^ _f_ | ^ ^ ^+^ ^ ^  | ^ ^ ^+^ ^ ^   | re_v_eal    | _F_ootnote  _i_mages    | _w_iden   | _a_genda   _|_ table
 ^ ^ _j_ ^ ^  ^ ^ _n_ ^ ^  | ^ ^ _d_ ^ ^ | ^ ^ ^ ^ ^ ^ | ^ ^ _]_ ^ ^  | ^ ^ _}_ ^ ^   | _r_efile    | RefTe_X_    _*_ heading | _K_ill    | _-_ code   _H_ome
               |       | c_L_ear | _U_pdate | _D_isplay | _A_rchive   | eff_O_rt    _E_xport    | cop_y_    | _C_lock    e_x_ecute
               |       |       |        | _S_tore   |           |           _>_ cal     |         | dra_W_er   _q_uit
               |       |       |        | _I_nsert  |           |           _<_ date    |         |
"
  ("j" outline-next-visible-heading)
  ("k" outline-previous-visible-heading)
  ("l" org-down-element)
  ("h" org-up-element)
  ("g" sk/hydra-deft :exit t)
  ("u" org-next-item)
  ("d" org-previous-item)
  ("n" org-forward-heading-same-level)
  ("p" org-backward-heading-same-level)
  ("]" org-next-block)
  ("[" org-previous-block)
  ("U" org-update-all-dblocks :color blue)
  ("}" org-next-link)
  ("{" org-previous-link)
  ("D" org-toggle-link-display)
  ("S" org-store-link)
  ("I" org-insert-link)
  ("f" org-table-next-field)
  ("b" org-table-previous-field)
  ("L" org-table-blank-field :color blue)
  ("t" sk/hydra-org-tag-todo/body :exit t)
  ("T" sk/hydra-org-tag-todo/body :exit t)
  ("|" sk/hydra-org-tables :exit t)
  ("v" org-reveal)
  ("r" org-refile :color blue)
  ("A" org-archive-subtree-default :color blue)
  ("o" org-note)
  ("F" org-footnote)
  ("W" sk/hydra-org-drawer/body :exit t)
  ("e" org-preview-latex-fragment)
  ("i" org-display-inline-images)
  ("*" org-toggle-heading)
  ("E" org-export-dispatch)
  ("C" sk/hydra-org-clock/body :exit t)
  ("X" org-reftex-citation :color blue)
  (">" org-goto-calendar :color blue)
  ("<" org-date-from-calendar)
  ("O" org-set-effort)
  ("s" org-narrow-to-subtree)
  ("w" widen)
  ("K" org-cut-subtree)
  ("y" org-copy-subtree)
  ("c" org-capture)
  ("a" org-agenda)
  ("-" org-edit-src-code)
  ("R" sk/hydra-org-manipulate/body :exit t)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("O" sk/hydra-of-org/body :exit t)
  ("x" counsel-M-x :color blue)
  ("q" nil :color blue))

;; Evil maps
(define-key evil-normal-state-map (kbd "goo") 'sk/hydra-of-org/body)

;;; Evil maps for general org mode
(evil-set-initial-state 'calendar-mode 'emacs)
(define-key evil-normal-state-map (kbd "SPC c") 'org-capture)
(define-key evil-normal-state-map (kbd "SPC o") 'org-agenda)
(define-key evil-normal-state-map (kbd "SPC -") 'org-edit-src-code)
(define-key evil-normal-state-map (kbd "SPC =") 'org-edit-src-exit)
(define-key evil-normal-state-map (kbd "SPC ]") 'org-narrow-to-subtree)
(define-key evil-normal-state-map (kbd "[u") 'org-up-element)
(define-key evil-normal-state-map (kbd "]u") 'org-down-element)
(define-key evil-normal-state-map (kbd "[o") 'org-previous-visible-heading)
(define-key evil-normal-state-map (kbd "]o") 'org-next-visible-heading)
(define-key evil-normal-state-map (kbd "[i") 'org-previous-item)
(define-key evil-normal-state-map (kbd "]i") 'org-next-item)
(define-key evil-normal-state-map (kbd "[h") 'org-backward-heading-same-level)
(define-key evil-normal-state-map (kbd "]h") 'org-forward-heading-same-level)
(define-key evil-normal-state-map (kbd "[b") 'org-previous-block)
(define-key evil-normal-state-map (kbd "]b") 'org-next-block)
(define-key evil-normal-state-map (kbd "[l") 'org-previous-link)
(define-key evil-normal-state-map (kbd "]l") 'org-next-link)
(define-key evil-normal-state-map (kbd "[f") 'org-table-previous-field)
(define-key evil-normal-state-map (kbd "]f") 'org-table-next-field)
(define-key evil-normal-state-map (kbd "gob") 'org-table-blank-field)
(define-key evil-normal-state-map (kbd "gox") 'org-preview-latex-fragment)
(define-key evil-normal-state-map (kbd "goi") 'org-toggle-inline-images)
(define-key evil-normal-state-map (kbd "gor") 'org-refile)
(define-key evil-normal-state-map (kbd "goa") 'org-archive-subtree-default)
(define-key evil-normal-state-map (kbd "gon") 'org-add-note)
(define-key evil-normal-state-map (kbd "gof") 'org-footnote-new)
(define-key evil-normal-state-map (kbd "goe") 'org-export-dispatch)
(define-key evil-normal-state-map (kbd "gol") 'org-insert-link)
(define-key evil-normal-state-map (kbd "gou") 'org-store-link)
(define-key evil-normal-state-map (kbd "god") 'org-toggle-link-display)
(define-key evil-normal-state-map (kbd "goy") 'org-copy-subtree)
(define-key evil-normal-state-map (kbd "gok") 'org-cut-subtree)
(define-key evil-normal-state-map (kbd "goh") 'org-toggle-heading)
(define-key evil-normal-state-map (kbd "go>") 'org-goto-calendar)
(define-key evil-normal-state-map (kbd "go<") 'org-date-from-calendar)
(define-key evil-normal-state-map (kbd "gos") 'org-sort)
(define-key evil-visual-state-map (kbd "SPC c") 'org-capture)
(define-key evil-visual-state-map (kbd "SPC o") 'org-agenda)

;; Themes
(load-theme 'leuven t)

;;; Evil - Vim emulation - Continued
;; Escape for everything
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
;; Maps
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "t") 'imenu)
(define-key evil-normal-state-map (kbd "Z") 'delete-other-windows)
(define-key evil-normal-state-map (kbd "Q") 'winner-undo)
(define-key evil-normal-state-map (kbd "K") 'man)
(define-key evil-normal-state-map (kbd "\\") 'universal-argument)
(define-key evil-normal-state-map (kbd "gs") 'electric-newline-and-maybe-indent)
(define-key evil-normal-state-map (kbd "gl") 'browse-url-at-point)
(define-key evil-normal-state-map (kbd "gL") 'browse-url-at-mouse)
(define-key evil-normal-state-map (kbd "[F") 'delete-frame)
(define-key evil-normal-state-map (kbd "]F") 'make-frame)
(define-key evil-normal-state-map (kbd "SPC q") 'evil-quit)
(define-key evil-normal-state-map (kbd "SPC w") 'save-buffer)
(define-key evil-normal-state-map (kbd "SPC k") 'kill-buffer)
(define-key evil-normal-state-map (kbd "SPC z") 'sk/toggle-frame-fullscreen-non-native)
(define-key evil-normal-state-map (kbd "SPC f") 'find-file)
(define-key evil-normal-state-map (kbd "SPC [") 'widen)
(define-key evil-normal-state-map (kbd "SPC 4") 'shell-command)
(define-key evil-normal-state-map (kbd "SPC DEL") 'whitespace-cleanup)
(define-key evil-normal-state-map (kbd "SPC ,") 'describe-bindings)
(define-key evil-normal-state-map (kbd "SPC 6") 'quick-calc)
(define-key evil-normal-state-map (kbd "SPC \\") 'toggle-input-method)
(define-key evil-visual-state-map (kbd "SPC ]") 'narrow-to-region)
(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

;; More useful arrow keys
(global-set-key (kbd "<left>") 'evil-window-left)
(global-set-key (kbd "<right>") 'evil-window-right)
(global-set-key (kbd "<up>") 'evil-window-up)
(global-set-key (kbd "<down>") 'evil-window-down)
(define-key evil-normal-state-map (kbd "<left>") 'evil-window-left)
(define-key evil-normal-state-map (kbd "<right>") 'evil-window-right)
(define-key evil-normal-state-map (kbd "<up>") 'evil-window-up)
(define-key evil-normal-state-map (kbd "<down>") 'evil-window-down)
(define-key evil-visual-state-map (kbd "<left>") 'evil-window-left)
(define-key evil-visual-state-map (kbd "<right>") 'evil-window-right)
(define-key evil-visual-state-map (kbd "<up>") 'evil-window-up)
(define-key evil-visual-state-map (kbd "<down>") 'evil-window-down)
(define-key evil-insert-state-map (kbd "<left>") 'evil-window-left)
(define-key evil-insert-state-map (kbd "<right>") 'evil-window-right)
(define-key evil-insert-state-map (kbd "<up>") 'evil-window-up)
(define-key evil-insert-state-map (kbd "<down>") 'evil-window-down)

;; Evil surround
(sk/require-package 'evil-surround)
(global-evil-surround-mode 1)

;; '%' matching like vim
(sk/require-package 'evil-matchit)
(define-key evil-normal-state-map "%" #'evilmi-jump-items)
(define-key evil-inner-text-objects-map "%" #'evilmi-text-object)
(define-key evil-outer-text-objects-map "%" #'evilmi-text-object)
(global-evil-matchit-mode 1)

;; Evil args
(sk/require-package 'evil-args)
(define-key evil-inner-text-objects-map "," #'evil-inner-arg)
(define-key evil-outer-text-objects-map "," #'evil-outer-arg)
(define-key evil-normal-state-map "\C-j" #'evil-jump-out-args)

;; Jump lists like vim
(sk/require-package 'evil-jumper)
(global-evil-jumper-mode 1)

;; Evil commentary
(sk/require-package 'evil-commentary)
(defun diminish-evil-commentary ()
  (interactive)
  (diminish 'evil-commentary-mode ""))
(add-hook 'evil-commentary-mode-hook 'diminish-evil-commentary)
(evil-commentary-mode)

;; Evil exchange
(sk/require-package 'evil-exchange)
(evil-exchange-install)

;; Increment and decrement numbers like vim
(sk/require-package 'evil-numbers)
(define-key evil-normal-state-map (kbd "C-k") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-j") 'evil-numbers/dec-at-pt)

;;; Evil text objects - Courtesy PythonNut
;; evil block indentation textobject for Python
(defun evil-indent--current-indentation ()
  "Return the indentation of the current line. Moves point."
  (buffer-substring-no-properties (point-at-bol)
                                  (progn (back-to-indentation)
                                         (point))))
(defun evil-indent--block-range (&optional point)
  "Return the point at the begin and end of the text block "
  ;; there are faster ways to mark the entire file
  ;; so assume the user wants a block and skip to there
  (while (and (string-empty-p
               (evil-indent--current-indentation))
              (not (eobp)))
    (forward-line))
  (cl-flet* ((empty-line-p ()
                           (string-match "^[[:space:]]*$"
                                         (buffer-substring-no-properties
                                          (line-beginning-position)
                                          (line-end-position))))
             (line-indent-ok (indent)
                             (or (<= (length indent)
                                     (length (evil-indent--current-indentation)))
                                 (empty-line-p))))
    (let ((indent (evil-indent--current-indentation)) start begin end)
      ;; now skip ahead to the Nth block with this indentation
      (dotimes (index (or last-prefix-arg 0))
        (while (and (line-indent-ok) (not (eobp))) (forward-line))
        (while (or (line-indent-ok indent) (eobp)) (forward-line)))
      (save-excursion
        (setq start (goto-char (or point (point))))
        (while (and (line-indent-ok indent) (not (bobp)))
          (setq begin (point))
          (forward-line -1))
        (goto-char start)
        (while (and (line-indent-ok indent) (not (eobp)))
          (setq end (point))
          (forward-line))
        (goto-char end)
        (while (empty-line-p)
          (forward-line -1)
          (setq end (point)))
        (list begin end)))))
(evil-define-text-object evil-indent-i-block (&optional count beg end type)
  "Text object describing the block with the same indentation as the current line."
  (let ((range (evil-indent--block-range)))
    (evil-range (first range) (second range) 'line)))
(evil-define-text-object evil-indent-a-block (&optional count beg end type)
  "Text object describing the block with the same indentation as the current line and the line above."
  :type line
  (let ((range (evil-indent--block-range)))
    (evil-range (save-excursion
                  (goto-char (first (evil-indent--block-range)))
                  (forward-line -1)
                  (point-at-bol))
                (second range) 'line)))
(evil-define-text-object evil-indent-a-block-end (count &optional beg end type)
  "Text object describing the block with the same indentation as the current line and the lines above and below."
  :type line
  (let ((range (evil-indent--block-range)))
    (evil-range (save-excursion
                  (goto-char (first range))
                  (forward-line -1)
                  (point-at-bol))
                (save-excursion
                  (goto-char (second range))
                  (forward-line 1)
                  (point-at-eol))
                'line)))
(define-key evil-inner-text-objects-map "i" #'evil-indent-i-block)
(define-key evil-outer-text-objects-map "i" #'evil-indent-a-block)
(define-key evil-outer-text-objects-map "I" #'evil-indent-a-block-end)

;; Sentence and range
(evil-define-text-object evil-i-line-range (count &optional beg end type)
  "Text object describing the block with the same indentation as the current line."
  :type line
  (save-excursion
    (let ((beg (evil-avy-jump-line-and-revert))
          (end (evil-avy-jump-line-and-revert)))
      (if (> beg end)
          (evil-range beg end #'line)
        (evil-range end beg #'line)))))
(define-key evil-inner-text-objects-map "r" #'evil-i-line-range)
(define-key evil-inner-text-objects-map "s" #'evil-inner-sentence)
(define-key evil-outer-text-objects-map "s" #'evil-a-sentence)

;; Entire buffer text object
(evil-define-text-object evil-i-entire-buffer (count &optional ben end type)
  "Text object describing the entire buffer excluding empty lines at the end"
  :type line
  (evil-range (point-min) (save-excursion
                            (goto-char (point-max))
                            (skip-chars-backward " \n\t")
                            (point)) 'line))

(evil-define-text-object evil-an-entire-buffer (count &optional beg end type)
  "Text object describing the entire buffer"
  :type line
  (evil-range (point-min) (point-max) 'line))
(define-key evil-inner-text-objects-map "a" #'evil-i-entire-buffer)
(define-key evil-outer-text-objects-map "a" #'evil-an-entire-buffer)

;;; Evil operators
;; Macros on all objects
(evil-define-operator evil-macro-on-all-lines (beg end &optional arg)
  (evil-with-state
    (evil-normal-state)
    (goto-char end)
    (evil-visual-state)
    (goto-char beg)
    (evil-ex-normal (region-beginning) (region-end)
                    (concat "@"
                            (single-key-description
                             (read-char "What macro?"))))))
(define-key evil-operator-state-map "g@" #'evil-macro-on-all-lines)
(define-key evil-normal-state-map "g@" #'evil-macro-on-all-lines)

;; Evil text object proper sentence with abbreviation
(sk/require-package 'sentence-navigation)
(define-key evil-normal-state-map ")" 'sentence-nav-evil-forward)
(define-key evil-normal-state-map "(" 'sentence-nav-evil-backward)
(define-key evil-normal-state-map "g)" 'sentence-nav-evil-forward-end)
(define-key evil-normal-state-map "g(" 'sentence-nav-evil-backward-end)
(define-key evil-outer-text-objects-map "s" 'sentence-nav-evil-outer-sentence)
(define-key evil-inner-text-objects-map "s" 'sentence-nav-evil-inner-sentence)

;;; Navigation

;; Neotree
(sk/require-package 'neotree)
(add-hook 'neotree-mode-hook
            (lambda ()
              (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "o") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "h") 'neotree-select-up-node)
              (define-key evil-normal-state-local-map (kbd "l") 'neotree-change-root)
              (define-key evil-normal-state-local-map (kbd ".") 'neotree-hidden-file-toggle)
              (define-key evil-normal-state-local-map (kbd "r") 'neotree-refresh)
              (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
              (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))
(define-key evil-normal-state-map (kbd "SPC n") 'neotree-toggle)

;; Evil avy bindings
(define-key evil-normal-state-map (kbd "SPC h") 'avy-goto-line)
(define-key evil-visual-state-map (kbd "SPC h") 'avy-goto-line)
(define-key evil-normal-state-map (kbd "s") 'avy-goto-char-2)
(define-key evil-visual-state-map (kbd "s") 'avy-goto-char-2)

;; Ediff plain window and vertical
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;; Remote file navigation
(sk/require-package 'tramp)
(setq tramp-ssh-controlmaster-options "ssh")

;; Very large file viewing
(sk/require-package 'vlf)

;; Swiper helpers
(defun swiper-at-point ()
  (swiper symbol-at-point))

;;; Dash at point
(sk/require-package 'dash-at-point)
(define-key evil-normal-state-map (kbd "SPC 1") 'dash-at-point-with-docset)

;;; Interact with OS services
;; Jabber
(sk/require-package 'jabber)
(setq jabber-history-enabled t
      jabber-activity-mode nil
      jabber-use-global-history nil
      jabber-backlog-number 40
      jabber-backlog-days 30)
(setq jabber-alert-presence-message-function
      (lambda (who oldstatus newstatus statustext) nil))
(define-key evil-normal-state-map (kbd "SPC 2") 'jabber-chat-with)
(setq jabber-account-list
      '(("sriram.krishnaswamy.92@chat.facebook.com"
         (:network-server . "chat.facebook.com")
         (:password . "HZ3RK54H2A"))
        ("sriram.krish.92@gmail.com"
         (:network-server . "talk.google.com")
         (:password . "chceuskratogxjzs")
         (:connection-type . ssl))))

;;; Text editing

;; Indenting
(defun my-generate-tab-stops (&optional width max)
  "Return a sequence suitable for `tab-stop-list'."
  (let* ((max-column (or max 200))
         (tab-width (or width tab-width))
         (count (/ max-column tab-width)))
    (number-sequence tab-width (* tab-width count) tab-width)))
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)
(setq tab-stop-list (my-generate-tab-stops))

;; Spell check
(add-hook 'text-mode-hook 'flyspell-mode)
(defun flyspell-goto-previous-error (arg)
  "Go to arg previous spelling error."
  (interactive "p")
  (while (not (= 0 arg))
    (let ((pos (point))
          (min (point-min)))
      (if (and (eq (current-buffer) flyspell-old-buffer-error)
               (eq pos flyspell-old-pos-error))
          (progn
            (if (= flyspell-old-pos-error min)
                ;; goto beginning of buffer
                (progn
                  (message "Restarting from end of buffer")
                  (goto-char (point-max)))
              (backward-word 1))
            (setq pos (point))))
      ;; seek the next error
      (while (and (> pos min)
                  (let ((ovs (overlays-at pos))
                        (r '()))
                    (while (and (not r) (consp ovs))
                      (if (flyspell-overlay-p (car ovs))
                          (setq r t)
                        (setq ovs (cdr ovs))))
                    (not r)))
        (backward-word 1)
        (setq pos (point)))
      ;; save the current location for next invocation
      (setq arg (1- arg))
      (setq flyspell-old-pos-error pos)
      (setq flyspell-old-buffer-error (current-buffer))
      (goto-char pos)
      (if (= pos min)
          (progn
            (message "No more miss-spelled word!")
            (setq arg 0))
        (forward-word)))))
(defun diminish-flyspell ()
  (interactive)
  (define-key evil-normal-state-map (kbd "]s") 'flyspell-goto-next-error)
  (define-key evil-normal-state-map (kbd "[s") 'flyspell-goto-previous-error)
  (diminish 'flyspell-mode ""))
(add-hook 'flyspell-mode-hook 'diminish-flyspell)
(defun diminish-aspell ()
  (interactive)
  (diminish 'aspell-mode ""))
(add-hook 'aspell-mode-hook 'diminish-aspell)
(defun diminish-ispell ()
  (interactive)
  (diminish 'ispell-mode ""))
(add-hook 'ispell-mode-hook 'diminish-ispell)

;; Better folding
(sk/require-package 'origami)
(define-key evil-normal-state-map (kbd "]z") 'origami-next-fold)
(define-key evil-normal-state-map (kbd "[z") 'origami-previous-fold)
(define-key evil-normal-state-map (kbd "zx") 'origami-toggle-node)
(define-key evil-normal-state-map (kbd "zs") 'origami-show-only-node)
(define-key evil-normal-state-map (kbd "zg") 'origami-toggle-all-nodes)
(define-key evil-normal-state-map (kbd "zu") 'origami-undo)
(define-key evil-normal-state-map (kbd "zr") 'origami-redo)
(define-key evil-normal-state-map (kbd "zf") 'origami-close-node)
(define-key evil-normal-state-map (kbd "zd") 'origami-open-node)

;; Play well with FCI mode
(defvar-local company-fci-mode-on-p nil)
(defun company-turn-off-fci (&rest ignore)
  (when (boundp 'fci-mode)
    (setq company-fci-mode-on-p fci-mode)
    (when fci-mode (fci-mode -1))))
(defun company-maybe-turn-on-fci (&rest ignore)
  (when company-fci-mode-on-p (fci-mode 1)))
(add-hook 'company-completion-started-hook 'company-turn-off-fci)
(add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
(add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)

;;; Language and Syntax

;;; Deft for quickly accessing notes
(sk/require-package 'deft)
(setq deft-extensions '("org" "md" "txt" "tex")
      deft-recursive t
      deft-use-filename-as-title t
      deft-directory "~/Dropbox/notes")
;; Hydra for deft
(defhydra hydra-deft (:color red
                      :hint nil)
  "
        ^Move^                     ^Deft^
^^^^^^^^^^^^^^^-------------------------------------------------------
^ ^ _k_ ^ ^      _g_oto      _n_ew       _f_ilter   _v_ersion   _q_uit
_h_ ^+^ _l_                _o_pen      _c_lear    _R_ename
^ ^ _j_ ^ ^                _a_rchive   _r_efresh
"
  ("j" next-line)
  ("k" previous-line)
  ("h" beginning-of-buffer)
  ("l" end-of-buffer)
  ("g" avy-goto-line)
  ("n" deft-new-file-named :color blue)
  ("f" deft-filter)
  ("c" deft-filter-clear)
  ("o" deft-open-file-other-window :color blue)
  ("r" deft-refresh)
  ("R" deft-rename-file)
  ("v" deft-show-version)
  ("a" deft-archive-file)
  ("q" nil :color blue))
(defun open-deft-and-start-hydra ()
  (interactive)
  (deft)
  (hydra-deft/body))
(define-key evil-normal-state-map (kbd "SPC t") 'open-deft-and-start-hydra)

;;; Org mode
;;; REPL

;; Eyebrowse mode
(sk/require-package 'eyebrowse)
(setq eyebrowse-wrap-around t
      eyebrowse-switch-back-and-forth t)
(defun diminish-eyebrowse ()
  (interactive)
  (require 'eyebrowse)
  (diminish 'eyebrowse-mode ""))
(add-hook 'eyebrowse-mode-hook 'diminish-eyebrowse)
(add-hook 'prog-mode-hook 'eyebrowse-mode)
(add-hook 'text-mode-hook 'eyebrowse-mode)

;; Hydra for compilation
(defhydra hydra-eyebrowse (:color blue
                           :hint nil)
  "
 ^Eyebrowse^
 ^^^^^^^^^---------------------
 _0_   _4_  _8_   sw_i_tch   _l_ast
 _1_   _5_  _9_   _r_ename   _q_uit
 _2_   _6_      _n_ext
 _3_   _7_      _p_revious
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
  ("n" eyebrowse-next-window-config)
  ("p" eyebrowse-prev-window-config)
  ("l" eyebrowse-last-window-config)
  ("q" nil))
(define-key evil-normal-state-map (kbd "SPC i") 'hydra-eyebrowse/body)

;;; Wrap up

;; Fill column indicator
(sk/require-package 'fill-column-indicator)
(setq fci-rule-width 5
      fci-rule-column 79)
(define-key evil-normal-state-map (kbd "SPC ax") 'fci-mode)

;; Column enforce column that highlights if I go over 100 characters
;; I try to stick within 80 characters but, frankly, I prefer 100.
;; Hence a compromise
(sk/require-package 'column-enforce-mode)
(require 'column-enforce-mode)
(setq column-enforce-column 99)
(defun diminish-column-enforce ()
  (interactive)
  (diminish 'column-enforce-mode ""))
(add-hook 'column-enforce-mode-hook 'diminish-column-enforce)
(add-hook 'prog-mode-hook 'column-enforce-mode)

;; Ledger mode for accounting
(sk/require-package 'ledger-mode)

;; Profiler
(sk/require-package 'esup)

;;; Construct mode hydras

;; Activate stuff
(defhydra hydra-activate (:color red
                          :hint nil)
  "
 ^(De)Activate^                  ^Packages^    ^Minor mode^
 ^^^^^^^^^----------------------------------------------------------------------
 _b_attery   _n_umber   scrollb_a_r  _p_aradox     _c_ompany    _i_ndentation   S_X_      _W_hich-key  _q_uit
 _t_ime      _w_rap     toolba_r_    instal_l_     _y_asnippet  _x_ FCI         _j_abber  _o_rg        col_f_orce
 _F_ont      _s_pell               _I_nitialize  _e_lpy       _G_eeknote      _F_old    _g_gtags
"
  ("b" display-battery-mode)
  ("t" display-time-mode)
  ("F" set-frame-font :color blue)
  ("n" linum-mode :color blue)
  ("w" toggle-truncate-lines :color blue)
  ("s" flyspell-mode :color blue)
  ("r" tool-bar-mode)
  ("a" scroll-bar-mode)
  ("p" paradox-list-packages :color blue)
  ("l" package-install :color blue)
  ("I" package-initialize :color blue)
  ("c" company-mode :color blue)
  ("f" column-enforce-mode :color blue)
  ("y" yas-global-mode :color blue)
  ("e" elpy-enable :color blue)
  ("i" highlight-indentation-mode :color blue)
  ("x" fci-mode)
  ("G" geeknote-create :color blue)
  ("X" sx-tab-all-questions :color blue)
  ("j" jabber-connect :color blue)
  ("o" sk/org-custom-load :color blue)
  ("g" ggtags-mode :color blue)
  ("F" global-origami-mode :color blue)
  ("W" which-key-mode :color blue)
  ("q" nil :color blue))
(define-key evil-normal-state-map (kbd "SPC a") 'hydra-activate/body)

;; Hydra of languages
(defhydra hydra-langs (:color blue
                       :hint nil)
  "
 ^Languages^                        ^Terminal^
 ^^^^^^^^^---------------------------------------
 _j_ulia      _m_atlab    _e_lisp   e_s_hell     multi-_t_erm
 _p_ython     _r_         _g_ist    tmu_x_       _q_uit
"
  ("j" hydra-julia/body :exit t)
  ("p" hydra-python/body :exit t)
  ("m" hydra-matlab/body :exit t)
  ("r" hydra-r/body :exit t)
  ("e" hydra-elisp/body :exit t)
  ("g" yagist-region-or-buffer :exit t)
  ("s" eshell-command :exit t)
  ("x" hydra-eamux/body :exit t)
  ("t" multi-term-here :exit t)
  ("q" nil))
(define-key evil-normal-state-map (kbd "SPC s") 'hydra-langs/body)
(define-key evil-visual-state-map (kbd "SPC s") 'hydra-langs/body)

;; Garbage collector - decrease threshold by an order
(setq gc-cons-threshold 10000000)

;; Start server - not very useful since I start emacs-mac these days
;; (server-start)
;;; .emacs ends here
