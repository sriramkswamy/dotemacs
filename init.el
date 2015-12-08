;; Garbage collector - increase threshold
(setq gc-cons-threshold 100000000)

;; Wrap init file
(let ((file-name-handler-alist nil)) "~/.emacs.d/init.el")

;;; Packages
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

;; Add homebrew packages
(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; Define function for a required package
(defun require-package (package)
  (setq-default highlight-tabs t)
  "Install given PACKAGE."
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

;; Mac stuff
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta))

;; Get the proper path
(require-package 'exec-path-from-shell)
(exec-path-from-shell-copy-env "PYTHONPATH")
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Enable winner-mode
(add-hook 'after-init-hook #'winner-mode)

;;; Hydra - Must have more than evil
(require-package 'hydra)

;;; GUI
;; No welcome screen - opens directly in scratch buffer
(setq inhibit-startup-message t
      initial-scratch-message ""
      initial-major-mode 'fundamental-mode
      visible-bell nil
      inhibit-splash-screen t)

;; Non-native fullscreen
(setq ns-use-native-fullscreen nil)
(defun toggle-frame-fullscreen-non-native ()
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

;; DocView Settings
(setq doc-view-continuous t
      doc-view-resolution 200)

;; Don't blink the cursor
(blink-cursor-mode -1)

;; Diminish minor modes
(require-package 'diminish)
(defun diminish-eldoc ()
  (interactive)
  (diminish 'eldoc-mode ""))
(add-hook 'eldoc-mode-hook 'diminish-eldoc)

;; Improve dired
(require-package 'dired+)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;;; Avy
(require-package 'avy)

;; Hydra - especially in emacs mode
(defhydra hydra-avy (:color red
                     :hint nil)
  "
 ^Line^    ^Char/Word^         ^Scroll^     ^Buffer^   ^Para^      ^Function^
 ^^^^^^^^^-------------------------------------------------------------------
 _l_ine    _c_haracter         _r_ecenter   _b_eg      _n_ext      _F_orward
 _q_uit    _d_ouble character  _N_ext       _e_nd      _p_revious  _B_ackward
 _H_ead    _w_ord              _P_revious   e_x_exute
"
  ("l" avy-goto-line)
  ("c" avy-goto-char)
  ("d" avy-goto-char-2)
  ("w" avy-goto-word-or-subword-1)
  ("s" avy-goto-subword-0)
  ("r" recenter-top-bottom)
  ("N" scroll-up)
  ("P" scroll-down)
  ("b" beginning-of-buffer)
  ("e" end-of-buffer)
  ("n" forward-paragraph)
  ("p" backward-paragraph)
  ("F" beginning-of-defun)
  ("B" end-of-defun)
  ("H" ak-hydra-of-hydras/body :exit t)
  ("x" counsel-M-x :color blue)
  ("q" nil :color blue))
(global-set-key (kbd "C-l") 'hydra-avy/body)

;; some helper functions for vi hydra
(defun vi-open-line-above ()
  "Insert a newline above the current line and put point at beginning."
  (interactive)
  (unless (bolp)
    (beginning-of-line))
  (newline)
  (forward-line -1)
  (indent-according-to-mode))
(defun vi-open-line-below ()
  "Insert a newline below the current line and put point at beginning."
  (interactive)
  (unless (eolp)
    (end-of-line))
  (newline-and-indent))
(defun vi-join-line ()
  "Join the current line with the next line"
  (interactive)
  (next-line)
  (delete-indentation))

;; some helper hydras for vi
(defhydra hydra-window-and-frame (:color red
                                  :hint nil)
  "
 ^Move^      ^Size^    ^Buffer^   ^Split^         ^Window^         ^Frame^     ^Text^
 ^^^^^^^^^^^^^^^--------------------------------------------------------------------------------
 ^ ^ _p_ ^ ^     ^ ^ _P_ ^ ^   _s_ave     _|_ vertical    _Z_ fullscreen   _M_aximize  _+_ zoom in
 _b_ ^+^ _f_     _B_ ^+^ _F_   b_u_ffers  ___ horizontal  _Q_ winner-undo  _m_inimize  _-_ zoom out
 ^ ^ _n_ ^ ^     ^ ^ _N_ ^ ^   _r_ecent   _O_nly          _R_ winner-redo  _S_et name
                   f_i_les    _q_uit          f_o_llow         _D_elete
"
  ("b" windmove-left)
  ("n" windmove-down)
  ("p" windmove-up)
  ("f" windmove-right)
  ("B" shrink-window-horizontally)
  ("N" enlarge-window)
  ("P" shrink-window)
  ("F" enlarge-window-horizontally)
  ("|" split-window-right)
  ("_" split-window-below)
  ("s" save-buffer)
  ("d" delete-window)
  ("Z" toggle-frame-fullscreen-non-native)
  ("O" delete-other-windows)
  ("Q" (progn
         (winner-undo)
         (setq this-command 'winner-undo)))
  ("R" winner-redo)
  ("M" toggle-frame-maximized)
  ("m" suspend-frame)
  ("D" delete-frame)
  ("S" set-frame-name)
  ("u" switch-to-buffer)
  ("r" recentf-open-files-item)
  ("i" find-file)
  ("o" follow-mode)
  ("+" text-scale-increase)
  ("-" text-scale-decrease)
  ("q" nil))

;; Vi - Hydra
(defhydra hydra-vi (:color red
                    :hint nil)
  "
^small^   ^large^   ^medium^    ^scroll^                      ^editing^                     ^search^      ^others^
^^^^^^^^^^^^^--------------------------------------------------------------------------------------------------------------
^ ^ _k_ ^ ^   ^ ^ _g_ ^ ^   ^ ^ _{_ ^ ^     ^ ^ _B_ ^ ^        _v_ mark           _s_ave       _o_pen line    _/_ forward     _t_ imenu     _q_uit
_h_ ^+^ _l_   _a_ ^+^ _e_   _w_ ^+^ _b_     ^ ^ ^+^ ^ ^        _x_ delete char    _J_oin       _O_pen line    _?_ backward    _f_ avy
^ ^ _j_ ^ ^   ^ ^ _G_ ^ ^   ^ ^ _}_ ^ ^     ^ ^ _F_ ^ ^        _d_elete           _S_plit      _>_ indent     _*_ at point    _W_indow
                            _z_ center   _y_ank             _p_ut        _=_ reindent
"
  ("l" forward-char)
  ("h" backward-char)
  ("j" next-line)
  ("k" previous-line)
  ("w" forward-word)
  ("b" backward-word)
  ("a" beginning-of-line)
  ("e" end-of-line)
  ("}" forward-paragraph)
  ("{" backward-paragraph)
  ("g" beginning-of-buffer)
  ("G" end-of-buffer)
  ("F" scroll-up)
  ("B" scroll-down)
  ("v" set-mark-command)
  ("x" delete-char)
  ("d" delete-region)
  ("y" kill-ring-save)
  ("s" save-buffer)
  ("p" yank)
  ("t" imenu)
  ("u" undo)
  ("R" redo)
  ("J" vi-join-line)
  ("S" reindent-then-newline-and-indent)
  ("/" isearch-forward-regexp)
  ("?" isearch-backward-regexp)
  ("*" isearch-forward-symbol-at-point)
  (":" execute-extended-command)
  (">" indent-region)
  ("=" indent-relative-maybe)
  ("z" recenter)
  ("n" isearch-occur)
  ("f" hydra-avy/body :exit t)
  ("W" hydra-window-and-frame/body :exit t)
  ("o" vi-open-line-below :color blue)
  ("O" vi-open-line-above :color blue)
  ("c" delete-char :color blue)
  ("i" nil :color blue)
  ("q" nil :color blue))
(global-set-key (kbd "C-r") 'hydra-vi/body)

;; Hydra - for elisp
(defhydra hydra-elisp (:color red
                       :hint nil)
  "
 ^Send^
 ^^^^^^^^^---------------------
 _f_unction    _L_ang     _q_uit
 _e_xp
 _r_egion
 _b_uffer
"
  ("f" eval-defun)
  ("e" eval-expression)
  ("r" eval-region)
  ("b" eval-buffer)
  ("L" hydra-langs/body :exit t)
  ("q" nil :color blue))

;;; Evil - Vim emulation layer
(require-package 'evil)
(setq evil-default-cursor t
      evil-want-C-u-scroll t
      evil-want-Y-yank-to-eol t)
(evil-mode 1)
;; Specify evil initial states
(evil-set-initial-state 'dired-mode 'emacs)
(evil-set-initial-state 'paradox-menu-mode 'emacs)
(evil-set-initial-state 'calendar-mode 'emacs)
(evil-set-initial-state 'diff-mode 'emacs)
(evil-set-initial-state 'speedbar-mode 'emacs)

;;; Paradox for package
(require-package 'paradox)
(define-key evil-normal-state-map (kbd "SPC al") 'paradox-list-packages)
(setq paradox-github-token t)

;; Themes
(require-package 'color-theme-modern)
(require-package 'zenburn-theme)
(require-package 'gruvbox-theme)
(load-theme 'leuven t t)
(enable-theme 'leuven)

;; Vertical split eshell
(defun eshell-vertical ()
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
(defun eshell-horizontal ()
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

;; Window manipulation - hydra - definition at the beginning
(define-key evil-normal-state-map (kbd "gw") 'hydra-window-and-frame/body)
(global-set-key (kbd "C-c C-h") 'hydra-window-and-frame/body)
(global-set-key (kbd "C-x C-h") 'hydra-window-and-frame/body)

;;; Which key
(require-package 'which-key)
(which-key-setup-side-window-bottom)
(defun diminish-which-key ()
  (interactive)
  (diminish 'which-key-mode ""))
(add-hook 'which-key-mode-hook 'diminish-which-key)

;; Move lines - from stack overflow
(defun move-text-internal (arg)
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
(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))
(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))
(define-key evil-normal-state-map (kbd "]x") 'move-text-down)
(define-key evil-normal-state-map (kbd "[x") 'move-text-up)

;; Inserting blank lines above and below - should do some mark trickery
(defun blank-line-up ()
  (interactive)
  (move-beginning-of-line nil)
  (newline))
(defun blank-line-down ()
  (interactive)
  (move-end-of-line nil)
  (newline)
  (forward-line -1))
(define-key evil-normal-state-map (kbd "]n") 'blank-line-down)
(define-key evil-normal-state-map (kbd "[n") 'blank-line-up)

;; Split windows and move
(defun split-below-and-move ()
  (interactive)
  (split-window-below)
  (other-window 1))
(defun split-right-and-move ()
  (interactive)
  (split-window-right)
  (other-window 1))

;;; Evil - Vim emulation - Continued
;; Escape for everything
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
;; Maps
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "Z") 'delete-other-windows)
(define-key evil-normal-state-map (kbd "Q") 'winner-undo)
(define-key evil-normal-state-map (kbd "R") 'winner-redo)
(define-key evil-normal-state-map (kbd "w") 'split-right-and-move)
(define-key evil-normal-state-map (kbd "W") 'split-below-and-move)
(define-key evil-normal-state-map (kbd "U") 'undo-tree-visualize)
(define-key evil-normal-state-map (kbd "K") 'man)
(define-key evil-normal-state-map (kbd "+") 'eshell-vertical)
(define-key evil-normal-state-map (kbd "-") 'eshell-horizontal)
(define-key evil-normal-state-map (kbd "\\") 'universal-argument)
(define-key evil-normal-state-map (kbd "gs") 'electric-newline-and-maybe-indent)
(define-key evil-normal-state-map (kbd "gl") 'browse-url-at-point)
(define-key evil-normal-state-map (kbd "gL") 'browse-url-at-mouse)
(define-key evil-normal-state-map (kbd "[F") 'delete-frame)
(define-key evil-normal-state-map (kbd "]F") 'make-frame)
(define-key evil-normal-state-map (kbd "SPC q") 'evil-quit)
(define-key evil-normal-state-map (kbd "SPC w") 'save-buffer)
(define-key evil-normal-state-map (kbd "SPC k") 'kill-buffer)
(define-key evil-normal-state-map (kbd "SPC z") 'toggle-frame-fullscreen-non-native)
(define-key evil-normal-state-map (kbd "SPC f") 'find-file)
(define-key evil-normal-state-map (kbd "SPC [") 'widen)
(define-key evil-normal-state-map (kbd "SPC 3") 'select-frame-by-name)
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
(require-package 'evil-surround)
(global-evil-surround-mode 1)

;; '%' matching like vim
(require-package 'evil-matchit)
(define-key evil-normal-state-map "%" #'evilmi-jump-items)
(define-key evil-inner-text-objects-map "%" #'evilmi-text-object)
(define-key evil-outer-text-objects-map "%" #'evilmi-text-object)
(global-evil-matchit-mode 1)

;; Evil args
(require-package 'evil-args)
(define-key evil-inner-text-objects-map "," #'evil-inner-arg)
(define-key evil-outer-text-objects-map "," #'evil-outer-arg)
(define-key evil-normal-state-map "\C-j" #'evil-jump-out-args)

;; Jump lists like vim
(require-package 'evil-jumper)
(global-evil-jumper-mode 1)

;; Evil commentary
(require-package 'evil-commentary)
(defun diminish-evil-commentary ()
  (interactive)
  (diminish 'evil-commentary-mode ""))
(add-hook 'evil-commentary-mode-hook 'diminish-evil-commentary)
(evil-commentary-mode)

;; Evil exchange
(require-package 'evil-exchange)
(evil-exchange-install)

;; Increment and decrement numbers like vim
(require-package 'evil-numbers)
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
(require-package 'sentence-navigation)
(define-key evil-normal-state-map ")" 'sentence-nav-evil-forward)
(define-key evil-normal-state-map "(" 'sentence-nav-evil-backward)
(define-key evil-normal-state-map "g)" 'sentence-nav-evil-forward-end)
(define-key evil-normal-state-map "g(" 'sentence-nav-evil-backward-end)
(define-key evil-outer-text-objects-map "s" 'sentence-nav-evil-outer-sentence)
(define-key evil-inner-text-objects-map "s" 'sentence-nav-evil-inner-sentence)

;;; Navigation

;; Neotree
(require-package 'neotree)
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

;; Enable recentf mode
(recentf-mode)

;; Backups at .saves folder in the current folder
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;; Ediff plain window and vertical
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;; Remote file navigation
(require-package 'tramp)
(setq tramp-ssh-controlmaster-options "ssh")

;; Very large file viewing
(require-package 'vlf)

;;; ag
(require-package 'ag)
(define-key evil-normal-state-map (kbd "SPC 7") 'ag-project-regexp)
(define-key evil-visual-state-map (kbd "SPC 7") 'ag-project-regexp)

;;; wgrep-ag
(require-package 'wgrep-ag)

;;; Swiper (with Ivy and counsel)
(require-package 'swiper)
(require-package 'counsel)
(setq ivy-display-style 'fancy
      ivy-height 15)
;; Fuzzy for M-x
(setq ivy-re-builders-alist
      '((counsel-M-x . ivy--regex-fuzzy)
        (t . ivy--regex-plus)))
(defun diminish-ivy ()
  (interactive)
  (diminish 'ivy-mode ""))
(add-hook 'ivy-mode-hook 'diminish-ivy)
(ivy-mode 1)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-q") 'counsel-M-x)
(define-key evil-normal-state-map (kbd "SPC d") 'counsel-M-x)
(define-key evil-normal-state-map (kbd "t") 'imenu)
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

;; Swiper helpers
(defun swiper-at-point ()
  (swiper symbol-at-point))

;; Help
(defhydra hydra-apropos (:color blue
                         :hint nil)
  "
 ^Apropos^
 ^^^^^^^^^------------------------------------------------------------
 _a_propos   _d_oc   _v_ar   _c_md   _l_ib   _u_ser-option   valu_e_   _q_uit
"
  ("a" apropos)
  ("d" apropos-documentation)
  ("v" apropos-variable)
  ("c" apropos-command)
  ("l" apropos-library)
  ("u" apropos-user-option)
  ("e" apropos-value)
  ("q" nil))
(defhydra hydra-help (:color blue
                      :hint nil)
  "
 ^Help^
 ^^^^^^^^^---------------------------------------------------
 _b_inding    _i_nfo     _t_utorial  _a_ll            _q_uit
 _f_unction   _s_ymbol   _p_ackage   _l_anguage env
 _v_ariable   _e_macs    _h_elp
 _m_ode       synta_x_   _k_ey
"
  ("b" describe-binding)
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
  ("a" hydra-apropos/body :exit t)
  ("l" describe-language-environment)
  ("q" nil))
(define-key evil-normal-state-map (kbd "SPC x") 'hydra-help/body)

;; Bookmarks - hydra
(defhydra hydra-bookmarks (:color red
                              :hint nil)
  "
 ^Bookmarks^
 ^^^^^^^^^-----------------------------
 _s_et  _S_ave  _j_ump  _d_elete  _q_uit
  "
  ("s" bookmark-set)
  ("S" bookmark-save)
  ("j" bookmark-jump)
  ("d" bookmark-delete)
  ("q" nil :color blue))
(define-key evil-normal-state-map (kbd "SPC `") 'hydra-bookmarks/body)

;; Find file in project
(require-package 'find-file-in-project)
(define-key evil-normal-state-map (kbd "SPC p") 'find-file-in-project)
(define-key evil-normal-state-map (kbd "SPC TAB") 'ff-find-other-file)

;;; Swoop
(require-package 'swoop)
(define-key evil-normal-state-map (kbd "*") 'swoop-pcre-regexp)
(define-key evil-normal-state-map (kbd "#") 'swoop-pcre-regexp)
(define-key evil-visual-state-map (kbd "*") 'swoop-pcre-regexp)
(define-key evil-visual-state-map (kbd "#") 'swoop-pcre-regexp)

;;; Dash at point
(require-package 'dash-at-point)
(define-key evil-normal-state-map (kbd "SPC 1") 'dash-at-point-with-docset)

;;; Visual regexp
(require-package 'visual-regexp)
(require-package 'visual-regexp-steroids)
(define-key evil-normal-state-map (kbd "SPC 5") 'vr/select-query-replace)
(define-key evil-visual-state-map (kbd "SPC 5") 'vr/select-query-replace)

;;; Spotlight
(require-package 'spotlight)
(define-key evil-normal-state-map (kbd "SPC 8") 'spotlight)
;;; Reveal in Finder
(require-package 'reveal-in-osx-finder)
(define-key evil-normal-state-map (kbd "gF") 'reveal-in-osx-finder)

;;; Manage external services
(require-package 'prodigy)

;;; Cmake ide
(require-package 'cmake-ide)

;;; GTags
(require-package 'ggtags)
(defun diminish-ggtags ()
  (interactive)
  (diminish 'ggtags-mode ""))
(add-hook 'ggtags-mode-hook 'diminish-ggtags)
(add-hook 'prog-mode-hook 'ggtags-mode)
;; Add exec-path for Gtags
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/Cellar/global/6.5/bin"))
(setq exec-path (append exec-path '("/usr/local/Cellar/global/6.5/bin")))
(setq-local imenu-create-index-function #'ggtags-build-imenu-index)
(define-key evil-normal-state-map (kbd "T") 'ggtags-find-tag-regexp)

;;; Interact with OS services
;; Jabber
(require-package 'jabber)
(setq jabber-history-enabled t
      jabber-activity-mode nil
      jabber-use-global-history nil
      jabber-backlog-number 40
      jabber-backlog-days 30)
(setq jabber-alert-presence-message-function
      (lambda (who oldstatus newstatus statustext) nil))
(define-key evil-normal-state-map (kbd "SPC 2") 'jabber-chat-with)

;; Google under point
(require-package 'google-this)
(define-key evil-normal-state-map (kbd "SPC 9") 'google-this-search)
(define-key evil-visual-state-map (kbd "SPC 9") 'google-this)

;; Evernote with geeknote
(require-package 'geeknote)
(define-key evil-normal-state-map (kbd "SPC ae") 'geeknote-create)

;; Stackexchange
(require-package 'sx)
(define-key evil-normal-state-map (kbd "SPC aq") 'sx-tab-all-questions)

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

;; Narrow to region
(put 'narrow-to-region 'disabled nil)

;; Spell check
(add-hook 'text-mode-hook 'flyspell-mode)
(defun diminish-flyspell ()
  (interactive)
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
(require-package 'origami)
(define-key evil-normal-state-map (kbd "]z") 'origami-next-fold)
(define-key evil-normal-state-map (kbd "[z") 'origami-previous-fold)
(define-key evil-normal-state-map (kbd "zx") 'origami-toggle-node)
(define-key evil-normal-state-map (kbd "zs") 'origami-show-only-node)
(define-key evil-normal-state-map (kbd "zg") 'origami-toggle-all-nodes)
(define-key evil-normal-state-map (kbd "zu") 'origami-undo)
(define-key evil-normal-state-map (kbd "zr") 'origami-redo)
(define-key evil-normal-state-map (kbd "zf") 'origami-close-node)
(define-key evil-normal-state-map (kbd "zd") 'origami-open-node)

;; Highlight stuff
(require-package 'volatile-highlights)
(require 'volatile-highlights)
(defun diminish-volatile-highlights ()
  (interactive)
  (diminish 'volatile-highlights-mode ""))
(add-hook 'volatile-highlights-mode-hook 'diminish-volatile-highlights)
(volatile-highlights-mode t)

;; Smart tabs
(require-package 'smart-tab)
(defun diminish-smart-tab ()
  (interactive)
  (diminish 'smart-tab-mode ""))
(add-hook 'smart-tab-mode-hook 'diminish-smart-tab)
(global-smart-tab-mode)

;;; Multiple cursors
(require-package 'multiple-cursors)

;; Hydra for multiple-cursors
(defhydra hydra-mc (:color red
                    :hint nil)
  "
^Mark^             ^Unmark^       ^Lines^
^^^^^^^^^^^^^^^------------------------------------------------
^ ^ _k_ ^ ^     _a_ll    _p_revious     _e_dit    _N_umbers   _q_uit
_h_ ^+^ _l_            _n_ext         _A_ppend  _L_etters   _c_hange
^ ^ _j_ ^ ^                         _I_nsert            _K_ill
"
  ("j" mc/mark-next-like-this)
  ("k" mc/mark-previous-like-this)
  ("h" mc/skip-to-next-like-this)
  ("l" mc/skip-to-previous-like-this)
  ("a" mc/mark-all-like-this)
  ("p" mc/unmark-previous-like-this)
  ("n" mc/unmark-next-like-this)
  ("e" mc/edit-lines :color blue)
  ("I" mc/edit-beginnings-of-lines :color blue)
  ("A" mc/edit-ends-of-lines :color blue)
  ("N" mc/insert-numbers :color blue)
  ("L" mc/insert-letters :color blue)
  ("c" nil :color blue)
  ("K" mc/keyboard-quit)
  ("q" nil :color blue))
(define-key evil-normal-state-map (kbd "gm") 'hydra-mc/body)
(define-key evil-visual-state-map (kbd "gm") 'hydra-mc/body)

;;; Expand regions
(require-package 'expand-region)

;; hydra for expand regions python
(defhydra hydra-ex-py (:color red
                    :hint nil)
  "
^expand^                   ^block^     ^string^
^^^^^^^^^^--------------------------------------------
_B_lock and dec  _g_oto      _i_nside    _I_nside   _q_uit
_l_ine           _E_xpand    _o_utside   _O_utside
"
  ("B" er/mark-python-block-and-decorator)
  ("l" er/mark-python-statement)
  ("g" hydra-avy/body :exit t)
  ("i" er/mark-python-block)
  ("o" er/mark-outer-python-block)
  ("I" er/mark-inside-python-string)
  ("O" er/mark-outside-python-string)
  ("E" hydra-ex/body :exit t)
  ("q" nil :color blue))

;; hydra for expand regions
(defhydra hydra-ex (:color red
                    :hint nil)
  "
^expand^               ^semantics^                  ^Pairs^    ^Quotes^    ^Lang^
^^^^^^^^^^----------------------------------------------------------------------------------
_e_xpand    _c_ontract    _p_ara   _s_ymbol   _S_entence  _i_nside   _I_nside    _P_ython    _q_uit
_r_estart   _g_oto        _w_ord   _u_rl      _C_omment   _o_utside  _O_utside
"
  ("e" er/expand-region)
  ("c" er/contract-region)
  ("r" set-mark)
  ("g" hydra-avy/body :exit t)
  ("C" er/mark-comment)
  ("p" er/mark-paragraph)
  ("f" er/mark-defun)
  ("s" er/mark-symbol)
  ("S" er/mark-sentence)
  ("w" er/mark-word)
  ("u" er/mark-url)
  ("i" er/mark-inside-pairs)
  ("o" er/mark-outside-pairs)
  ("I" er/mark-inside-quotes)
  ("O" er/mark-outside-quotes)
  ("P" hydra-ex-py/body :exit t)
  ("q" nil :color blue))
(define-key evil-normal-state-map (kbd "ge") 'hydra-ex/body)
(define-key evil-visual-state-map (kbd "ge") 'hydra-ex/body)

;; Flx with company
(require-package 'flx)
(require-package 'company-flx)

;;; Company
(require-package 'company)
(with-eval-after-load 'company
  (company-flx-mode +1))
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-files))
(setq company-idle-delay 0
      company-minimum-prefix-length 1
      company-require-match 0
      company-selection-wrap-around t
      company-dabbrev-downcase nil)
;; Maps
(global-set-key [(control return)] 'company-complete-common-or-cycle)
(define-key evil-normal-state-map (kbd "SPC ac") 'company-mode)
(defun my-company-hook ()
  (interactive)
  (diminish 'company-mode " ς")
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map [tab] 'company-complete-common-or-cycle))
(add-hook 'company-mode-hook 'my-company-hook)
(add-hook 'prog-mode-hook 'company-mode)

;; Company C headers
(require-package 'company-c-headers)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-c-headers))

;; Irony mode for C++
(require-package 'irony)
(defun diminish-irony ()
  (interactive)
  (diminish 'irony-mode " Γ"))
(add-hook 'irony-mode-hook 'diminish-irony)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; Company irony
(require-package 'company-irony)
(require-package 'company-irony-c-headers)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

;; Support auctex
(require-package 'company-auctex)

;; Support Go
(require-package 'company-go)

;; Support inf-ruby
(require-package 'company-inf-ruby)

;; Support tern
(require-package 'company-tern)

;; Support web mode
(require-package 'company-web)

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

;;; YASnippet
(require-package 'yasnippet)
;; Add yasnippet support for all company backends
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")
(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))
(defun yas-company-hook ()
  (interactive)
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))
(add-hook 'company-mode-hook 'yas-company-hook)
;; Just enable helm/ivy/ido and this uses them automatically
(setq yas-prompt-functions '(yas-completing-prompt))
;; Disable in shell
(defun force-yasnippet-off ()
  (yas-minor-mode -1)
  (setq yas-dont-activate t))
(add-hook 'term-mode-hook 'force-yasnippet-off)
(add-hook 'shell-mode-hook 'force-yasnippet-off)
(defun diminish-yas ()
  (interactive)
  (diminish 'yas-minor-mode " γ"))
(add-hook 'yas-global-mode-hook 'diminish-yas)
(add-hook 'prog-mode-hook 'yas-global-mode)
(define-key evil-normal-state-map (kbd "SPC ay") 'yas-global-mode)
(define-key evil-insert-state-map (kbd "C-j") 'yas-insert-snippet)

;;; Language and Syntax

;; ESS - Emacs speaks statistics
(require-package 'ess)
(add-to-list 'auto-mode-alist '("\\.R$" . R-mode))
;; Vertical split R shell
(defun r-shell-here ()
  "opens up a new r shell in the directory associated with the current buffer's file."
  (interactive)
  (require 'ess-site)
  (split-window-right)
  (other-window 1)
  (R)
  (other-window 1))
;; Vertical split julia REPL
(defun julia-shell-here ()
  "opens up a new julia REPL in the directory associated with the current buffer's file."
  (interactive)
  (require 'ess-site)
  (split-window-right)
  (other-window 1)
  (julia)
  (other-window 1))

;; Hydra - for julia
(defhydra hydra-julia (:color red
                       :hint nil)
  "
 ^Send^       ^Shell^
 ^^^^^^^^^-------------------------
 _f_unction   _s_tart    _L_ang    _q_uit
 _l_ine       _S_witch
 _r_egion     _o_ther
 _b_uffer
"
  ("f" ess-eval-function)
  ("l" ess-eval-line)
  ("r" ess-eval-region)
  ("b" ess-eval-buffer)
  ("s" julia-shell-here)
  ("S" ess-switch-to-ESS)
  ("o" other-window)
  ("L" hydra-langs/body :exit t)
  ("q" nil :color blue))

;; Hydra - for r
(defhydra hydra-r (:color red
                   :hint nil)
  "
 ^Send^       ^Shell^
 ^^^^^^^^^-------------------------
 _f_unction   _s_tart    _L_ang    _q_uit
 _l_ine       _S_witch
 _r_egion     _o_ther
 _b_uffer
"
  ("f" ess-eval-function)
  ("l" ess-eval-line)
  ("r" ess-eval-region)
  ("b" ess-eval-buffer)
  ("s" r-shell-here)
  ("S" ess-switch-to-ESS)
  ("o" other-window)
  ("L" hydra-langs/body :exit t)
  ("q" nil :color blue))

;; Lisp
(require-package 'paredit)

;; Slime
(require-package 'slime)

;; Cider
(require-package 'cider)

;; Geiser for scheme-mode
(require-package 'geiser)

;; Haskell
(require-package 'haskell-mode)

;; Markdown
(require-package 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Pandoc mode
(require-package 'pandoc-mode)
(add-hook 'markdown-mode-hook 'pandoc-mode)

;; Highlight indentation
(require-package 'highlight-indentation)
(define-key evil-normal-state-map (kbd "SPC ai") 'highlight-indentation-mode)

;; Elpy
(require-package 'elpy)
(add-hook 'python-mode-hook 'elpy-enable)
(add-hook 'python-mode-hook 'elpy-use-ipython)
(defun diminish-elpy ()
  (interactive)
  (diminish 'elpy-mode ""))
(add-hook 'elpy-mode-hook 'diminish-elpy)

;; Hydra - for python
(defhydra hydra-python (:color red
                        :hint nil)
  "
 ^Send^       ^Shell^    ^Navigate^
 ^^^^^^^^^----------------------------------------------
 _f_unction   _s_tart    _d_efinition   _L_ang     _q_uit
 _l_ine       _S_witch   l_o_cation
 _r_egion     _B_uffer
 _b_uffer
"
  ("f" python-shell-send-defun)
  ("l" elpy-shell-send-current-statement)
  ("r" elpy-shell-send-region)
  ("b" elpy-shell-send-buffer)
  ("s" elpy-shell-switch-to-shell)
  ("S" elpy-shell-switch-to-shell)
  ("B" elpy-shell-switch-to-buffer)
  ("d" elpy-goto-definition)
  ("o" elpy-goto-location)
  ("L" hydra-langs/body :exit t)
  ("q" nil :color blue))

;; Cython mode
(require-package 'cython-mode)

;; Virtualenv for python
(require-package 'virtualenvwrapper)
(setq venv-location "~/Py34/")

;; LaTeX-mode
(require-package 'auctex)
(require-package 'auctex-latexmk)

;; Web mode
(require-package 'web-mode)
(add-hook 'html-mode-hook 'web-mode)

;; JavaScript
(require-package 'js2-mode)
(require-package 'skewer-mode)

;; Applescript
(require-package 'applescript-mode)
(add-to-list 'auto-mode-alist '("\\.scpt\\'" . applescript-mode))

;; YAML mode
(require-package 'yaml-mode)

;; Ruby
(require-package 'inf-ruby)

;; Editing my gitconfig
(require-package 'gitconfig-mode)

;; Sage
(require-package 'sage-shell-mode)
(setq sage-shell:sage-executable "/Applications/Sage-6.8.app/Contents/Resources/sage/sage"
      sage-shell:input-history-cache-file "~/.emacs.d/.sage_shell_input_history"
      sage-shell-sagetex:auctex-command-name "LaTeX"
      sage-shell-sagetex:latex-command "latexmk")

;; SQL
(require-package 'emacsql)
(require-package 'emacsql-mysql)
(require-package 'emacsql-sqlite)
(require-package 'esqlite)
(require-package 'pcsv)

;; Go mode
(require-package 'go-mode)

;; Java
(require-package 'emacs-eclim)
(defun my-eclim-mode-hook ()
  (interactive)
  (require 'eclimd)
  (require 'company-emacs-eclim)
  (setq eclim-executable (or (executable-find "eclim") "/Applications/Eclipse/Eclipse.app/Contents/Eclipse/eclim")
        eclimd-executable (or (executable-find "eclimd") "/Applications/Eclipse/Eclipse.app/Contents/Eclipse/eclimd")
        eclimd-wait-for-process nil
        eclimd-default-workspace "~/Documents/workspace/eclipse/")
  (company-emacs-eclim-setup))
(add-hook 'eclim-mode-hook 'my-eclim-mode-hook)
(add-hook 'java-mode-hook 'eclim-mode)

;;; Flycheck
(require-package 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(define-key evil-normal-state-map (kbd "]e") 'flycheck-next-error)
(define-key evil-normal-state-map (kbd "[e") 'flycheck-previous-error)
(define-key evil-normal-state-map (kbd "]E") 'flycheck-last-checker)
(define-key evil-normal-state-map (kbd "[E") 'flycheck-first-error)
(define-key evil-normal-state-map (kbd "SPC l") 'flycheck-list-errors)

;; Irony for flycheck
(require-package 'flycheck-irony)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;;; Deft for quickly accessing notes
(require-package 'deft)
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
(define-key evil-normal-state-map (kbd "goj") 'org-goto)
(define-key evil-normal-state-map (kbd "goU") 'org-update-all-dblocks)
(define-key evil-normal-state-map (kbd "gog") 'org-toggle-link-display)
(define-key evil-normal-state-map (kbd "gor") 'org-reveal)
(define-key evil-normal-state-map (kbd "gof") 'org-refile)
(define-key evil-normal-state-map (kbd "goX") 'org-reftex-citation)
(define-key evil-normal-state-map (kbd "goa") 'org-attach)
(define-key evil-normal-state-map (kbd "goA") 'org-archive-subtree-default)
(define-key evil-normal-state-map (kbd "gon") 'org-add-note)
(define-key evil-normal-state-map (kbd "goF") 'org-footnote-new)
(define-key evil-normal-state-map (kbd "goe") 'org-export-dispatch)
(define-key evil-normal-state-map (kbd "gol") 'org-insert-link)
(define-key evil-normal-state-map (kbd "gou") 'org-store-link)
(define-key evil-normal-state-map (kbd "goO") 'org-open-at-point)
(define-key evil-normal-state-map (kbd "goy") 'org-copy-subtree)
(define-key evil-normal-state-map (kbd "gok") 'org-cut-subtree)
(define-key evil-normal-state-map (kbd "goE") 'org-set-effort)
(define-key evil-normal-state-map (kbd "goh") 'org-toggle-heading)
(define-key evil-normal-state-map (kbd "go>") 'org-goto-calendar)
(define-key evil-normal-state-map (kbd "go<") 'org-date-from-calendar)
(define-key evil-normal-state-map (kbd "gos") 'org-sort)
(define-key evil-normal-state-map (kbd "goR") 'org-remove-file)
(define-key evil-visual-state-map (kbd "SPC c") 'org-capture)
(define-key evil-visual-state-map (kbd "SPC o") 'org-agenda)

;; Correct those annoying double caps typos
(defun dcaps-to-scaps ()
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
      (add-hook 'post-self-insert-hook #'dcaps-to-scaps nil 'local)
    (remove-hook 'post-self-insert-hook #'dcaps-to-scaps 'local)))
(defun diminish-dubcaps ()
  (interactive)
  (diminish 'dubcaps-mode ""))
(add-hook 'dubcaps-mode-hook 'diminish-dubcaps)
(add-hook 'org-mode-hook #'dubcaps-mode)

;; Turns the next page in adjoining pdf-tools pdf
(defun other-pdf-next ()
  "Turns the next page in adjoining PDF file"
  (interactive)
  (other-window 1)
  (doc-view-next-page)
  (other-window 1))
;; Turns the previous page in adjoining pdf
(defun other-pdf-previous ()
  "Turns the previous page in adjoining PDF file"
  (interactive)
  (other-window 1)
  (doc-view-previous-page)
  (other-window 1))
(define-key evil-normal-state-map (kbd "]p") 'other-pdf-next)
(define-key evil-normal-state-map (kbd "[p") 'other-pdf-previous)

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
                            ("personal" . ?m)
                            ("report" . ?r)
                            ("thesis" . ?t) ;; temporary
                            ("accounts" . ?a)
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

(setq org-agenda-files (list "~/Dropbox/notes/work.org"
                             "~/Dropbox/notes/blog.org"
                             "~/Dropbox/notes/ledger.org"
                             "~/Dropbox/notes/notes.org"))

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

;; Calendar
(require-package 'org-caldav)

;; LaTeX
(require-package 'cdlatex)
(defun diminish-org ()
  (interactive)
  (diminish 'org-indent-mode "")
  (diminish 'org-cdlatex-mode ""))
(add-hook 'org-mode-hook 'diminish-org)
(add-hook 'org-mode-hook 'org-cdlatex-mode)

;; Pomodoro
(require-package 'org-pomodoro)

;; Babel for languages
(require-package 'babel)
(setq org-confirm-babel-evaluate nil)
;; Org load languages
(defun org-custom-load ()
  (interactive)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     ;; (dot . t)
     ;; (ditaa . t)
     (latex . t)
     ;; (gnuplot . t)
     ;; (sh . t)
     ;; (C . t)
     ;; (R . t)
     ;; (octave . t)
     (matlab . t)
     (python . t))))
(define-key evil-normal-state-map (kbd "SPC ao") 'org-custom-load)
(require-package 'ob-ipython)

;; Export using reveal and impress-js
(require-package 'ox-reveal)
(require-package 'ox-impress-js)

;; Restructred text and pandoc
(require-package 'ox-rst)
(require-package 'ox-pandoc)

;; Trello
(require-package 'org-trello)

;; Org navigation and manipulation - hydra
(defhydra hydra-org-manipulate (:color red
                                :hint nil)
  "
^Meta^      ^Shift^             ^Org^
^^^^^^^^^^^^^^^-----------------------------------------
^ ^ _k_ ^ ^     ^ ^ _K_ ^ ^     _f_ promote    _c_ycle
_h_ ^+^ _l_     _H_ ^+^ _L_     _b_ demote     _C_olumns
^ ^ _j_ ^ ^     ^ ^ _J_ ^ ^     _n_ item down  _q_uit
                    _p_ item up
"
  ("c" org-cycle)
  ("h" org-metaleft)
  ("l" org-metaright)
  ("j" org-metadown)
  ("k" org-metaup)
  ("H" org-shiftleft)
  ("L" org-shiftright)
  ("J" org-shiftdown)
  ("K" org-shiftup)
  ("f" org-promote)
  ("b" org-demote)
  ("n" org-move-item-down)
  ("p" org-move-item-up)
  ("C" org-columns)
  ("q" nil :color blue))
(define-key evil-normal-state-map (kbd "goo") 'hydra-org-manipulate/body)

;; Org table manipulation - hydra
(defhydra hydra-org-tables (:color red
                            :hint nil)
  "
^Field^      ^Move^    ^Insert^      ^Delete^   ^Field^     ^Table^     ^Formula^
^^^^^^^^^^^^^^^------------------------------------------------------------------------------
^ ^ _k_ ^ ^     ^ ^ _K_ ^ ^     _r_ow        _R_ow      _e_dit      _a_lign      _s_um      _|_ create
_h_ ^+^ _l_     _H_ ^+^ _L_     _c_olumn     _C_olumn   _b_lank     _I_mport     _f_ eval   _q_uit
^ ^ _j_ ^ ^     ^ ^ _J_ ^ ^     _-_ hline             _i_nfo      _E_xport     _F_ edit
"
  ("a" org-table-align)
  ("l" org-table-next-field)
  ("h" org-table-previous-field)
  ("j" org-table-end-of-field)
  ("k" org-table-beginning-of-field)
  ("r" org-table-insert-row)
  ("c" org-table-insert-column)
  ("-" org-table-insert-hline)
  ("J" org-table-move-row-down)
  ("K" org-table-move-row-up)
  ("H" org-table-move-column-left)
  ("L" org-table-move-column-right)
  ("R" org-table-kill-row)
  ("C" org-table-delete-column)
  ("b" org-table-blank-field)
  ("e" org-table-edit-field)
  ("i" org-table-field-info)
  ("s" org-table-sum)
  ("f" org-table-eval-formula)
  ("F" org-table-edit-formulas)
  ("|" org-table-create-or-convert-from-region)
  ("I" org-table-import)
  ("E" org-table-export)
  ("q" nil :color blue))
(define-key evil-normal-state-map (kbd "go|") 'hydra-org-tables/body)

;; Org clock manipulation - hydra
(defhydra hydra-org-clock (:color red
                           :hint nil)
  "
       ^Clock^                 ^Timer^
^^^^^^^^^^^^^^^-----------------------------------------------------
_i_in      _z_ resolve      _I_n       _s_tamp           _q_uit
_o_out     _l_ast           _O_out     _S_tamp inactive
_r_eport   _C_ancel         _t_imer
_d_isplay  _G_oto           _T_ set timer
"
  ("i" org-clock-in)
  ("o" org-clock-out)
  ("r" org-clock-report)
  ("z" org-resolve-clocks)
  ("C" org-clock-cancel)
  ("d" org-clock-display)
  ("l" org-clock-in-last)
  ("G" org-clock-goto)
  ("t" org-timer)
  ("T" org-timer-set-timer)
  ("I" org-timer-start)
  ("O" org-timer-stop)
  ("s" org-time-stamp)
  ("S" org-time-stamp-inactive)
  ("q" nil :color blue))
(define-key evil-normal-state-map (kbd "goc") 'hydra-org-clock/body)

;; Org tags and todo manipulation - hydra
(defhydra hydra-org-tag-todo (:color red
                              :hint nil)
  "
 ^tags^        ^TODO^        ^Checkbox^        ^Priority^
^^^^^^^^^^^^^^^----------------------------------------------------------
_t_ags         _T_ODO        _c_heckbox        _p_riority     _q_uit
_v_iew         _D_eadline    _x_ toggle        _i_ncrease
_m_atch        _C_lose       _u_pdate stats    _d_ecrease
_s_parse-tree  _S_chedule    _r_eset
             _V_iew        _U_pdate count
"
  ("t" org-set-tags-command :color blue)
  ("v" org-tags-view)
  ("m" org-match-sparse-tree :color blue)
  ("s" org-sparse-tree :color blue)
  ("p" org-priority)
  ("i" org-priority-up)
  ("d" org-priority-down)
  ("T" org-todo :color blue)
  ("D" org-deadline :color blue)
  ("C" org-deadline-close :color blue)
  ("S" org-schedule :color blue)
  ("V" org-check-deadlines)
  ("c" org-checkbox)
  ("x" org-toggle-checkbox)
  ("U" org-update-checkbox-count-maybe)
  ("r" org-reset-checkbox-state-subtree)
  ("u" org-update-statistics-cookies)
  ("q" nil :color blue))
(define-key evil-normal-state-map (kbd "got") 'hydra-org-tag-todo/body)
;; Org drawer - hydra
(defhydra hydra-org-drawer (:color red
                            :hint nil)
  "
 ^drawer^     ^Property^
^^^^^^^^^^^^^--------------------------
 _i_nsert     _I_nsert   _q_uit
            _S_et
            _D_elete
            _T_oggle
"
  ("i" org-insert-drawer)
  ("I" org-insert-property-drawer)
  ("S" org-set-property)
  ("D" org-delete-property)
  ("T" org-toggle-ordered-property)
  ("q" nil :color blue))
(define-key evil-normal-state-map (kbd "god") 'hydra-org-drawer/body)

;;; Version control

;; Magit
(require-package 'magit)
(define-key evil-normal-state-map (kbd "SPC g") 'magit-status)
;; Hydra for blame
(defhydra hydra-git-blame (:color red
                           :hint nil)
  "
 ^blame^
^^^^^^^^^--------------------------------
 ^ ^ _k_ ^ ^  _b_lame   _t_oggle   _y_ank
 _h_ ^+^ _l_  _q_uit
 ^ ^ _j_ ^ ^
"
  ("b" magit-blame)
  ("j" magit-blame-next-chunk)
  ("k" magit-blame-previous-chunk)
  ("l" magit-blame-next-chunk-same-commit)
  ("h" magit-blame-previous-chunk-same-commit)
  ("t" magit-blame-toggle-headings)
  ("y" magit-blame-copy-hash)
  ("q" magit-blame-quit :color blue))
(define-key evil-normal-state-map (kbd "gb") 'hydra-git-blame/body)

;; Diff-hl
(require-package 'diff-hl)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
(add-hook 'prog-mode-hook 'diff-hl-mode)
(add-hook 'html-mode-hook 'diff-hl-mode)
(add-hook 'text-mode-hook 'diff-hl-mode)
(add-hook 'org-mode-hook 'diff-hl-mode)
(add-hook 'diff-hl-mode-hook 'diff-hl-margin-mode)
(add-hook 'diff-hl-mode-hook 'diff-hl-flydiff-mode)
(define-key evil-normal-state-map (kbd "]d") 'diff-hl-next-hunk)
(define-key evil-normal-state-map (kbd "[d") 'diff-hl-previous-hunk)
(define-key evil-normal-state-map (kbd "gh") 'diff-hl-diff-goto-hunk)
(define-key evil-normal-state-map (kbd "gr") 'diff-hl-revert-hunk)

;; Git time-machine
(require-package 'git-timemachine)
;; Hydra for timemachine
(defhydra hydra-git-timemachine (:color red
                                 :hint nil)
  "
 ^time^    ^navigate^            ^hash^
^^^^^^^^^---------------------------------
 _s_tart   _n_ext      _g_oto      _b_rief
 _k_ill    _p_revious  _c_urrent   _f_ull
"
  ("s" git-timemachine)
  ("n" git-timemachine-show-next-revision)
  ("p" git-timemachine-show-previous-revision)
  ("g" git-timemachine-show-nth-revision)
  ("c" git-timemachine-show-current-revision)
  ("b" git-timemachine-kill-abbreviated-revision)
  ("f" git-timemachine-kill-revision)
  ("k" git-timemachine-quit :color blue)
  ("q" nil :color blue))
(define-key evil-normal-state-map (kbd "gt") 'hydra-git-timemachine/body)

;; Gists
(require-package 'yagist)
(setq yagist-view-gist t)

;;; REPL

;; Quickrun
(require-package 'quickrun)

;; Compile and multi-compile
(require-package 'multi-compile)
(setq multi-compile-alist '(
                            (c++-mode . (("cpp-omp" . "g++ %file-name -Wall -fopenmp -o -g %file-sans.out")
                                         ("cpp-mpi" . "mpic++ %file-name -o -g %file-sans.out")
                                         ("cpp-g++" . "g++ %file-name -o %file-sans.out")))
                            (c-mode . (("c-omp" . "gcc %file-name -Wall -fopenmp -o -g %file-sans.out")
                                       ("c-mpi" . "mpicc %file-name -o -g %file-sans.out")
                                       ("c-gcc" . "gcc %file-name -o %file-sans.out")))
                            ))
(setq multi-compile-completion-system 'default)

;; Hydra for compilation
(defhydra hydra-make (:color blue
                      :hint nil)
  "
 ^Multi-compile^
 ^^^^^^^^^---------------------
 _m_ake   _c_ompile  _r_un   _q_uit
"
  ("m" compile)
  ("c" multi-compile-run)
  ("r" quickrun)
  ("q" nil))
(define-key evil-normal-state-map (kbd "SPC m") 'hydra-make/body)

;; Multi-term
(require-package 'multi-term)
;; Vertical split multi-term
(defun multi-term-here ()
  "opens up a new terminal in the directory associated with the current buffer's file."
  (interactive)
  (split-window-right)
  (other-window 1)
  (multi-term))

;; Interact with Tmux
(require-package 'emamux)
;; Hydra for eamux
(defhydra hydra-eamux (:color blue
                       :hint nil)
  "
 ^Eamux^
 ^^^^^^^^^---------------------
 _s_end   _r_un   _l_ast  _c_lose   _q_uit
"
  ("s" emamux:send-command)
  ("r" emamux:run-command)
  ("l" emamux:run-last-command)
  ("c" emamux:close-runner-pane)
  ("q" nil))

;; Make the compilation window automatically disapper from enberg on #emacs
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

;; Eyebrowse mode
(require-package 'eyebrowse)
(setq eyebrowse-wrap-around t
      eyebrowse-switch-back-and-forth t)
(defun diminish-eyebrowse ()
  (interactive)
  (diminish 'eyebrowse-mode ""))
(add-hook 'eyebrowse-mode-hook 'diminish-eyebrowse)
(eyebrowse-mode t)

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

;; Diminish some stuff
(defun diminish-abbrev ()
  (interactive)
  (diminish 'abbrev-mode ""))
(add-hook 'abbrev-mode-hook 'diminish-abbrev)
(defun diminish-undo-tree ()
  (interactive)
  (diminish 'undo-tree-mode ""))
(add-hook 'undo-tree-mode-hook 'diminish-undo-tree)

;; Highlight the cursor line
(global-hl-line-mode 1)

;; GDB
(setq gdb-many-windows t
      gdb-show-main t)

;; Fill column indicator
(require-package 'fill-column-indicator)
(setq fci-rule-width 5
      fci-rule-column 79)
(define-key evil-normal-state-map (kbd "SPC ax") 'fci-mode)

;; Column enforce column that highlights if I go over 100 characters
;; I try to stick within 80 characters but, frankly, I prefer 100.
;; Hence a compromise
(require-package 'column-enforce-mode)
(require 'column-enforce-mode)
(setq column-enforce-column 99)
(defun diminish-column-enforce ()
  (interactive)
  (diminish 'column-enforce-mode ""))
(add-hook 'column-enforce-mode-hook 'diminish-column-enforce)
(add-hook 'prog-mode-hook 'column-enforce-mode)

;; Which function mode
(which-function-mode 1)

;; Enable subword mode
(defun diminish-subword ()
  (interactive)
  (diminish 'subword-mode ""))
(add-hook 'subword-mode-hook 'diminish-subword)
(global-subword-mode)

;; Enable smartparens-mode
(require-package 'smartparens)
(defun diminish-smartparens ()
  (interactive)
  (diminish 'smartparens-mode ""))
(add-hook 'smartparens-mode-hook 'diminish-smartparens)
(smartparens-global-mode)

;; Hide/Show mode
(defun diminish-hs-minor ()
  (interactive)
  (diminish 'hs-minor-mode ""))
(add-hook 'hs-minor-mode-hook 'diminish-hs-minor)
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; Column number mode
(column-number-mode)

;; Delete trailing whitespace on save
(require-package 'ws-butler)
(defun diminish-ws-butler ()
  (interactive)
  (diminish 'ws-butler-mode ""))
(add-hook 'ws-butler-mode-hook 'diminish-ws-butler)
(ws-butler-global-mode)

;; Region information
(require-package 'region-state)
(region-state-mode)

;; Restart Emacs from Emacs
(require-package 'restart-emacs)

;; Ledger mode for accounting
(require-package 'ledger-mode)

;; Profiler
(require-package 'esup)

;;; Construct mode hydras

;; Tags
(defhydra hydra-tags (:color red
                      :hint nil)
  "
 ^Tags^      ^Jump^         ^Python^
 ^^^^^^^^^---------------------------
 _c_reate    _r_eference    _d_efinition
 _u_pdate    _t_ag          _l_ocation
 _f_ind                   _q_uit
"
  ("c" ggtags-create-tags)
  ("u" ggtags-update-tags)
  ("f" ggtags-find-tag-regexp)
  ("r" ggtags-find-reference)
  ("t" ggtags-find-tag-dwim)
  ("d" elpy-goto-definition)
  ("l" elpy-goto-location)
  ("q" nil :color blue))
(define-key evil-normal-state-map (kbd "SPC j") 'hydra-tags/body)

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
  ("F" set-frame-font)
  ("n" linum-mode :color blue)
  ("w" toggle-truncate-lines :color blue)
  ("s" flyspell-mode :color blue)
  ("r" tool-bar-mode :color blue)
  ("a" scroll-bar-mode :color blue)
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
  ("o" org-custom-load :color blue)
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
