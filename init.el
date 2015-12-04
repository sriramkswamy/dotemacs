;;; Packages
(require 'package)

;; Garbage collector - increase threshold
(setq gc-cons-threshold 100000000)

;; packages based on versions
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t))
  (package-initialize)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(setq package-enable-at-startup nil)

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

;;; Evil - Just can't live without this
(require-package 'evil)
(require 'evil)
(setq evil-default-cursor t)
(evil-mode 1)
;; Specify evil initial states
(evil-set-initial-state 'dired-mode 'emacs)
(evil-set-initial-state 'paradox-menu-mode 'emacs)
(evil-set-initial-state 'calendar-mode 'emacs)

;;; Paradox for package
(require-package 'paradox)
(define-key evil-normal-state-map (kbd "SPC al") 'paradox-list-packages)
(setq paradox-github-token t)

;; Get the proper path
(require-package 'exec-path-from-shell)
(exec-path-from-shell-copy-env "PYTHONPATH")
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;;; GUI
;; No welcome screen - opens directly in scratch buffer
(setq inhibit-startup-message t
      initial-scratch-message ""
      visible-bell nil
      inhibit-splash-screen t)

;; No toolbar and scrollbar. Menubar only in GUI.
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(unless (display-graphic-p) (menu-bar-mode -1))

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

;; Enable winner-mode
(add-hook 'after-init-hook #'winner-mode)

;; Don't blink the cursor
(blink-cursor-mode -1)

;; Improve dired
(require-package 'dired+)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;; Powerline and spaceline
(require-package 'powerline)
(require-package 'spaceline)
(require 'powerline)
(setq powerline-default-separator 'nil)
(require 'spaceline-config)
(setq ns-use-srgb-colorspace nil)
(spaceline-toggle-anzu-on)
(spaceline-toggle-workspace-number-off)
(spaceline-spacemacs-theme)

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

;;; God-mode - Absolutely necessary
(require-package 'god-mode)
(require 'god-mode)
(setq god-exempt-major-modes nil
      god-exempt-predicates nil)
(add-to-list 'god-exempt-major-modes 'dired-mode)
(global-set-key (kbd "<escape>") 'god-local-mode)
(global-set-key (kbd "C-c C-m") 'god-local-mode)
(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-2") 'split-window-below)
(global-set-key (kbd "C-x C-3") 'split-window-right)
(global-set-key (kbd "C-x C-0") 'delete-window)
(global-set-key (kbd "C-x C-r") 'switch-to-buffer)
(global-set-key (kbd "C-x C-k") 'kill-buffer)
(global-set-key (kbd "C-x C-t") 'eshell-here)

;;; Which key
(require-package 'which-key)
(which-key-setup-side-window-bottom)
(which-key-mode)

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

;;; Evil - Vim emulation - Continued
;; Escape for everything
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
;; Maps
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "w") 'split-window-horizontally)
(define-key evil-normal-state-map (kbd "Z") 'delete-other-windows)
(define-key evil-normal-state-map (kbd "Q") 'winner-undo)
(define-key evil-normal-state-map (kbd "R") 'winner-redo)
(define-key evil-normal-state-map (kbd "w") 'split-window-horizontally)
(define-key evil-normal-state-map (kbd "W") 'split-window-vertically)
(define-key evil-normal-state-map (kbd "U") 'undo-tree-visualize)
(define-key evil-normal-state-map (kbd "K") 'man)
(define-key evil-normal-state-map (kbd "RET") 'evil-scroll-down)
(define-key evil-normal-state-map (kbd "DEL") 'evil-scroll-up)
(define-key evil-normal-state-map (kbd "+") 'eshell-vertical)
(define-key evil-normal-state-map (kbd "-") 'eshell-horizontal)
(define-key evil-normal-state-map (kbd "gs") 'electric-newline-and-maybe-indent)
(define-key evil-normal-state-map (kbd "gl") 'browse-url-at-point)
(define-key evil-normal-state-map (kbd "gL") 'browse-url-at-mouse)
(define-key evil-normal-state-map (kbd "gF") 'set-frame-name)
(define-key evil-normal-state-map (kbd "SPC q") 'evil-quit)
(define-key evil-normal-state-map (kbd "SPC w") 'save-buffer)
(define-key evil-normal-state-map (kbd "SPC k") 'kill-buffer)
(define-key evil-normal-state-map (kbd "SPC z") 'toggle-frame-fullscreen-non-native)
(define-key evil-normal-state-map (kbd "SPC f") 'find-file)
(define-key evil-normal-state-map (kbd "SPC [") 'widen)
(define-key evil-normal-state-map (kbd "SPC ;") 'evil-ex)
(define-key evil-normal-state-map (kbd "SPC 3") 'select-frame-by-name)
(define-key evil-normal-state-map (kbd "SPC DEL") 'whitespace-cleanup)
(define-key evil-normal-state-map (kbd "SPC ,") 'describe-bindings)
(define-key evil-visual-state-map (kbd "SPC ]") 'narrow-to-region)
(define-key evil-normal-state-map (kbd "SPC 6") 'quick-calc)
(define-key evil-normal-state-map (kbd "SPC se") 'eval-buffer)
(define-key evil-normal-state-map (kbd "SPC as") 'flyspell-mode)
(define-key evil-normal-state-map (kbd "SPC an") 'linum-mode)
(define-key evil-normal-state-map (kbd "SPC aw") 'toggle-truncate-lines)
(define-key evil-normal-state-map (kbd "SPC ab") 'display-battery-mode)
(define-key evil-normal-state-map (kbd "SPC at") 'display-time-mode)
(define-key evil-normal-state-map (kbd "SPC ap") 'package-install)
(define-key evil-normal-state-map (kbd "SPC af") 'set-frame-font)
(define-key evil-visual-state-map (kbd "SPC se") 'eval-region)
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

;; Evil visual-star
(require-package 'evil-visualstar)
(global-evil-visualstar-mode)

;; Evil args
(require-package 'evil-args)
(define-key evil-inner-text-objects-map "," #'evil-inner-arg)
(define-key evil-outer-text-objects-map "," #'evil-outer-arg)
(define-key evil-normal-state-map "\C-j" #'evil-jump-out-args)

;; Play nice with god mode
(require-package 'evil-god-state)
(define-key evil-normal-state-map (kbd "\\") 'evil-execute-in-god-state)

;; Search count
(require-package 'evil-anzu)
(with-eval-after-load 'evil
  (require 'evil-anzu))

;; Jump lists like vim
(require-package 'evil-jumper)
(global-evil-jumper-mode 1)

;; Like vim-sneak
(require-package 'evil-snipe)
(require 'evil-snipe)
(evil-snipe-mode 1)
(setq evil-snipe-repeat-keys t
      evil-snipe-scope 'visible
      evil-snipe-repeat-scope 'whole-visible
      evil-snipe-enable-highlight t
      evil-snipe-enable-incremental-highlight t)

;; Evil commentary
(require-package 'evil-commentary)
(evil-commentary-mode)

;; Evil exchange
(require-package 'evil-exchange)
(evil-exchange-install)

;; Increment and decrement numbers like vim
(require-package 'evil-numbers)
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-d") 'evil-numbers/dec-at-pt)

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

;; Enable recentf mode
(recentf-mode)

;; No backups
(setq make-backup-files nil
      auto-save-default nil)

;; Ediff plain window and vertical
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;; Remote file navigation
(require-package 'tramp)
(setq tramp-ssh-controlmaster-options "ssh")

;; Very large file viewing
(require-package 'vlf)

;;; Avy
(require-package 'avy)
(define-key evil-normal-state-map (kbd "SPC h") 'avy-goto-line)
(define-key evil-visual-state-map (kbd "SPC h") 'avy-goto-line)

;; ag
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
(ivy-mode 1)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-r") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-m") 'counsel-M-x)
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
(define-key evil-normal-state-map (kbd "SPC xf") 'counsel-describe-function)
(define-key evil-normal-state-map (kbd "SPC xv") 'counsel-describe-variable)
(define-key evil-normal-state-map (kbd "SPC xl") 'counsel-load-library)
(define-key evil-normal-state-map (kbd "SPC xi") 'counsel-info-lookup-symbol)
(define-key evil-normal-state-map (kbd "SPC e") 'counsel-ag)
(define-key evil-visual-state-map (kbd "SPC e") 'counsel-ag)
(define-key evil-visual-state-map (kbd "SPC d") 'counsel-M-x)
(define-key evil-insert-state-map (kbd "C-k") 'counsel-unicode-char)
(define-key evil-insert-state-map (kbd "C-d") 'ispell-word)
(define-key evil-insert-state-map (kbd "C-l") 'counsel-M-x)

;;; Hydra
(require-package 'hydra)

;; Bookmarks
(defhydra hydra-marks (:color red
                       :hint nil)
  "All marks"
  ("s" bookmark-set "set bookmark")
  ("S" bookmark-save "save bookmark")
  ("j" bookmark-jump "jump to bookmark")
  ("d" bookmark-delete "delete bookmark")
  ("m" pop-global-mark "mark ring")
  ("q" nil "quit hydra" :color blue))
(define-key evil-normal-state-map (kbd "SPC `") 'hydra-marks/body)

;; Org navigation and manipulation
(defhydra hydra-org-navigation (:color red
                                :hint nil)
  "Org manipulate "
  ("c" org-cycle "cycle")
  ("u" org-up-element "up")
  ("d" org-down-element "down")
  ("j" org-next-visible-heading "next")
  ("k" org-previous-visible-heading "previous")
  ("f" org-forward-heading-same-level "forward")
  ("b" org-backward-heading-same-level "backward")
  ("n" org-next-item "next item")
  ("p" org-previous-item "previous item")
  ("]" org-next-block "next block")
  ("[" org-previous-block "previous block")
  (">" org-next-link "next link")
  ("<" org-previous-link "previous link")
  ("H" org-metaleft "meta left")
  ("L" org-metaright "meta right")
  ("J" org-metadown "meta down")
  ("K" org-metaup "meta up")
  ("U" org-promote "promote")
  ("D" org-demote "demote")
  ("N" org-move-item-down "item down")
  ("P" org-move-item-up "item up")
  ("g" org-set-tags-command "set tags")
  ("t" org-todo "todo")
  ("d" org-deadline "deadline")
  ("D" org-deadline-close "deadline close")
  ("s" org-schedule "schedule item")
  ("i" org-clock-in "clock in")
  ("o" org-clock-out "clock out")
  ("r" org-clock-report "clock report")
  ("z" org-resolve-clocks "clock resolve")
  ("C" org-clock-cancel "clock cancel")
  ("S" org-clock-display "clock show")
  ("X" org-clock-in-last "clock last")
  ("G" org-clock-goto "clock goto")
  ("R" org-timer "timer")
  ("T" org-timer-set-timer "set timer")
  ("I" org-timer-start "start timer")
  ("O" org-timer-stop "stop timer")
  ("q" nil "quit" :color blue))
(define-key evil-normal-state-map (kbd "goo") 'hydra-org-navigation/body)

;; Org table manipulation
(defhydra hydra-org-tables (:color red
                            :hint nil)
  "Org tables "
  ("l" org-table-next-field "next field")
  ("h" org-table-previous-field "previous field")
  ("j" org-table-end-of-field "end of field")
  ("k" org-table-beginning-of-field "beginning of field")
  ("r" org-table-insert-row "insert row")
  ("c" org-table-insert-column "insert column")
  ("-" org-table-insert-hline "insert hline")
  ("J" org-table-move-row-down "move down")
  ("K" org-table-move-row-up "move up")
  ("H" org-table-move-column-left "move left")
  ("L" org-table-move-column-right "move right")
  ("R" org-table-kill-row "delete row")
  ("C" org-table-delete-column "delete column")
  ("b" org-table-blank-field "blank field")
  ("e" org-table-edit-field "edit field")
  ("i" org-table-field-info "field info")
  ("s" org-table-sum "table sum")
  ("f" org-table-eval-formula "table eval")
  ("F" org-table-edit-formulas "edit formulas")
  ("I" org-table-import "import")
  ("E" org-table-export "export")
  ("q" nil "quit" :color blue))
(define-key evil-normal-state-map (kbd "got") 'hydra-org-tables/body)

;; Window manipulation
(defhydra hydra-window (:color red
                        :hint nil)
  "Window nav "
  ("h" windmove-left "left")
  ("j" windmove-down "down")
  ("k" windmove-up "up")
  ("l" windmove-right "right")
  ("H" shrink-window-horizontally "shrink vertical")
  ("J" enlarge-window "enlarge horizontally")
  ("K" shrink-window "shrink horizontally")
  ("L" enlarge-window-horizontally "enlarge vertically")
  ("w" split-window-right "split right")
  ("W" split-window-below "split below")
  ("s" save-buffer "save")
  ("d" delete-window "delete")
  ("Z" delete-other-windows "only")
  ("Q" (progn
         (winner-undo)
         (setq this-command 'winner-undo)) "winner undo")
  ("R" winner-redo "winner redo")
  ("M" toggle-frame-maximized "maximize")
  ("n" make-frame "new frame")
  ("D" delete-frame "delete frame")
  ("u" ivy-switch-buffer "buffers")
  ("r" ivy-recentf "recertf")
  ("f" counsel-find-file "files")
  ("F" follow-mode "follow")
  ("+" text-scale-increase "zoom in")
  ("-" text-scale-decrease "zoom out")
  ("q" nil "cancel"))
(define-key evil-normal-state-map (kbd "gw") 'hydra-window/body)

;; Apropos
(defhydra hydra-apropos (:color blue
                         :hint nil)
  "Apropos"
  ("a" apropos "apropos")
  ("d" apropos-documentation "doc")
  ("v" apropos-variable "var")
  ("c" apropos-command "cmd")
  ("l" apropos-library "lib")
  ("u" apropos-user-option "option")
  ("e" apropos-value "value"))
(define-key evil-normal-state-map (kbd "SPC xa") 'hydra-apropos/body)

;; Find file in project
(require-package 'find-file-in-project)
(define-key evil-normal-state-map (kbd "SPC p") 'find-file-in-project)
(define-key evil-normal-state-map (kbd "SPC TAB") 'ff-find-other-file)

;;; Swoop
(require-package 'swoop)
(require 'swoop)
(define-key evil-normal-state-map (kbd "*") 'swoop-pcre-regexp)
(define-key evil-normal-state-map (kbd "*") 'swoop-pcre-regexp)
(define-key evil-visual-state-map (kbd "#") 'swoop-pcre-regexp)
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

;;; Manage external services
(require-package 'prodigy)

;; NeoTree - like NERDTree
(require-package 'neotree)
(add-hook 'neotree-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "o") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "i") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "~") 'neotree-change-root)
            (define-key evil-normal-state-local-map (kbd "r") 'neotree-refresh)
            (define-key evil-normal-state-local-map (kbd "C") 'neotree-create-node)
            (define-key evil-normal-state-local-map (kbd "D") 'neotree-delete-node)
            (define-key evil-normal-state-local-map (kbd "R") 'neotree-rename-node)
            (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)))
(define-key evil-normal-state-map (kbd "SPC n") 'neotree-toggle)

;;; Cmake ide
(require-package 'cmake-ide)

;;; GTags
(require-package 'ggtags)
(ggtags-mode)
;; Add exec-path for Gtags
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/Cellar/global/6.5/bin"))
(setq exec-path (append exec-path '("/usr/local/Cellar/global/6.5/bin")))
(setq-local imenu-create-index-function #'ggtags-build-imenu-index)
(define-key evil-normal-state-map (kbd "SPC ag") 'ggtags-create-tags)
(define-key evil-normal-state-map (kbd "SPC au") 'ggtags-update-tags)
(define-key evil-normal-state-map (kbd "T") 'ggtags-find-tag-regexp)
(define-key evil-normal-state-map (kbd "SPC jr") 'ggtags-find-reference)
(define-key evil-normal-state-map (kbd "SPC jt") 'ggtags-find-tag-dwim)

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
(define-key evil-normal-state-map (kbd "SPC aj") 'jabber-connect)
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
(require 'flyspell)
(add-hook 'text-mode-hook 'flyspell-mode)
;; move point to previous error
;; based on code by hatschipuh at
;; http://emacs.stackexchange.com/a/14912/2017
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
(define-key evil-normal-state-map (kbd "]s") 'flyspell-goto-next-error)
(define-key evil-normal-state-map (kbd "[s") 'flyspell-goto-previous-error)

;; Better folding
(require-package 'origami)
(global-origami-mode)
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
(volatile-highlights-mode t)

;; Smart tabs
(require-package 'smart-tab)
(global-smart-tab-mode)

;;; Multiple cursors
(require-package 'evil-mc)
(global-evil-mc-mode 1)

;; Flx with company
(require-package 'flx)
(require-package 'company-flx)

;;; Company
(require-package 'company)
(global-company-mode)
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
(define-key evil-normal-state-map (kbd "SPC ac") 'global-company-mode)
(defun my-company-hook ()
  (interactive)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map [tab] 'company-complete-common-or-cycle))
(add-hook 'company-mode-hook 'my-company-hook)

;; Company C headers
(require-package 'company-c-headers)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-c-headers))

;; Irony mode for C++
(require-package 'irony)
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
(yas-global-mode)
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
  (split-window-right)
  (other-window 1)
  (R)
  (other-window 1))
;; Vertical split julia REPL
(defun julia-shell-here ()
  "opens up a new julia REPL in the directory associated with the current buffer's file."
  (interactive)
  (split-window-right)
  (other-window 1)
  (julia)
  (other-window 1))
(define-key evil-normal-state-map (kbd "SPC tr") 'r-shell-here)
(define-key evil-normal-state-map (kbd "SPC tj") 'julia-shell-here)
(define-key evil-normal-state-map (kbd "SPC sf") 'ess-eval-function)
(define-key evil-normal-state-map (kbd "SPC sl") 'ess-eval-line)
;; For R - Intuitive
(define-key evil-normal-state-map (kbd "SPC sr") 'ess-eval-buffer)
(define-key evil-visual-state-map (kbd "SPC sr") 'ess-eval-region)
;; For Julia - intuitive
(define-key evil-normal-state-map (kbd "SPC sj") 'ess-eval-buffer)
(define-key evil-visual-state-map (kbd "SPC sj") 'ess-eval-region)

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
(define-key evil-normal-state-map (kbd "SPC jd") 'elpy-goto-definition)
(define-key evil-normal-state-map (kbd "SPC jl") 'elpy-goto-location)
(define-key evil-normal-state-map (kbd "SPC tp") 'elpy-shell-switch-to-shell)
(define-key evil-normal-state-map (kbd "SPC sd") 'python-shell-send-defun)
(define-key evil-normal-state-map (kbd "SPC ss") 'elpy-shell-send-current-statement)
(define-key evil-normal-state-map (kbd "SPC sp") 'elpy-shell-send-region-or-buffer)
(define-key evil-visual-state-map (kbd "SPC sp") 'elpy-shell-send-region-or-buffer)

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

;; MATLAB mode
(require-package 'matlab-mode)
(eval-after-load 'matlab
  '(add-to-list 'matlab-shell-command-switches "-nosplash"))
(setq matlab-shell-command "/Applications/MATLAB_R2014a.app/bin/matlab"
      matlab-indent-function t)
;; Vertical split matlab shell
(defun matlab-shell-here ()
  "opens up a new matlab shell in the directory associated with the current buffer's file."
  (interactive)
  (split-window-right)
  (other-window 1)
  (matlab-shell)
  (other-window 1))
(define-key evil-normal-state-map (kbd "SPC tm") 'matlab-shell-here)
(define-key evil-normal-state-map (kbd "SPC sm") 'matlab-shell-run-cell)
(define-key evil-visual-state-map (kbd "SPC sm") 'matlab-shell-run-region)

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
(define-key evil-normal-state-map (kbd "]l") 'flycheck-next-error)
(define-key evil-normal-state-map (kbd "[l") 'flycheck-previous-error)
(define-key evil-normal-state-map (kbd "]L") 'flycheck-last-checker)
(define-key evil-normal-state-map (kbd "[L") 'flycheck-first-error)

;; Irony for flycheck
(require-package 'flycheck-irony)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
(define-key evil-normal-state-map (kbd "SPC l") 'flycheck-list-errors)

;;; Org mode
(define-key evil-normal-state-map (kbd "SPC c") 'org-capture)
(define-key evil-normal-state-map (kbd "SPC o") 'org-agenda)
(define-key evil-normal-state-map (kbd "SPC -") 'org-edit-src-code)
(define-key evil-normal-state-map (kbd "SPC =") 'org-edit-src-exit)
(define-key evil-normal-state-map (kbd "SPC ]") 'org-narrow-to-subtree)
(define-key evil-normal-state-map (kbd "gox") 'org-preview-latex-fragment)
(define-key evil-normal-state-map (kbd "goi") 'org-toggle-inline-images)
(define-key evil-normal-state-map (kbd "goG") 'org-tags-view)
(define-key evil-normal-state-map (kbd "goj") 'org-goto)
(define-key evil-normal-state-map (kbd "goS") 'org-check-deadlines)
(define-key evil-normal-state-map (kbd "goU") 'org-update-all-dblocks)
(define-key evil-normal-state-map (kbd "goL") 'org-toggle-link-display)
(define-key evil-normal-state-map (kbd "gov") 'org-reveal)
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
(define-key evil-normal-state-map (kbd "gom") 'org-match-sparse-tree)
(define-key evil-normal-state-map (kbd "goy") 'org-copy-subtree)
(define-key evil-normal-state-map (kbd "gok") 'org-cut-subtree)
(define-key evil-normal-state-map (kbd "goR") 'org-toggle-ordered-property)
(define-key evil-normal-state-map (kbd "goE") 'org-set-effort)
(define-key evil-normal-state-map (kbd "gop") 'org-set-property)
(define-key evil-normal-state-map (kbd "goP") 'org-delete-property)
(define-key evil-normal-state-map (kbd "god") 'org-insert-drawer)
(define-key evil-normal-state-map (kbd "goD") 'org-insert-property-drawer)
(define-key evil-normal-state-map (kbd "goM") 'orgstruct-mode)
(define-key evil-normal-state-map (kbd "goB") 'orgtbl-mode)
(define-key evil-normal-state-map (kbd "goh") 'org-toggle-heading)
(define-key evil-normal-state-map (kbd "gos") 'org-update-statistics-cookies)
(define-key evil-normal-state-map (kbd "go>") 'org-goto-calendar)
(define-key evil-normal-state-map (kbd "go<") 'org-date-from-calendar)
(define-key evil-normal-state-map (kbd "go.") 'org-time-stamp)
(define-key evil-normal-state-map (kbd "go,") 'org-time-stamp-inactive)
(define-key evil-normal-state-map (kbd "go#") 'org-priority)
(define-key evil-normal-state-map (kbd "go%") 'org-timer-set-timer)
(define-key evil-normal-state-map (kbd "go^") 'org-sort)
(define-key evil-normal-state-map (kbd "go/") 'org-sparse-tree)
(define-key evil-normal-state-map (kbd "go|") 'org-table-create-or-convert-from-region)
(define-key evil-normal-state-map (kbd "go'") 'org-columns)
(define-key evil-normal-state-map (kbd "go]") 'org-remove-file)
(define-key evil-normal-state-map (kbd "go[") 'org-agenda-file-to-front)
(define-key evil-visual-state-map (kbd "SPC oc") 'org-capture)
(define-key evil-visual-state-map (kbd "SPC oa") 'org-agenda)
(define-key evil-visual-state-map (kbd "go|") 'org-table-create-or-convert-from-region)
(define-key evil-visual-state-map (kbd "goe") 'org-export-dispatch)
(define-key evil-visual-state-map (kbd "go'") 'org-columns)

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
      org-completion-use-ido t
      org-default-notes-file "~/Dropbox/notes/inbox.org"
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
(setq org-tag-alist (quote ((:startgroup)
                            ("@office" . ?o)
                            ("@home" . ?h)
                            (:endgroup)
                            ("errand" . ?t)
                            ("personal" . ?p)
                            ("work" . ?w)
                            ("noexport" . ?e)
                            ("note" . ?n))))

;; TODO Keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "HOLD(h@/!)" "|" "DONE(d!)")
        (sequence "|" "CANCELLED(c@)")))

;; Links
(setq org-link-abbrev-alist
      '(("bugzilla"  . "http://10.1.2.9/bugzilla/show_bug.cgi?id=")
        ("url-to-ja" . "http://translate.google.fr/translate?sl=en&tl=ja&u=%h")
        ("google"    . "http://www.google.com/search?q=")
        ("gmaps"      . "http://maps.google.com/maps?q=%s")))

;; Capture templates
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Dropbox/notes/tasks.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/Dropbox/notes/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")))

;; Calendar
(require-package 'org-caldav)

;; LaTeX
(require-package 'cdlatex)
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

;;; Version control

;; Magit
(require-package 'magit)
(define-key evil-normal-state-map (kbd "SPC g") 'magit-status)
(define-key evil-normal-state-map (kbd "gb") 'magit-blame)
(define-key evil-normal-state-map (kbd "gz") 'magit-blame-quit)
(define-key evil-normal-state-map (kbd "gj") 'magit-blame-next-chunk)
(define-key evil-normal-state-map (kbd "gJ") 'magit-blame-next-chunk-same-commit)
(define-key evil-normal-state-map (kbd "gk") 'magit-blame-previous-chunk)
(define-key evil-normal-state-map (kbd "gK") 'magit-blame-previous-chunk-same-commit)
(define-key evil-normal-state-map (kbd "gy") 'magit-blame-copy-hash)
(define-key evil-normal-state-map (kbd "gt") 'magit-blame-toggle-headings)

;; Diff-hl
(require-package 'diff-hl)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
(add-hook 'prog-mode-hook 'diff-hl-mode)
(add-hook 'html-mode-hook 'diff-hl-mode)
(add-hook 'text-mode-hook 'diff-hl-mode)
(add-hook 'org-mode-hook 'diff-hl-mode)
(diff-hl-margin-mode)
(diff-hl-flydiff-mode)
(define-key evil-normal-state-map (kbd "]c") 'diff-hl-next-hunk)
(define-key evil-normal-state-map (kbd "[c") 'diff-hl-previous-hunk)
(define-key evil-normal-state-map (kbd "gh") 'diff-hl-revert-hunk)

;; Git time-machine
(require-package 'git-timemachine)
(define-key evil-normal-state-map (kbd "zl") 'git-timemachine)
(define-key evil-normal-state-map (kbd "zq") 'git-timemachine-quit)
(define-key evil-normal-state-map (kbd "zy") 'git-timemachine-kill-revision)
(define-key evil-normal-state-map (kbd "]q") 'git-timemachine-show-next-revision)
(define-key evil-normal-state-map (kbd "[q") 'git-timemachine-show-previous-revision)
(define-key evil-normal-state-map (kbd "]Q") 'git-timemachine-show-nth-revision)
(define-key evil-normal-state-map (kbd "[Q") 'git-timemachine-show-current-revision)

;; Gists
(require-package 'yagist)
(setq yagist-view-gist t)
(define-key evil-normal-state-map (kbd "SPC sg") 'yagist-buffer)
(define-key evil-visual-state-map (kbd "SPC sg") 'yagist-region)

;;; REPL

;; Eshell send command
(define-key evil-normal-state-map (kbd "SPC st") 'eshell-command)

;; Compile
(define-key evil-normal-state-map (kbd "SPC m") 'compile)

;; Multi-term
(require-package 'multi-term)
;; Vertical split multi-term
(defun multi-term-here ()
  "opens up a new terminal in the directory associated with the current buffer's file."
  (interactive)
  (split-window-right)
  (other-window 1)
  (multi-term))
(define-key evil-normal-state-map (kbd "SPC ts") 'multi-term-here)

;; Interact with Tmux
(require-package 'emamux)
(define-key evil-normal-state-map (kbd "SPC sx") 'emamux:send-command)

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

;; Quickrun
(require-package 'quickrun)
(define-key evil-normal-state-map (kbd "SPC 4") 'quickrun)

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

;; Plan9
(require 'eshell)
(require 'em-smart)
(setq eshell-where-to-jump 'begin
      eshell-review-quick-commands nil
      eshell-smart-space-goes-to-end t)

;; Eyebrowse mode
(require-package 'eyebrowse)
(setq eyebrowse-wrap-around t
      eyebrowse-switch-back-and-forth t)
(eyebrowse-mode t)
(define-key evil-normal-state-map (kbd "SPC ii") 'eyebrowse-switch-to-window-config)
(define-key evil-normal-state-map (kbd "SPC il") 'eyebrowse-last-window-config)
(define-key evil-normal-state-map (kbd "SPC in") 'eyebrowse-next-window-config)
(define-key evil-normal-state-map (kbd "SPC ip") 'eyebrowse-prev-window-config)
(define-key evil-normal-state-map (kbd "SPC ir") 'eyebrowse-rename-window-config)
(define-key evil-normal-state-map (kbd "SPC ic") 'eyebrowse-close-window-config)
(define-key evil-normal-state-map (kbd "SPC i0") 'eyebrowse-switch-to-window-config-0)
(define-key evil-normal-state-map (kbd "SPC i1") 'eyebrowse-switch-to-window-config-1)
(define-key evil-normal-state-map (kbd "SPC i2") 'eyebrowse-switch-to-window-config-2)
(define-key evil-normal-state-map (kbd "SPC i3") 'eyebrowse-switch-to-window-config-3)
(define-key evil-normal-state-map (kbd "SPC i4") 'eyebrowse-switch-to-window-config-4)
(define-key evil-normal-state-map (kbd "SPC i5") 'eyebrowse-switch-to-window-config-5)
(define-key evil-normal-state-map (kbd "SPC i6") 'eyebrowse-switch-to-window-config-6)
(define-key evil-normal-state-map (kbd "SPC i7") 'eyebrowse-switch-to-window-config-7)
(define-key evil-normal-state-map (kbd "SPC i8") 'eyebrowse-switch-to-window-config-8)
(define-key evil-normal-state-map (kbd "SPC i9") 'eyebrowse-switch-to-window-config-9)

;;; Wrap up

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
(global-column-enforce-mode)

;; Which function mode
(which-function-mode 1)

;; Enable subword mode
(global-subword-mode)

;; Enable smartparens-mode
(require-package 'smartparens)
(smartparens-global-mode)

;; Hide/Show mode
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; Column number mode
(column-number-mode)

;; Delete trailing whitespace on save
(require-package 'ws-butler)
(ws-butler-global-mode)

;; Region information
(require-package 'region-state)
(region-state-mode)

;; Restart Emacs from Emacs
(require-package 'restart-emacs)

;; Profiler
(require-package 'esup)

;;; Clean mode-line
(defvar mode-line-cleaner-alist
  `((smartparens-mode . "")
    (which-key-mode . "")
    (god-local-mode . " ψ")
    (evil-snipe-mode . "")
    (evil-snipe-local-mode . "")
    (evil-mc-mode . "")
    (evil-commentary-mode . "")
    (eyebrowse-mode . "")
    (ivy-mode . "")
    (elpy-mode . "")
    (ws-butler-mode . "")
    (org-cdlatex-mode . "")
    (subword-mode . "")
    (volatile-highlights-mode . "")
    (flycheck-mode . "")
    (flyspell-mode . " $")
    (aspell-mode . " $")
    (ispell-mode . " $")
    (ggtags-mode . "")
    (aggressive-indent-mode . "")
    (column-enforce-mode . "")
    (smart-tab-mode . "")
    (company-mode . " ς")
    (irony-mode . " Γ")
    (yas-minor-mode . " γ")
    (eldoc-mode . "")
    (org-indent-mode . "")
    (hs-minor-mode . "")
    (auto-complete-mode . "")
    (abbrev-mode . "")
    (undo-tree-mode . "")
    ;; Major modes
    (lisp-interaction-mode . ":λ:")
    (hi-lock-mode . "")
    (markdown-mode . ":MD:")
    (python-mode . ":PY:")
    (emacs-lisp-mode . ":EL:")
    (org-mode . ":ORG:")
    (nxhtml-mode . ":NX:"))
  "Alist for `clean-mode-line'.
When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")
(defun clean-mode-line ()
  (interactive)
  (loop for cleaner in mode-line-cleaner-alist
        do (let* ((mode (car cleaner))
                  (mode-str (cdr cleaner))
                  (old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
               (setcar old-mode-str mode-str))
             ;; major mode
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))
(add-hook 'after-change-major-mode-hook 'clean-mode-line)

;; Garbage collector - decrease threshold by an order
(setq gc-cons-threshold 10000000)

(server-start)
;;; .emacs ends here
