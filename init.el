;;; package --- Summary

;;; Commentary:
;; Here be pokemons.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Change some settings    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; General settings
(setq user-full-name "Sriram Krishnaswamy")                                    ; Hi Emacs, I'm Sriram
(setq gc-cons-threshold (* 100 1024 1024))                                     ; increase the threshold for garbage collection - 100 MB
(setq delete-old-versions -1)                                                  ; delete excess backup versions silently
(setq version-control t)                                                       ; use version control for backups
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))                  ; which directory to put backups file
(setq vc-make-backup-files t)                                                  ; make backups file even when in version controlled dir
(setq vc-follow-symlinks t)                                                    ; don't ask for confirmation when opening symlinked file
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t))) ; transform backups file name
(setq inhibit-startup-screen t)                                                ; inhibit useless and old-school startup screen
(setq visible-bell nil)                                                        ; no visible bell for errors
(setq ring-bell-function 'ignore)                                              ; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8)                                           ; use utf-8 by default for reading
(setq coding-system-for-write 'utf-8)                                          ; use utf-8 by default for writing
(setq sentence-end-double-space nil)                                           ; sentence SHOULD end with only a point.
(setq fill-column 80)                                                          ; toggle wrapping text at the 80th character
(setq initial-scratch-message "(hello-human)")                                 ; print a default message in the empty scratch buffer opened at startup
(menu-bar-mode -1)                                                             ; deactivate the menubar
(tool-bar-mode -1)                                                             ; deactivate the toolbar
(scroll-bar-mode -1)                                                           ; deactivate the scrollbar
(tooltip-mode -1)                                                              ; deactivate the tooltip
(setq initial-frame-alist                                                      ; initial frame size
	  '((width . 102)                                                          ; characters in a line
		(height . 54)))                                                        ; number of lines
(setq default-frame-alist                                                      ; subsequent frame size
	  '((width . 100)                                                          ; characters in a line
		(height . 52)))                                                        ; number of lines
(blink-cursor-mode -1)                                                         ; don't blink the cursor
(defun display-startup-echo-area-message () (message "Let the games begin!"))  ; change the default startup echo message
(setq-default truncate-lines t)                                                ; if line exceeds screen, let it
(setq large-file-warning-threshold (* 15 1024 1024))                           ; increase threshold for large files
(fset 'yes-or-no-p 'y-or-n-p)                                                  ; prompt for 'y' or 'n' instead of 'yes' or 'no'
(setq-default abbrev-mode t)                                                   ; turn on abbreviations by default
(setq save-abbrevs 'silently)                                                  ; don't inform when saving new abbreviations
(setq ediff-window-setup-function 'ediff-setup-windows-plain                   ; have a plain setup for ediff
	  ediff-split-window-function 'split-window-horizontally)                  ; show two vertical windows instead of horizontal ones
(setq recenter-positions '(top middle bottom))                                 ; recenter from the top instead of the middle
(put 'narrow-to-region 'disabled nil)                                          ; enable narrowing to region
(put 'narrow-to-defun 'disabled nil)                                           ; enable narrowing to function
(when (fboundp 'winner-mode)                                                   ; when you can find 'winner-mode'
  (winner-mode 1))                                                             ; activate winner mode
(setq recentf-max-saved-items 1000                                             ; set the number of recent items to be saved
	  recentf-exclude '("/tmp/" "/ssh:"))                                      ; exclude the temporary and remote files accessed recently
(setq ns-use-native-fullscreen nil)                                            ; don't use the native fullscreen - more useful in a Mac
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))         ; setup a new custom file
(when (file-exists-p custom-file)                                              ; if the custom file exists
  (load custom-file))                                                          ; load the custom file
(savehist-mode)                                                                ; keep persistent history
(subword-mode 1)                                                               ; move correctly over camelCase words
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))      ; load more configuration from the 'config' folder
(put 'scroll-left 'disabled nil)                                               ; enable sideward scrolling
(define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word)              ; backward kill word in minibuffer
(setq enable-recursive-minibuffers t)                                          ; use the minibuffer while using the minibuffer
;; how tabs are seen and added
(setq-default tab-width 4)
(setq-default tab-stop-list
			  '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80))

;; I prefer this set of keys to the defaults
(when (eq system-type 'darwin)
  (setq mac-command-key-is-meta t)
  (setq mac-command-modifier 'meta)
  (setq mac-option-key-is-meta nil)
  (setq mac-option-modifier nil))

;; dired settings
(setq dired-dwim-target t                                                    ; do what i mean
	  dired-recursive-copies 'top                                            ; copy recursively
	  dired-recursive-deletes 'top                                           ; delete recursively
	  dired-listing-switches "-alh")
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;; doc-view settings
(setq doc-view-continuous t)

;; Interpret ESC as <escape> in terminal unless pressed very fast
(defvar sk/fast-keyseq-timeout 100)

(defun sk/tty-ESC-filter (map)
  (if (and (equal (this-single-command-keys) [?\e])
		   (sit-for (/ sk/fast-keyseq-timeout 1000.0)))
	  [escape] map))

(defun sk/lookup-key (map key)
  (catch 'found
	(map-keymap (lambda (k b) (if (equal key k) (throw 'found b))) map)))

(defun sk/catch-tty-ESC ()
  "Setup key mappings of current terminal to turn a tty's ESC into `escape'."
  (when (memq (terminal-live-p (frame-terminal)) '(t pc))
	(let ((esc-binding (sk/lookup-key input-decode-map ?\e)))
	  (define-key input-decode-map
		[?\e] `(menu-item "" ,esc-binding :filter sk/tty-ESC-filter)))))

(sk/catch-tty-ESC)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Package management    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Include the module to install packages
(require 'package)
(setq package-enable-at-startup nil)      ; tells emacs not to load any packages before starting up
;; the following lines tell emacs where on the internet to look up
;; for new packages.
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
						 ("gnu"       . "http://elpa.gnu.org/packages/")
						 ("melpa"     . "https://melpa.org/packages/")))
(package-initialize)                      ; initialize the packages

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)  ; unless it is already installed
  (package-refresh-contents)                ; updage packages archive
  (package-install 'use-package))           ; and install the most recent version of use-package
(require 'use-package)                      ; Source use-package

;; Keep the mode-line clean
(use-package diminish                       ; diminish stuff from the mode-line
  :ensure t                                 ; ensure the package is present
  :demand t                                 ; load the package immediately
  :diminish (visual-line-mode . " ω")        ; diminish the `visual-line-mode'
  :diminish hs-minor-mode                   ; diminish the `hs-minor-mode'
  :diminish abbrev-mode                     ; diminish the `abbrev-mode'
  :diminish auto-fill-function              ; diminish the `auto-fill-function'
  :diminish subword-mode)                   ; diminish the `subword-mode'

;; ;; Make sure the path is set right
;; (use-package exec-path-from-shell
;;   :ensure t
;;   :init
;;   (setq exec-path-from-shell-check-startup-files nil)
;;   :config
;;   ;; (exec-path-from-shell-copy-env "PYTHONPATH")
;;   (when (memq window-system '(mac ns x))
;;     (exec-path-from-shell-initialize)))

;; better package menu
(use-package paradox
  :ensure t
  :commands (paradox-list-packages))

;;;;;;;;;;;;;;;;;;;;;;;;
;;    Key bindings    ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;;   (bind-key "C-c x" 'my-ctrl-c-x-command)
;;   (bind-key* "<C-return>" 'other-window)
;;   (bind-key "C-c x" 'my-ctrl-c-x-command some-other-mode-map)
;;   (unbind-key "C-c x" some-other-mode-map)
;;    (bind-keys :map dired-mode-map
;;               ("o" . dired-omit-mode)
;;               ("a" . some-custom-dired-function))
;;    (bind-keys :prefix-map my-customize-prefix-map
;;               :prefix "C-c c"
;;               ("f" . customize-face)
;;               ("v" . customize-variable))
;;    (bind-keys*
;;     ("C-o" . other-window)
;;     ("C-M-n" . forward-page)
;;     ("C-M-p" . backward-page))
;; Change some default emacs bindings
(bind-keys*
 ("C-j" . electric-newline-and-maybe-indent)
 ("M-k" . kill-whole-line)
 ("M-m" . toggle-input-method)
 ("C-x w" . delete-frame)
 ("C-x f" . make-frame-command)
 ("C-c m" . rectangle-mark-mode)
 ("C-x k" . kill-this-buffer)
 ("M-:" . goto-line)
 ("M-\"" . move-to-column)
 ("C-c g f" . find-file-at-point)
 ("C-c '" . woman)
 ("C-(" . kmacro-start-macro)
 ("C-)" . kmacro-end-macro)
 ("C-`" . kmacro-end-or-call-macro-repeat)
 ("C-~" . kmacro-name-last-macro)
 ("C-^" . mode-line-other-buffer)
 ("C-c j" . join-line)
 ("C-x C-y" . delete-blank-lines)
 ("C-x C-o" . other-window)
 ("C-x C-j" . winner-undo)
 ("C-x C-a" . winner-redo)
 ("C-c o f" . flyspell-mode)
 ("C-c o p" . flyspell-prog-mode)
 ("C-c o v" . visual-line-mode)
 ("C-c o b" . display-battery-mode)
 ("C-c o t" . display-time-mode))
(bind-keys
 ("C-x b" . ibuffer)
 ("M-n" . next-error)
 ("M-p" . previous-error)
 ("C-x C-0" . delete-window)
 ("C-x C-1" . delete-other-windows)
 ("C-x C-2" . split-window-below)
 ("C-x C-3" . split-window-right)
 ("C-x C-b" . switch-to-buffer))

;; Create consistent keybindings
(use-package general
  :ensure t
  :config
  (general-evil-setup))
(setq sk--evil-local-leader "m")
(general-nvmap :prefix sk--evil-local-leader
			   "" '(nil :which-key "major mode map"))
(setq sk--evil-global-leader "SPC")
(general-nvmap :prefix sk--evil-global-leader
			   "" '(nil :which-key "global map"))

;; hint for bindings
(use-package which-key
  :ensure t
  :demand t
  :diminish which-key-mode
  :bind* (("C-c ?" . which-key-show-top-level))
  :config
  ;; Emacs 26 breaks which-key; see https://github.com/justbur/emacs-which-key/issues/146
  (defalias 'display-buffer-in-major-side-window 'window--make-major-side-window)
  ;; turn on which key and add some names for default/common prefixes
  (which-key-enable-god-mode-support)
  (which-key-mode)
  (which-key-add-key-based-replacements
	"C-x ESC" "complex command"
	"C-x RET" "file encoding"
	"C-x 4" "window"
	"C-x 5" "frame"
	"C-x 6" "2C prefix"
	"C-x 8" "unicode"
	"C-x @" "event"
	"C-x X" "edebug"
	"C-x C-a" "edebug set"
	"C-x n" "narrow"
	"C-x r" "rect/reg/bookmarks"
	"C-x a" "abbrev"
	"C-x a i" "inverse"
	"M-s h" "highlight"
	"C-c g" "git/goto"
	"C-c !" "flycheck"
	"C-c &" "yasnippets"
	"C-c o" "options"
	"C-c y" "company"
	"C-c w" "align"
	"C-c ," "bibliography"
	"M-\\" "language hydras"
	"C-\\" "major mode"
	"C-c k" "convenience defuns"))

;;;;;;;;;;;;;;;;;;;;;;;;
;;    Modal states    ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; hydra
(use-package hydra
  :ensure t
  :demand t)

;; bookmark hydra
(defhydra hydra-bookmarks (:color blue :hint nil)
  "
 ^Bookmark^               ^Registers^
^^^^^^--------------------------------------------------------------------
 _s_: set   _b_: bookmark   _r_: jump to    _t_: point to    _f_: frameset to  _q_: quit
 _j_: jump  _d_: delete     _i_: insert to  _a_: increment   _w_: copy to
  "
  ("s" bookmark-set)
  ("b" bookmark-save)
  ("j" bookmark-jump)
  ("d" bookmark-delete)
  ("r" jump-to-register)
  ("i" insert-register)
  ("t" point-to-register)
  ("a" increment-register)
  ("f" frameset-to-register)
  ("w" copy-to-register)
  ("q" nil :color blue))
(bind-key* "C-x r" 'hydra-bookmarks/body)
(general-nvmap "+" '(hydra-bookmarks/body :which-key "bookmarks"))

;; rectangle mark mode
(defhydra hydra-rectangle (:color pink :hint nil)
  "
 _c_: clear    _o_: open    _k_: kill    _y_: yank    _r_: reset    _q_: quit
 _d_: delete   _s_: string  _w_: copy    _n_: number  _x_: deactivate
  "
  ("c" clear-rectangle)
  ("d" delete-rectangle)
  ("o" open-rectangle)
  ("s" string-rectangle)
  ("k" kill-rectangle)
  ("w" copy-rectangle-as-kill)
  ("y" yank-rectangle)
  ("n" rectangle-number-lines)
  ("x" sk/remove-mark)
  ("r" (if (region-active-p)
		   (deactivate-mark)
		 (rectangle-mark-mode 1)) nil)
  ("q" nil :color blue))
(bind-key* "C-x m" 'hydra-rectangle/body)

;; Evil - Vim emulation
;; (require 'sk-evil)
;; God mode - Modal Emacs
(require 'sk-god)

;;;;;;;;;;;;;;;;;;;
;;    Editing    ;;
;;;;;;;;;;;;;;;;;;;

;; snippets
(use-package yasnippet
  :ensure t
  :commands (yas-insert-snippet yas-new-snippet)
  :bind* (("C-]" . yas-insert-snippet)
		  ("C-c o y" . yas-minor-mode))
  :diminish yas-minor-mode
  :config
  (setq yas/triggers-in-field t); Enable nested triggering of snippets
  (setq yas-prompt-functions '(yas-completing-prompt))
  (add-hook 'snippet-mode-hook '(lambda () (setq-local require-final-newline nil)))
  (yas-global-mode))

;;;;;;;;;;;;;;;;;;;;;;
;;    Navigation    ;;
;;;;;;;;;;;;;;;;;;;;;;

;; neotree for folder tree
(use-package neotree
  :ensure t
  :init
  (setq neo-theme 'arrow)
  :bind* (("C-c v" . neotree-toggle)))

;; f and t like vim
(use-package iy-go-to-char
  :ensure t
  :bind* (("C-r f" . iy-go-to-char)
		  ("C-r C-f" . iy-go-to-char)
		  ("C-r d" . iy-go-to-char-backward)
		  ("C-r C-d" . iy-go-to-char-backward)
		  ("C-r t" . iy-go-up-to-char)
		  ("C-r C-t" . iy-go-up-to-char)
		  ("C-r r" . iy-go-up-to-char-backward)
		  ("C-r C-r" . iy-go-up-to-char-backward)
		  ("C-'" . iy-go-to-char-done)
		  ("C-;" . iy-go-to-or-up-to-continue-backward)))

;; Avy - simulating a mouse click
(use-package avy
  :ensure t
  :demand t
  :bind* (("C-t" . avy-goto-char-2)
		  ("M-t" . avy-goto-char-timer)
		  ("M-l" . avy-goto-line))
  :init
  (setq avy-keys-alist
		`((avy-goto-char-timer . (?j ?k ?l ?f ?s ?d))
		  (avy-goto-char-2 . (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))
		  (avy-goto-line . (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))))
  (setq avy-style 'pre)
  (setq avy-background t)
  :config
  ;; jump to windows quickly
  (use-package ace-window
	:ensure t
	:bind (("C-x C-o" . ace-window)))
  ;; jump and open links fast
  (use-package ace-link
	:ensure t
	:demand t
	:config
	(ace-link-setup-default)))

;; window movement hydra
(defhydra hydra-windows (:color red :hint nil)
  "
 ^Move^    ^Size^    ^Change^                    ^Split^           ^Text^
 ^^^^^^^^^^^------------------------------------------------------------------
 ^ ^ _k_ ^ ^   ^ ^ _K_ ^ ^   _u_: winner-undo _r_: rotate  _v_: vertical     _+_: zoom in
 _h_ ^+^ _l_   _H_ ^+^ _L_   _r_: winner-redo _o_: other   _s_: horizontal   _-_: zoom out
 ^ ^ _j_ ^ ^   ^ ^ _J_ ^ ^   _c_: close                  _z_: zoom         _q_: quit
"
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)
  ("H" shrink-window-horizontally)
  ("K" shrink-window)
  ("J" enlarge-window)
  ("L" enlarge-window-horizontally)
  ("v" sk/split-right-and-move)
  ("s" sk/split-below-and-move)
  ("c" delete-window)
  ("r" sk/rotate-windows)
  ("o" ace-window :color blue)
  ("z" delete-other-windows)
  ("u" (progn
		 (winner-undo)
		 (setq this-command 'winner-undo)))
  ("r" winner-redo)
  ("+" text-scale-increase)
  ("-" text-scale-decrease)
  ("q" nil :exit t))
(bind-key* "C-x C-o" 'hydra-windows/body)

;; tags based navigation
(use-package ggtags
  :ensure t
  :diminish ggtags-mode
  :bind* (("M-=" . hydra-ggtags/body)
		  ("M-[" . ggtags-create-tags)
		  ("M-]" . ggtags-find-tag-regexp)
		  ("M-." . ggtags-update-tags)))
;; tags hydra
(defhydra hydra-ggtags (:hint nil :color red)
  "
 ^Tags^              ^Find^
------------------------------------------------------
 _c_: create tags    _d_: find definition      _q_: quit
 _u_: update tags    _f_: find reference
 _r_: reload         _t_: tags dwim"
  ("c" ggtags-create-tags)
  ("u" ggtags-update-tags)
  ("r" ggtags-reload-tags)
  ("d" ggtags-find-definition)
  ("f" ggtags-find-reference)
  ("t" ggtags-find-tag-dwim)
  ("q" nil :exit t))

;; dash documentation
(use-package dash-at-point
  :ensure t
  :bind (("M-'" . dash-at-point-with-docset))
  :bind* (("C-M-h" . dash-at-point-with-docset)))

;; change perspectives - similar to vim tabs
(use-package persp-mode
  :ensure t
  :diminish (persp-mode . " π")
  :init
  (setq persp-auto-save-opt 0)
  :commands (persp-next
			 persp-prev
			 persp-switch
			 persp-frame-switch
			 persp-window-switch
			 persp-rename
			 persp-copy
			 persp-kill
			 persp-save-state-to-file
			 persp-load-state-from-file
			 persp-temporarily-display-buffer
			 persp-switch-to-buffer
			 persp-add-buffer
			 persp-import-buffers
			 persp-import-win-config
			 persp-remove-buffer
			 persp-kill-buffer
			 persp-mode)
  :bind* (("M--" . hydra-persp-mode/body))
  :init
  (setq persp-autokill-buffer-on-remove 'kill-weak)
  :config
  (persp-mode 1))
;; hydra for perspectives
(defhydra hydra-persp-mode (:color blue :hint nil)
  "
 ^Persp^                                                                                         ^Desktop^
------------------------------------------------------------------------------------------------------------------------
 _j_: next  _s_: switch         _y_: copy          _l_: load from file    _b_: add buffer _r_: rm buffer   _S_: save      _q_: quit
 _k_: prev  _w_: switch in win  _d_: delete        _t_: switch w/o adding _i_: import all _K_: kill buffer _A_: save in dir
 _f_: frame _n_: rename         _p_: save to file  _a_: switch to buffer  _I_: import win _o_: switch off  _R_: read
  "
  ("j" persp-next :color red)
  ("k" persp-prev :color red)
  ("s" persp-switch)
  ("f" persp-frame-switch)
  ("w" persp-window-switch)
  ("n" persp-rename)
  ("y" persp-copy)
  ("d" persp-kill)
  ("p" persp-save-state-to-file)
  ("l" persp-load-state-from-file)
  ("t" persp-temporarily-display-buffer)
  ("a" persp-switch-to-buffer)
  ("b" persp-add-buffer)
  ("i" persp-import-buffers)
  ("I" persp-import-win-config)
  ("r" persp-remove-buffer)
  ("K" persp-kill-buffer)
  ("o" persp-mode)
  ("S" desktop-save)
  ("A" desktop-save-in-desktop-dir)
  ("R" desktop-read)
  ("q" nil :color blue))

;; switch window configs
(use-package eyebrowse
  :ensure t
  :diminish eyebrowse-mode
  :init
  (setq eyebrowse-wrap-around t)
  :bind* (("M-0" . eyebrowse-switch-to-window-config-0)
		  ("M-1" . eyebrowse-switch-to-window-config-1)
		  ("M-2" . eyebrowse-switch-to-window-config-2)
		  ("M-3" . eyebrowse-switch-to-window-config-3)
		  ("M-4" . eyebrowse-switch-to-window-config-4)
		  ("M-5" . eyebrowse-switch-to-window-config-5)
		  ("M-6" . eyebrowse-switch-to-window-config-6)
		  ("M-7" . eyebrowse-switch-to-window-config-7)
		  ("M-8" . eyebrowse-switch-to-window-config-8)
		  ("M-9" . eyebrowse-switch-to-window-config-9)
		  ("C--" . eyebrowse-switch-to-window-config)
		  ("M-!" . eyebrowse-close-window-config)
		  ("M-#" . eyebrowse-prev-window-config)
		  ("M-*" . eyebrowse-next-window-config)
		  ("M-+" . eyebrowse-last-window-config)
		  ("C-+" . eyebrowse-create-window-config)
		  ("C-%" . eyebrowse-rename-window-config))
  :config
  (eyebrowse-mode t))

;; wgrep + ag for refactoring - as an alternate to ivy/helm based commands
(use-package ag
  :ensure t
  :bind* (("M-s a" . ag-project-at-point)
		  ("M-s C-a" . ag-project-at-point)
		  ("M-s d" . ag)
		  ("M-s C-d" . ag)))
;; writeable grep
(use-package wgrep-ag
  :ensure t
  :init
  (setq wgrep-auto-save-buffer t)
  :bind* (("M-s e" . wgrep-change-to-wgrep-mode)
		  ("M-s C-e" . wgrep-change-to-wgrep-mode)
		  ("M-s f" . wgrep-finish-edit)
		  ("M-s C-f" . wgrep-finish-edit)
		  ("M-s k" . wgrep-abort-changes)
		  ("M-s C-k" . wgrep-abort-changes)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Debugging using GDB    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq gdb-many-windows t         ; let gdb invoke multiple windows
	  gdb-show-main t)           ; focus on the main window

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Remote edits - Tramp    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq tramp-default-method "ssh"                           ; remote log using ssh
	  tramp-backup-directory-alist backup-directory-alist) ; use the same backup directory for remote backups

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Improve aesthetics      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load one of these themes
(use-package zenburn-theme
  :ensure t)
(load-theme 'zenburn t)
;; (load-theme 'wombat t)
;; (load-theme 'leuven t)

;; better modeline
(use-package spaceline
  :ensure t
  :demand t
  :init
  (setq powerline-default-separator 'bar)
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)
  (spaceline-helm-mode))

;; rainbow paranthesis for easier viewing
(use-package rainbow-delimiters
  :ensure t
  :demand t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'text-mode-hook 'rainbow-delimiters-mode))

;; warn me if I go over a character limit
(use-package column-enforce-mode
  :ensure t
  :diminish column-enforce-mode
  :init
  (setq column-enforce-column 99)
  :config
  (progn
	(add-hook 'prog-mode-hook 'column-enforce-mode)))

;; highlight indentation levels
(use-package indent-guide
  :ensure t
  :diminish indent-guide-mode
  :bind* (("C-c o i" . indent-guide-mode))
  :config
  (add-hook 'python-mode-hook 'indent-guide-mode))

;; indicate margins
(use-package fill-column-indicator
  :ensure t
  :bind* (("C-c o m" . fci-mode))
  :init
  (setq fci-rule-width 5
		fci-rule-column 79)
  (setq fci-rule-color "#bebebe"))

;; toggle line numbers
(use-package nlinum
  :ensure t
  :commands (nlinum-mode
			 global-nlinum-mode)
  :bind* (("C-c o n" . nlinum-mode)))

;; visual regexp substitution
(use-package visual-regexp
  :ensure t
  :bind* (("M-s v" . vr/query-replace)
		  ("M-s C-v" . vr/query-replace))
  :config
  ;; change the regexp syntax
  (use-package visual-regexp-steroids
	:ensure t
	:bind* (("M-s V" . vr/select-query-replace)
			("M-s M-v" . vr/select-query-replace))))

;; restrict text to certain places - in my case the center of the screen
(use-package visual-fill-column
  :ensure t
  :defer t
  :bind (("C-c o o" . visual-fill-column-mode))
  :config
  (setq-default visual-fill-column-center-text t
				visual-fill-column-width 100
				visual-fill-column-fringes-outside-margins nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Convenience packages    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PDF tools for better pdf viewing
(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install))

;; easy copying between system clipboard and Emacs kill ring
(use-package osx-clipboard
  :ensure t
  :diminish osx-clipboard-mode
  :demand t
  :config
  (osx-clipboard-mode +1))

;; cleanup whitespace
(use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :config
  (ws-butler-global-mode))

;; restart emacs from emacs
(use-package restart-emacs
  :ensure t
  :bind* (("C-x c" . restart-emacs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Convenience functions    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; VERY useful
(require 'sk-functions)
;; hydra for some of these functions
(defhydra hydra-exchange (:color red :hint nil)
  "
 ^Line^         ^Words^       ^Change^
^^^^^^^^------------------------------------------------------------
 _j_: next      _h_: back     _r_: overwrite  _q_: quit
 _k_: previous  _l_: forward
  "
  ("k" sk/move-text-up)
  ("j" sk/move-text-down)
  ("h" sk/transpose-words-backward)
  ("l" sk/transpose-words-forward)
  ("r" overwrite-mode :color blue)
  ("q" nil :color blue))
(bind-key* "C-c x" 'hydra-exchange/body)

;;;;;;;;;;;;;;;;;;;
;;    Writing    ;;
;;;;;;;;;;;;;;;;;;;

;; markdown and pandoc support
(use-package markdown-mode
  :ensure t
  :mode ("\\.markdown\\'" "\\.mkd\\'" "\\.md\\'")
  :bind* (("C-\\ n" . hydra-markdown/body))
  :bind (:map markdown-mode-map
			  ("M-\\" . hydra-markdown/body)
			  ("C-0" . markdown-insert-link)
			  ("C-9" . markdown-insert-reference-link-dwim)
			  ("C-8" . markdown-insert-header-dwim)
			  ("C-2" . markdown-jump)
			  ("C-3" . markdown-insert-image)
			  ("C-4" . markdown-gfm-code-block)
			  ("C-5" . markdown-insert-italic)
			  ("C-6" . markdown-insert-bold)
			  ("C-7" . markdown-insert-blockquote))
  :init
  (setq markdown-open-command "/Applications/Markoff.app/Contents/MacOS/Markoff")
  :config
  (use-package pandoc-mode
	:ensure t
	:diminish pandoc-mode
	:config
	(add-hook 'markdown-mode-hook 'pandoc-mode)))

;; hydra for markdown stuff
(defhydra hydra-markdown (:color pink :hint nil)
  "
 ^Markdown^               ^Commands^      ^Insert^                                                     ^Headings^       ^Preview^
^^^^^^^^^^----------------------------------------------------------------------------------------------------------------------------------------------
 _j_/_J_: next   _f_: follow  _>_: promote    _l_: link       _m_: image      _c_: code block  _L_: list         _1_: h1  _5_: h5   _e_: export        _m_: markdown        _q_: quit
 _k_/_K_: prev   _g_: goto    _<_: demote     _r_: ref link   _M_: ref image  _p_: pre         _P_: pre region   _2_: h2  _6_: h6   _v_: preview       _x_: export and preview
 _h_: up level _d_: kill    _[_: move up    _w_: wiki link  _F_: footnote   _u_: blockquote  _Q_: quote region _3_: h3  _=_: H1   _R_: check refs    _C_: complete buffers
 _i_: italic   _b_: bold    _]_: move down  _U_: URI        ___: hrule      _B_: kbd         _t_: toggle img   _4_: h4  _-_: H2   _n_: cleanup linum _o_: pandoc
"
  ("k" markdown-previous-visible-heading)
  ("j" markdown-next-visible-heading)
  ("K" markdown-backward-same-level)
  ("J" markdown-forward-same-level)
  ("h" markdown-up-heading)
  ("i" markdown-insert-italic :color blue)
  ("f" markdown-follow-thing-at-point)
  ("g" markdown-jump)
  ("b" markdown-insert-bold :color blue)
  ("d" markdown-kill-thing-at-point)
  ("<" markdown-demote)
  (">" markdown-promote)
  ("[" markdown-move-up)
  ("]" markdown-move-down)
  ("_" markdown-insert-hr :color blue)
  ("l" markdown-insert-link :color blue)
  ("r" markdown-insert-reference-link :color blue)
  ("w" markdown-insert-wiki-link :color blue)
  ("U" markdown-insert-uri :color blue)
  ("F" markdown-insert-footnote :color blue)
  ("m" markdown-insert-image :color blue)
  ("M" markdown-insert-reference-image :color blue)
  ("c" markdown-insert-gfm-code-block :color blue)
  ("p" markdown-insert-pre :color blue)
  ("u" markdown-insert-blockquote :color blue)
  ("B" markdown-insert-kbd :color blue)
  ("L" markdown-insert-list-item :color blue)
  ("P" markdown-pre-region :color blue)
  ("Q" markdown-blockquote-region :color blue)
  ("t" markdown-toggle-inline-images)
  ("1" markdown-insert-header-atx-1)
  ("2" markdown-insert-header-atx-2)
  ("3" markdown-insert-header-atx-3)
  ("4" markdown-insert-header-atx-4)
  ("5" markdown-insert-header-atx-5)
  ("6" markdown-insert-header-atx-6)
  ("=" markdown-insert-header-setext-1)
  ("-" markdown-insert-header-setext-2)
  ("e" markdown-export :color blue)
  ("v" markdown-preview :color blue)
  ("R" markdown-check-refs :color blue)
  ("n" markdown-cleanup-list-numbers :color blue)
  ("m" markdown-other-window :color blue)
  ("x" markdown-export-and-preview :color blue)
  ("C" markdown-complete-buffer :color blue)
  ("o" pandoc-main-hydra/body :exit t)
  ("q" nil :color blue))

;; LaTeX support
(use-package tex-site
  :defer 2
  :ensure auctex
  :ensure auctex-latexmk
  :mode (("\\.tex\\'" . LaTeX-mode)
		 ("\\.xtx\\'" . LaTeX-mode))
  :bind (:map LaTeX-mode-map
			  ("M-\\" . hydra-latex/body)
			  ("C-0" . preview-at-point)
			  ("C-9" . preview-clearout-buffer)
			  ("C-2" . LaTeX-fill-region)
			  ("C-3" . LaTeX-fill-paragraph)
			  ("C-4" . TeX-command-master)
			  ("C-5" . LaTeX-fill-section)
			  ("C-6" . LaTeX-fill-environment)
			  ("C-7" . sk/setup-latex)
			  ("C-8" . LaTeX-environment))
  :bind* (("C-\\ x" . hydra-latex/body)
		  ("C-r i x" . LaTeX-mark-section)
		  ("C-r a x" . LaTeX-mark-section)
		  ("C-r a e" . LaTeX-mark-environment))
  :init
  (setq reftex-plug-into-AUCTeX t)
  (setq reftex-default-bibliography '("~/Dropbox/PhD/articles/tensors/tensors.bib"))
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq TeX-PDF-mode t)
  :config
  ;; LaTeX autocompletion
  (use-package company-auctex
	:ensure t
	:demand t
	:config
	(progn
	  (add-to-list 'company-backends 'company-auctex)))
  ;; Use Skim as viewer, enable source <-> PDF sync
  ;; make latexmk available via C-c C-c
  ;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
  (add-hook 'LaTeX-mode-hook (lambda ()
							   (push
								'("latexmk" "latexmk -xelatex -pdf %s" TeX-run-TeX nil t
								  :help "Run latexmk on file")
								TeX-command-list)))
  (add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))
  ;; use Skim as default pdf viewer
  ;; Skim's displayline is used for forward search (from .tex to .pdf)
  ;; option -b highlights the current line; option -g opens Skim in the background
  (setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
  (setq TeX-view-program-list
		'(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b"))))
;; custom function to setup latex mode properly - why isn't the normal use-package definition working?
(defun sk/setup-latex ()
  "hooks to setup latex properly"
  (interactive)
  (visual-line-mode)
  (flyspell-mode)
  (auctex-latexmk-setup)
  (reftex-mode)
  (turn-on-reftex))
(add-hook 'LaTeX-mode-hook 'sk/setup-latex)
(defun sk/diminish-reftex ()
  "diminish reftex because use-package is unable to do it"
  (interactive)
  (diminish 'reftex-mode))
(add-hook 'reftex-mode-hook 'sk/diminish-reftex)

;; hydra for latex
(defhydra hydra-latex (:color pink :hint nil)
  "
 ^LaTeX^                                                ^Commands^
^^^^^^^^^^----------------------------------------------------------------------------------------------------
 _l_: preview at pt   _i_: fill region    _v_: tex view     _c_: command master    _O_: cmd kill      _q_: quit
 _L_: preview buffer  _b_: fill para      _m_: master file  _o_: compile output    _C_: preview clear
 _f_: fill env        _s_: fill section   _h_: home buffer  _r_: cmd run all       _x_: setup
"
  ("l" preview-at-point)
  ("L" preview-buffer :color blue)
  ("f" LaTeX-fill-environment)
  ("i" LaTeX-fill-region)
  ("b" LaTeX-fill-paragraph)
  ("s" LaTeX-fill-section)
  ("v" TeX-view :color blue)
  ("m" TeX-master-file-ask :color blue)
  ("h" TeX-home-buffer)
  ("c" TeX-command-master :color blue)
  ("o" TeX-recenter-output-buffer :color blue)
  ("r" TeX-command-run-all :color blue)
  ("O" TeX-kill-job :color blue)
  ("C" preview-clearout-buffer :color blue)
  ("x" sk/setup-latex :color blue)
  ("q" nil :color blue))

;; pick out weasel words
(use-package writegood-mode
  :ensure t
  :diminish writegood-mode
  :bind* (("C-c {" . writegood-grade-level)
		  ("C-c }" . writegood-reading-ease))
  :config
  (progn
	(add-hook 'text-mode-hook 'writegood-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Version control    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; best git wrapper ever
(use-package magit
  :ensure t
  :bind* (("C-c e" . magit-status)
		  ("C-c g b" . magit-blame))
  :config
  ;; Github integration - press '@' in Magit status
  (use-package magithub
	:ensure t))

;; highlight diffs
(use-package diff-hl
  :ensure t
  :bind* (("C-r i h" . diff-hl-mark-hunk)
		  ("C-r C-i C-h" . diff-hl-mark-hunk)
		  ("C-r a h" . diff-hl-mark-hunk)
		  ("C-r C-a C-h" . diff-hl-mark-hunk)
		  ("M-j" . hydra-diff-hl-mark/body))
  :commands (global-diff-hl-mode
			 diff-hl-mode
			 diff-hl-next-hunk
			 diff-hl-previous-hunk
			 diff-hl-mark-hunk
			 diff-hl-diff-goto-hunk
			 diff-hl-revert-hunk)
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode)
  (diff-hl-margin-mode)
  (diff-hl-dired-mode))
;; hydra for diffs
(defhydra hydra-diff-hl-mark (:color red :hint nil)
  "
 ^Hunk^                   ^Mark^
^^^^^^^^------------------------------------------------------------
 _g_: goto   _j_: next      _h_: local prev   _f_: global next  _q_: quit
 _r_: revert _k_: previous  _l_: local next   _b_: global back
  "
  ("k" diff-hl-previous-hunk)
  ("j" diff-hl-next-hunk)
  ("g" diff-hl-diff-goto-hunk :color blue)
  ("r" diff-hl-revert-hunk)
  ("h" back-button-local-backward)
  ("l" back-button-local-forward)
  ("b" back-button-global-backward)
  ("f" back-button-global-forward)
  ("q" nil :color blue))

;; git timemachine
(use-package git-timemachine
  :ensure t
  :bind* (("C-c g t" . git-timemachine-toggle)))

;; posting gists
(use-package yagist
  :ensure t
  :init
  (setq yagist-encrypt-risky-config t)
  :bind*(("C-c g p" . yagist-region-or-buffer)
		 ("C-c g P" . yagist-region-or-buffer-private)))

;; browse remote packages
(use-package browse-at-remote
  :ensure t
  :bind* (("C-c g r" . browse-at-remote)))

;;;;;;;;;;;;;;;
;;    Org    ;;
;;;;;;;;;;;;;;;

;; org configuration
(require 'sk-org)

;;;;;;;;;;;;;;;;;;;;;;;
;;    Programming    ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; better tabs and spaces convention
(use-package editorconfig
  :ensure t
  :demand t
  :config
  (editorconfig-mode 1))

;; start services
(use-package prodigy
  :ensure t
  :bind* (("C-c b" . prodigy))
  :config
  (prodigy-define-tag
	:name 'blog
	:ready-message "Serving blog. Ctrl-C to shutdown server")
  (prodigy-define-service
	:name "personal blog build"
	:command "hexo"
	:args '("generate")
	:cwd "/Users/sriramkswamy/Dropbox/blog"
	:tags '(blog)
	:kill-signal 'sigkill)
  (prodigy-define-service
	:name "personal blog view"
	:command "hexo"
	:args '("server")
	:cwd "/Users/sriramkswamy/Dropbox/blog"
	:tags '(blog)
	:kill-signal 'sigkill
	:kill-process-buffer-on-stop t)
  (prodigy-define-service
	:name "personal blog publish"
	:command "hexo"
	:args '("deploy")
	:cwd "/Users/sriramkswamy/Dropbox/blog"
	:tags '(blog)
	:kill-signal 'sigkill)
  (prodigy-define-service
	:name "watchandcode blog build"
	:command "hexo"
	:args '("generate")
	:cwd "/Users/sriramkswamy/Downloads/JS/watchandcode"
	:tags '(blog)
	:kill-signal 'sigkill)
  (prodigy-define-service
	:name "watchandcode blog view"
	:command "hexo"
	:args '("server")
	:cwd "/Users/sriramkswamy/Downloads/JS/watchandcode"
	:tags '(blog)
	:kill-signal 'sigkill
	:kill-process-buffer-on-stop t)
  (prodigy-define-service
	:name "watchandcode blog publish"
	:command "hexo"
	:args '("deploy")
	:cwd "/Users/sriramkswamy/Downloads/JS/watchandcode"
	:tags '(blog)
	:kill-signal 'sigkill))

;; YAML mode
(use-package yaml-mode
  :ensure t
  :mode ("\\.yml$" "\\.yaml$"))

;; error checking
(use-package flycheck
  :ensure t
  :defer 2
  :diminish flycheck-mode
  :commands (flycheck-buffer
			 flycheck-previous-error
			 flycheck-next-error
			 flycheck-list-errors
			 flycheck-explain-error-at-point
			 flycheck-display-error-at-point
			 flycheck-select-checker
			 flycheck-verify-setup)
  :config
  (global-flycheck-mode))
;; hydra for flycheck
(defhydra hydra-flycheck (:color red :hint nil)
  "
 _j_: next     _l_: list    _d_: display  _s_: select  _q_: quit
 _k_: previous _e_: explain _c_: check    _v_: verify
  "
  ("j" flycheck-next-error)
  ("k" flycheck-previous-error)
  ("l" flycheck-list-errors :color blue)
  ("e" flycheck-explain-error-at-point)
  ("d" flycheck-display-error-at-point)
  ("c" flycheck-buffer)
  ("s" flycheck-select-checker)
  ("v" flycheck-verify-setup)
  ("q" nil :color blue))
(bind-key* "C-c l" 'hydra-flycheck/body)

;; autocompletion
(use-package company
  :ensure t
  :init
  (setq company-minimum-prefix-length 2
		company-require-match 0
		company-selection-wrap-around t
		company-dabbrev-downcase nil
		company-tooltip-limit 20                      ; bigger popup window
		company-tooltip-align-annotations 't          ; align annotations to the right tooltip border
		company-idle-delay .2                         ; decrease delay before autocompletion popup shows
		company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
  (eval-after-load 'company
	'(add-to-list 'company-backends '(company-files
									  company-capf)))
  :bind* (("M-u" . company-complete)
		  ("C-c o c" . company-mode))
  :bind* (("C-c y f" . company-files)
		  ("C-c y a" . company-dabbrev)
		  ("C-c y d" . company-ispell)
		  :map company-active-map
		  ("C-n"    . company-select-next)
		  ("C-p"    . company-select-previous)
		  ([return] . company-complete-selection)
		  ([tab]    . yas-expand)
		  ("TAB"    . yas-expand)
		  ("C-w"    . backward-kill-word)
		  ("C-c"    . company-abort)
		  ("C-c"    . company-search-abort))
  :diminish company-mode
  :config
  (global-company-mode))

;; project management
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :commands (projectile-project-root)
  :bind* (("C-*" . projectile-find-other-file)
		  ("C-#" . projectile-find-other-file-other-window)
		  ("C-!" . projectile-commander)
		  ("C-$" . projectile-run-eshell)
		  ("C-&" . projectile-run-async-shell-command-in-root))
  :init
  (setq projectile-keymap-prefix (kbd "C-x p"))
  :config
  (projectile-mode))

;; Shell interaction
(require 'sk-shell)

;; additional configuration - this is where language specific configurations reside
(require 'sk-programming)

;;;;;;;;;;;;;;;
;;    Fun    ;;
;;;;;;;;;;;;;;;

;; google stuff
(use-package google-this
  :ensure t
  :config
  (defun sk/google-this ()
	"Google efficiently"
	(interactive)
	(if (region-active-p)
		(google-this-region 1)
	  (google-this-symbol 1)))
  :bind* (("C-c \\" . sk/google-this)))

;; creepy function
(defun hello-human ()
  "The scratch function for fun"
  (interactive)
  (message (concat "I know who you are, " user-full-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Narrowing packages    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ivy
;; (require 'sk-ivy)
;; helm
(require 'sk-helm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Included packages    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; dired, doc-view and all that
(require 'sk-included)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Local configuration    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load the local configuration if it exists
(when (file-exists-p (concat user-emacs-directory "local.el"))
  (load-file (concat user-emacs-directory "local.el")))

;;;;;;;;;;;;;;;;;;;;;;;;
;;    Start server    ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package server
  :config
  (unless (server-running-p)
	(server-start)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Reduce GC threshold    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1 MB now
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 1 1024 1024))))
