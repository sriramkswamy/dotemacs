;; Emacs configuration

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Change some default settings    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; General settings
(setq user-full-name "Sriram Krishnaswamy")									  	; Hi Emacs, I'm Sriram
(setq gc-cons-threshold (* 500 1024 1024))									  	; increase the threshold for garbage collection - 100 MB
(setq delete-old-versions -1)												  	; delete excess backup versions silently
(setq version-control t)													  	; use version control for backups
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))				  	; which directory to put backups file
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))	; transform backups file name
(setq inhibit-startup-screen t)												  	; inhibit useless and old-school startup screen
(setq visible-bell nil)														  	; no visible bell for errors
(setq ring-bell-function 'ignore)											  	; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8)										  	; use utf-8 by default for reading
(setq coding-system-for-write 'utf-8)										  	; use utf-8 by default for writing
(setq sentence-end-double-space nil)										  	; sentence SHOULD end with only a point.
(setq-default fill-column 80)													; toggle wrapping text at the 80th character
(setq initial-major-mode 'fundamental-mode)                                     ; set the mode of the initial scratch buffer
(setq initial-scratch-message "")           								  	; print nothing and leave screen at insert mode
(menu-bar-mode -1)															  	; deactivate the menubar
(tooltip-mode -1)															  	; deactivate the tooltip
(blink-cursor-mode -1)														  	; don't blink the cursor
(defun display-startup-echo-area-message () (message "Let the games begin!")) 	; change the default startup echo message
(setq-default truncate-lines t)												  	; if line exceeds screen, let it
(setq large-file-warning-threshold (* 15 1024 1024))						  	; increase threshold for large files
(fset 'yes-or-no-p 'y-or-n-p)												  	; prompt for 'y' or 'n' instead of 'yes' or 'no'
(setq-default abbrev-mode t)												  	; turn on abbreviations by default
(setq recenter-positions '(middle top bottom))								  	; recenter from the top instead of the middle
(put 'narrow-to-region 'disabled nil)										  	; enable narrowing to region
(put 'narrow-to-defun 'disabled nil)										  	; enable narrowing to function
(when (fboundp 'winner-mode)												  	; when you can find 'winner-mode'
  (winner-mode 1))															  	; activate winner mode
(setq recentf-max-saved-items 1000											  	; set the number of recent items to be saved
      recentf-exclude '("/tmp/" "/ssh:"))									  	; exclude the temporary and remote files accessed recently
(setq ns-use-native-fullscreen nil)											  	; don't use the native fullscreen - more useful in a Mac
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))		  	; setup a new custom file
(when (file-exists-p custom-file)											  	; if the custom file exists
  (load custom-file :noerror :nomessage))									  	; load the custom file but don't show any messages or errors
(put 'scroll-left 'disabled nil)											  	; enable sideward scrolling
(define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word)			  	; backward kill word in minibuffer
(setq enable-recursive-minibuffers t)										  	; use the minibuffer while using the minibuffer
(setq echo-keystrokes 0.05)													  	; when to echo keystrokes
(setq-default ibuffer-expert t)												    ; don't ask confirmation when deleting unmodified buffers
(setq frame-resize-pixelwise t)												    ; resize based on pixels to remove annoying gaps
(setq imenu-auto-rescan t)												        ; rescan automatically for new tags

;; how tabs are seen and added
(setq-default tab-width 4)
(setq-default tab-stop-list
			  '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80))

;; how to interpret the command key and the option key on a Mac
(when (eq system-type 'darwin)
  (setq mac-command-key-is-meta nil)
  ;; (setq mac-command-modifier 'meta)
  (setq mac-option-key-is-meta t)
  (setq mac-option-modifier 'meta))

;; General frame size and configuration plus Linux HiDPI hacks
(when (display-graphic-p)
  (tool-bar-mode -1)															  	; deactivate the toolbar
  (cond ((eq system-type 'gnu/linux)                 ; if system is GNU/Linux
		 (when (display-graphic-p)
		   (scroll-bar-mode -1)
		   (set-frame-font "DejaVu Sans Mono"))
		 (setq initial-frame-alist													  	; initial frame size
			   '((width . 100)														  	; characters in a line
				 (height . 45)))														  	; number of lines
		 (setq default-frame-alist													  	; subsequent frame size
			   '((width . 100)														  	; characters in a line
				 (height . 45))))
		((eq system-type 'darwin)                    ; if system is macOS
		 ;; (mac-auto-operator-composition-mode)        ; ligature support
		 ;; (set-frame-font "Fira Code")
		 (setq initial-frame-alist													  	; initial frame size
			   '((width . 100)														  	; characters in a line
				 (height . 45)))														  	; number of lines
		 (setq default-frame-alist													  	; subsequent frame size
			   '((width . 100)														  	; characters in a line
				 (height . 45)))														  	; number of lines
		 (when (display-graphic-p)
		   (scroll-bar-mode -1)))
		((eq system-type 'windows-nt)                ; if system is Windows
		 (set-frame-font "Lucida Sans Typewriter")
		 (setq initial-frame-alist													  	; initial frame size
			   '((width . 100)														  	; characters in a line
				 (height . 45)))														  	; number of lines
		 (setq default-frame-alist													  	; subsequent frame size
			   '((width . 100)														  	; characters in a line
				 (height . 45)))														  	; number of lines
		 (when (display-graphic-p)
		   (scroll-bar-mode -1)))))

;; dummy function
(defun sk/nothing ()
  "Sends a message saying nothing here"
  (interactive)
  (message "Nothing here!"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Package management    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Include the module to install packages
(require 'package)

;; tells emacs not to load any packages before starting up
(setq package-enable-at-startup nil)

;; the following lines tell emacs where on the internet to look up
;; for new packages.
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
						 ("gnu"       . "http://elpa.gnu.org/packages/")
						 ("melpa"     . "https://melpa.org/packages/")))

;; initialize the packages
(package-initialize)

;; allow unsigned packages
(setq package-check-signature nil)

;;;;;;;;;;;;;;;;;;;;;;;
;;    use-package    ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path (concat user-emacs-directory "use-package/"))
  (require 'use-package))

;; Keep the mode-line clean
(use-package diminish
  :ensure t
  :defer t)

;; extensions for use-package
(use-package use-package-ensure-system-package
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Built-in packages    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; directory listing
(use-package dired
  :hook (dired-mode . dired-hide-details-mode)
  :init
  (setq dired-dwim-target t
        dired-recursive-copies 'top
        dired-recursive-deletes 'top
        dired-listing-switches "-alh"))

;; dired library for additional functions
(use-package dired-x
  :commands
  (dired-jump))

;; pdf/image viewing
(use-package doc-view
  :hook ((doc-view-mode . doc-view-fit-page-to-window)
         (doc-view-minor-mode . doc-view-fit-page-to-window))
  :init
  (setq doc-view-continuous t))

;; builtin version control
(use-package vc
  :init
  (setq vc-make-backup-files t
        vc-follow-symlinks t))

;; diff management
(use-package ediff
  :init
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally))

;; abbrev
(use-package abbrev
  :diminish abbrev-mode
  :init
  (setq save-abbrevs 'silently)
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))

;; subword navigation
(use-package subword
  :hook (prog-mode . subword-mode)
  :diminish subword-mode
  :config
  (subword-mode 1))

;; visual line - soft wrap
(defun sk/diminish-visual-line ()
  "diminish visual line mode"
  (interactive)
  (diminish 'visual-line-mode " ω"))

(use-package visual-line
  :hook ((text-mode . visual-line-mode)
		 (visual-line-mode . sk/diminish-visual-line))
  :commands
  (visual-line-mode))

;; auto fill mode
(use-package simple
  :diminish auto-fill-function)

;; auto revert mode
(use-package autorevert
  :defer t
  :diminish auto-revert-mode)

;; documentation helper
(use-package eldoc
  :hook (prog-mode . eldoc-mode)
  :diminish eldoc-mode)

;; spell check
(use-package flyspell
  :hook ((text-mode . flyspell-mode)
         (org . flyspell-mode))
  :diminish (flyspell-mode . " φ"))

;; save history
(use-package savehist
  :defer t
  :config
  (savehist-mode 1))

;; debugging gdb
(use-package gud
  :init
  (setq gdb-many-windows t
        gdb-show-main t)
  :commands
  (gud-run
   gud-down
   gud-up
   gud-next
   gud-step
   gud-next
   gud-finish
   gud-cont
   gud-goto-info
   gud-basic-call
   gud-print
   gud-refresh
   gud-find-c-expr
   gdb-toggle-breakpoint
   gdb-delete-breakpoint
   gdb))

;; ibuffer
(use-package ibuffer
  :commands
  (ibuffer)
  :bind (:map ibuffer-mode-map
			  ("a a" . ibuffer-mark-by-mode)
			  ("a s" . ibuffer-mark-special-buffers)
			  ("a m" . ibuffer-mark-unsaved-buffers)
			  ("a u" . ibuffer-mark-modified-buffers)
			  ("a r" . ibuffer-mark-by-name-regexp)
			  ("a f" . ibuffer-mark-by-file-name-regexp)
			  ("a d" . ibuffer-mark-dired-buffers)
			  ("a h" . ibuffer-mark-hel-buffers)))

;; line number after emacs 26
(if (version< emacs-version "26")
    (message "Line number mode not activated")
  (use-package display-line-numbers
    :init
    (setq display-line-numbers-type 'relative)
    :hook ((prog-mode . display-line-numbers-mode)
		   (text-mode . display-line-numbers-mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Third party packages    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; extra functions
(add-to-list 'load-path
             (expand-file-name "defuns" user-emacs-directory))

;; extra macros
(add-to-list 'load-path
             (expand-file-name "macros" user-emacs-directory))

;; extra major/minor modes
(add-to-list 'load-path
             (expand-file-name "modes" user-emacs-directory))

;; language specific setup
(add-to-list 'load-path
             (expand-file-name "lang" user-emacs-directory))

;; other configuration
(add-to-list 'load-path
             (expand-file-name "config" user-emacs-directory))

;; string manipulation library
(use-package s
  :ensure t)

;; list library
(use-package dash
  :ensure t)

;; Make sure the path is set right for macOS after installing brew
(if (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :ensure t
    :defer 1
    :ensure-system-package
    (brew ."/usr/bin/ruby -e \"$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)\"")
    :bind* (("C-x x" . exec-path-from-shell-initialize)
			("C-x y" . exec-path-from-shell-copy-env))
    :init
    (setq exec-path-from-shell-check-startup-files nil)
    :config
    (exec-path-from-shell-initialize))
  (use-package exec-path-from-shell
    :ensure t
    :defer 1
	:bind* (("C-x x" . exec-path-from-shell-initialize)
			("C-x y" . exec-path-from-shell-copy-env))
    :init
    (setq exec-path-from-shell-check-startup-files nil)
    :config
    (exec-path-from-shell-initialize)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Core functions    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Macros
(require 'sk-repl-macros)

;; functions
(require 'sk-buffer-defuns)
(require 'sk-dired-defuns)
(require 'sk-display-defuns)
(require 'sk-doc-defuns)
(require 'sk-editing-defuns)
(require 'sk-navigation-defuns)
(require 'sk-search-defuns)
(require 'sk-debug-defuns)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Core packages    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; hint for bindings
(use-package which-key
  :ensure t
  :demand t
  :diminish which-key-mode
  :bind* (("C-c ?" . which-key-show-top-level))
  :config
  ;; workaround for emacs 26
  (if (version< emacs-version "26")
      (message "Tracking stable Emacs")
    (defalias 'display-buffer-in-major-side-window 'window--make-major-side-window))
  ;; turn on which key and add some names for default/common prefixes
  (which-key-enable-god-mode-support)
  (which-key-mode))

;; key chord for when necessary
(use-package use-package-chords
  :ensure t
  :demand t
  :config
  ;; (key-chord-define-global "  " 'sk/enable-ryo-modal-mode)
  (key-chord-mode 1))

;; modality
(require 'sk-ryo-modal)

;; navigation
(require 'sk-nav)

;; editing
(require 'sk-edit)

;; version control
(require 'sk-versions)

;; semi modality
(require 'sk-hydra)

;; narrowing framework
(require 'sk-narrow)

;; select a good theme
(use-package flatui-theme
	:ensure t)
(use-package flatui-dark-theme
	:ensure t)

;; choose a single theme or change it
(load-theme 'wombat t)
;; (if (display-graphic-p)
;; 	(load-theme 'flatui t)
;;   (load-theme 'flatui-dark))

(when (eq system-type 'darwin)
  ;; fancy battery
  (use-package fancy-battery
	:ensure t
	:init
	(setq fancy-battery-show-percentage t)
	:config
	(fancy-battery-mode)))

;; restart emacs from emacs
(use-package restart-emacs
  :ensure t
  :bind* (("C-x C" . restart-emacs)))

;; treemacs for folder tree
(if (version< emacs-version "25.2")
	(use-package neotree
	  :ensure t
	  ;; :init
	  :commands
	  (neotree-toggle
	   neotree))
  (use-package treemacs
	:ensure t
	:init
	(setq treemacs-follow-after-init          t
		  treemacs-width                      25
		  treemacs-indentation                2
		  treemacs-collapse-dirs              (if (executable-find "python") 3 0)
		  treemacs-silent-refresh             nil
		  treemacs-change-root-without-asking nil
		  treemacs-sorting                    'alphabetic-desc
		  treemacs-show-hidden-files          t
		  treemacs-never-persist              nil
		  treemacs-is-never-other-window      nil
		  treemacs-goto-tag-strategy          'refetch-index)
	:commands
	(treemacs-toggle
	 treemacs)))

;; shell/tmux configuration
(require 'sk-shell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Language support    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; emacs semantic support
(require 'sk-semantic)

;; linting/checking
(require 'sk-linter)

;; debugging
(require 'sk-debug)

;; auto completion
(require 'sk-complete)

;; emacs lisp support
(require 'sk-elisp)

;; org support
(require 'sk-org)

;; latex support
(require 'sk-latex)

;; plain text format support
(require 'sk-text)

;; lsp support
(require 'sk-lsp)

;; matlab support
(require 'sk-matlab)

;; stats support
(require 'sk-stats)

;; python support
(require 'sk-python)

;; clang based languages support
(require 'sk-clang)

;; support for all the various config files
(require 'sk-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Extra services    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; trailing whitespace management
(use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :commands
  (ws-butler-mode
   ws-butler-global-mode
   ws-butler-clean-region
   ws-butler-maybe-trim-eob-lines)
  :hook ((after-init . ws-butler-global-mode)))

;; mode specific scratch buffer
(use-package scratch
  :ensure t
  :commands
  (scratch))

;; start background processes
(use-package prodigy
  :ensure t
  :commands (prodigy)
  :config
  (prodigy-define-tag
	:name 'latexmk
	:ready-message "=== Watching for updated files. Use ctrl/C to stop ...")
  (prodigy-define-service
	:name "beamer slides+notes current"
	:command "latexmk"
	:args '("-pdf" "-pvc" "./both.tex")
	:cwd (directory-file-name default-directory)
	:tags '(latexmk)
	:kill-signal 'sigkill)
  (prodigy-define-service
	:name "beamer slides current"
	:command "latexmk"
	:args '("-pdf" "-pvc" "./slides.tex")
	:cwd (directory-file-name default-directory)
	:tags '(latexmk)
	:kill-signal 'sigkill)
  (prodigy-define-service
	:name "beamer notes current"
	:command "latexmk"
	:args '("-pdf" "-pvc" "./notes.tex")
	:cwd (directory-file-name default-directory)
	:tags '(latexmk)
	:kill-signal 'sigkill)
  (prodigy-define-service
	:name "beamer slides+notes parent"
	:command "latexmk"
	:args '("-pdf" "-pvc" "./both.tex")
	:cwd (file-name-directory (directory-file-name default-directory))
	:tags '(latexmk)
	:kill-signal 'sigkill)
  (prodigy-define-service
	:name "beamer slides parent"
	:command "latexmk"
	:args '("-pdf" "-pvc" "./slides.tex")
	:cwd (file-name-directory (directory-file-name default-directory))
	:tags '(latexmk)
	:kill-signal 'sigkill)
  (prodigy-define-service
	:name "beamer notes parent"
	:command "latexmk"
	:args '("-pdf" "-pvc" "./notes.tex")
	:cwd (file-name-directory (directory-file-name default-directory))
	:tags '(latexmk)
	:kill-signal 'sigkill)
  (prodigy-define-service
	:name "beamer slides+notes project"
	:command "latexmk"
	:args '("-pdf" "-pvc" "./both.tex")
	:cwd (file-name-directory (vc-root-dir))
	:tags '(latexmk)
	:kill-signal 'sigkill)
  (prodigy-define-service
	:name "beamer notes project"
	:command "latexmk"
	:args '("-pdf" "-pvc" "./notes.tex")
	:cwd (file-name-directory (vc-root-dir))
	:tags '(latexmk)
	:kill-signal 'sigkill)
  (prodigy-define-service
	:name "beamer slides project"
	:command "latexmk"
	:args '("-pdf" "-pvc" "./slides.tex")
	:cwd (file-name-directory (vc-root-dir))
	:tags '(latexmk)
	:kill-signal 'sigkill))

;;;;;;;;;;;;;;;;;;;;;;;;
;;    Key bindings    ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; immutable bindings
(bind-keys*
 ("C-w"     . sk/kill-region-or-backward-word)
 ("M-w"     . sk/copy-region-or-line)
 ("M-c"     . sk/toggle-letter-case)
 ("M-h"     . sk/remove-mark)
 ("C-x k"   . sk/kill-buffer)
 ("C-x w"   . save-buffers-kill-terminal))

;; mutable bindings
(bind-keys
 ("C-o"     . sk/open-line-above)
 ("M-o"     . sk/open-line-below)
 ("M-n"		. sk/nothing)
 ("M-p"		. sk/nothing)
 ("C-x C-b" . ibuffer)
 ("C-c C-c" . sk/nothing)
 ("C-c '"	. sk/nothing)
 ("C-c C-f" . sk/nothing))

;; modal bindings
(require 'sk-ryo-bindings)
(require 'sk-ryo-which-key)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Reduce GC threshold    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1 MB now
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 1 1024 1024))))
