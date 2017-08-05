;; Emacs configuration

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Change some settings    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; General settings
(setq user-full-name "Sriram Krishnaswamy")									  	; Hi Emacs, I'm Sriram
(setq gc-cons-threshold (* 500 1024 1024))									  	; increase the threshold for garbage collection - 100 MB
(setq delete-old-versions -1)												  	; delete excess backup versions silently
(setq version-control t)													  	; use version control for backups
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))				  	; which directory to put backups file
(setq vc-make-backup-files t)												  	; make backups file even when in version controlled dir
(setq vc-follow-symlinks t)													  	; don't ask for confirmation when opening symlinked file
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))	; transform backups file name
(setq inhibit-startup-screen t)												  	; inhibit useless and old-school startup screen
(setq visible-bell nil)														  	; no visible bell for errors
(setq ring-bell-function 'ignore)											  	; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8)										  	; use utf-8 by default for reading
(setq coding-system-for-write 'utf-8)										  	; use utf-8 by default for writing
(setq sentence-end-double-space nil)										  	; sentence SHOULD end with only a point.
(setq-default fill-column 80)													; toggle wrapping text at the 80th character
(setq initial-scratch-message "(hello-human)")								  	; print a default message in the empty scratch buffer opened at startup
(menu-bar-mode -1)															  	; deactivate the menubar
(tool-bar-mode -1)															  	; deactivate the toolbar
(scroll-bar-mode -1)														  	; deactivate the scrollbar
(tooltip-mode -1)															  	; deactivate the tooltip
(setq initial-frame-alist													  	; initial frame size
	  '((width . 102)														  	; characters in a line
		(height . 54)))														  	; number of lines
(setq default-frame-alist													  	; subsequent frame size
	  '((width . 100)														  	; characters in a line
		(height . 52)))														  	; number of lines
(blink-cursor-mode -1)														  	; don't blink the cursor
(defun display-startup-echo-area-message () (message "Let the games begin!")) 	; change the default startup echo message
(setq-default truncate-lines t)												  	; if line exceeds screen, let it
(setq large-file-warning-threshold (* 15 1024 1024))						  	; increase threshold for large files
(fset 'yes-or-no-p 'y-or-n-p)												  	; prompt for 'y' or 'n' instead of 'yes' or 'no'
(setq-default abbrev-mode t)												  	; turn on abbreviations by default
(setq save-abbrevs 'silently)												  	; don't inform when saving new abbreviations
(setq ediff-window-setup-function 'ediff-setup-windows-plain				  	; have a plain setup for ediff
	  ediff-split-window-function 'split-window-horizontally)				  	; show two vertical windows instead of horizontal ones
(setq recenter-positions '(top middle bottom))								  	; recenter from the top instead of the middle
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
(savehist-mode)																  	; keep persistent history
(subword-mode 1)															  	; move correctly over camelCase words
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))	  	; load more configuration from the 'config' folder
(put 'scroll-left 'disabled nil)											  	; enable sideward scrolling
(define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word)			  	; backward kill word in minibuffer
(setq enable-recursive-minibuffers t)										  	; use the minibuffer while using the minibuffer
(setq echo-keystrokes 0.05)													  	; when to echo keystrokes
(setq-default ibuffer-expert t)												    ; don't ask confirmation when deleting unmodified buffers
(setq frame-resize-pixelwise t)												    ; resize based on pixels to remove annoying gaps

;; line number after emacs 26
(if (version< emacs-version "26")
	(message "Line number mode not activated")
  (global-display-line-numbers-mode))

;; how tabs are seen and added
(setq-default tab-width 4)
(setq-default tab-stop-list
			  '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80))

;; how to interpret the command key and the option key in a Mac
(when (eq system-type 'darwin)
  (setq mac-command-key-is-meta nil)
  ;; (setq mac-command-modifier 'meta)
  (setq mac-option-key-is-meta t)
  (setq mac-option-modifier 'meta))

;; dired settings
(setq dired-dwim-target t                            ; do what i mean
	  dired-recursive-copies 'top                    ; copy recursively
	  dired-recursive-deletes 'top                   ; delete recursively
	  dired-listing-switches "-alh")
(add-hook 'dired-mode-hook 'dired-hide-details-mode) ; hide the unimportant details

;; doc-view settings
(setq doc-view-continuous t)                                         ; continuous PDF mode
(add-hook 'doc-view-mode-hook 'doc-view-fit-page-to-window)          ; fit page to window
(add-hook 'doc-view-minor-mode-hook 'doc-view-fit-page-to-window)    ; fit page to window

;; debugger
;; (setq debug-on-error t)  ; toggle debug to show backtrace on error
(setq gdb-many-windows t ; let gdb invoke multiple windows
	  gdb-show-main t)   ; focus on the main window

;; remote editing
(setq tramp-default-method "ssh"                           ; remote log using ssh
	  tramp-backup-directory-alist backup-directory-alist) ; use the same backup directory for remote backups

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

;; allow unsigned packages
(setq package-check-signature nil)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents)               ; updage packages archive
  (package-install 'use-package))          ; and install the most recent version of use-package
(require 'use-package)                     ; Source use-package
;; if starting via daemon, just load everything
(if (daemonp)
      (setq use-package-always-demand t))

;; Bootstrap `org'
(unless (package-installed-p 'org) ; unless it is already installed
  (package-refresh-contents)       ; updage packages archive
  (package-install 'org))

;; Keep the mode-line clean
(use-package diminish                 ; diminish stuff from the mode-line
  :ensure t                           ; ensure the package is present
  :demand t                           ; load the package immediately
  :diminish (visual-line-mode . " ω") ; diminish the `visual-line-mode'
  :diminish hs-minor-mode             ; diminish the `hs-minor-mode'
  :diminish abbrev-mode               ; diminish the `abbrev-mode'
  :diminish auto-fill-function        ; diminish the `auto-fill-function'
  :diminish subword-mode)             ; diminish the `subword-mode'
(add-hook 'text-mode-hook 'visual-line-mode)

;; diminish auto-revert mode
(defun sk/diminish-auto-revert ()
  (interactive)
  (diminish 'auto-revert-mode ""))
(add-hook 'auto-revert-mode-hook 'sk/diminish-auto-revert)

;; diminish auto-revert mode
(defun sk/diminish-hs-minor ()
  (interactive)
  (diminish 'hs-minor-mode ""))
(add-hook 'hs-minor-mode-hook 'sk/diminish-hs-minor)
(add-hook 'text-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; subword-mode for camelCase operations
(add-hook 'prog-mode-hook 'subword-mode)

;; diminish eldoc mode
(defun sk/diminish-eldoc ()
  (interactive)
  (diminish 'eldoc-mode ""))
(add-hook 'eldoc-mode-hook 'sk/diminish-eldoc)

;; diminish flyspell mode
(defun sk/diminish-flyspell ()
  (interactive)
  (diminish 'flyspell-mode " φ"))
(add-hook 'flyspell-mode-hook 'sk/diminish-flyspell)
(add-hook 'text-mode-hook 'flyspell-mode)

;; string manipulation library
(use-package s
  :ensure t)

;; list library
(use-package dash
  :ensure t)

;; Make sure the path is set right
(use-package exec-path-from-shell
  :ensure t
  :demand t
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  ;; (exec-path-from-shell-copy-env "PYTHONPATH")
  (exec-path-from-shell-initialize))

;; ;; better package menu
;; (use-package paradox
;;   :ensure t
;;   :commands (paradox-list-packages))

;; load a dired library when called
(use-package dired-x
  :commands (dired-jump))

;;;;;;;;;;;;;;;;;;;;;;;;
;;    Key bindings    ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; (bind-key "C-c x" 'my-ctrl-c-x-command)
;; (bind-key* "<C-return>" 'other-window)
;; (bind-key "C-c x" 'my-ctrl-c-x-command some-other-mode-map)
;; (unbind-key "C-c x" some-other-mode-map)
;;  (bind-keys :map dired-mode-map
;;             ("o" . dired-omit-mode)
;;             ("a" . some-custom-dired-function))
;;  (bind-keys :prefix-map my-customize-prefix-map
;;             :prefix "C-c c"
;;             ("f" . customize-face)
;;             ("v" . customize-variable))
;;  (bind-keys*
;;   ("C-o" . other-window)
;;   ("C-M-n" . forward-page)
;;   ("C-M-p" . backward-page))
;; Change some default emacs bindings
(bind-keys*
 ("C-j"		. electric-newline-and-maybe-indent)
 ("C-z"		. recenter-top-bottom)
 ("M-k"		. kill-whole-line)
 ("C-x Q"	. save-buffers-kill-emacs)
 ("C-x w"	. delete-frame)
 ("C-x f"	. make-frame-command)
 ("C-c SPC" . rectangle-mark-mode)
 ("C-x k"	. kill-this-buffer)
 ("C-x y"	. desktop-read)
 ("C-x x"	. desktop-remove)
 ("C-x t"	. desktop-save)
 ("C-x j"	. desktop-change-dir)
 ("M-:"		. goto-line)
 ("M-\""	. move-to-column)
 ("C-c g f" . find-file-at-point)
 ("C-c M"	. woman)
 ("C-("		. kmacro-start-macro)
 ("C-)"		. kmacro-end-macro)
 ("C-@"		. kmacro-end-or-call-macro-repeat)
 ("C-^"		. mode-line-other-buffer)
 ("C-c j"	. join-line)
 ("C-x C-y" . delete-blank-lines)
 ("C-x C-o" . other-window)
 ("C-x C-j" . winner-undo)
 ("C-x C-a" . winner-redo)
 ("C-c o f" . flyspell-mode)
 ("C-c o p" . flyspell-prog-mode)
 ("C-c o v" . visual-line-mode)
 ("C-c o b" . display-battery-mode)
 ("C-c o t" . display-time-mode)
 ("C-c o r" . scroll-bar-mode)
 ("C-c o d" . desktop-save-mode)
 ("C-x 7"	. wdired-change-to-wdired-mode)
 ("C-x 9"	. wdired-abort-changes)
 ("C-x s"	. set-frame-name)
 ("C-x c"	. save-buffers-kill-terminal))
(bind-keys
 ("C-x b"	. ibuffer)
 ("C-x C-b"	. switch-to-buffer)
 ("C-h j"	. describe-face)
 ("M-n"		. next-error)
 ("M-p"		. previous-error)
 ("C-c C-c" . sk/nothing)
 ("C-c b" . sk/nothing)
 ("C-c C-f" . sk/nothing))

;;;;;;;;;;;;;;;;
;;    Core    ;;
;;;;;;;;;;;;;;;;

;; core pillars of my configuration
(require 'sk-core)

;; git and version control
(require 'sk-vcs)

;; load theme
;; (load-theme 'leuven t)

(use-package material-theme
  :ensure t
  :config
  (if (display-graphic-p)
	  (load-theme 'material-light t)
	(load-theme 'material t)))

;; (use-package gruvbox-theme
;;   :ensure t
;;   :config
;;   (load-theme 'gruvbox t))

;; easy copying between system clipboard and Emacs kill ring
(use-package osx-clipboard
  :ensure t
  :diminish osx-clipboard-mode
  :demand t
  :config
  (osx-clipboard-mode +1))

;; restart emacs from emacs
(use-package restart-emacs
  :ensure t
  :bind* (("C-x C" . restart-emacs)))

;; neotree for folder tree
(use-package neotree
  :ensure t
  :init
  (setq neo-theme 'arrow)
  :bind* (("C-c n" . neotree-projectile-action)))
(ryo-modal-key "SPC n" 'neotree-projectile-action)

;;;;;;;;;;;;;;;;;;;
;;    Writing    ;;
;;;;;;;;;;;;;;;;;;;

;; Org mode configuration
(require 'sk-org)
(ryo-modal-key "g o" 'org-capture)
(ryo-modal-key "g a" 'org-agenda)

;; markdown and latex for writing prose
(require 'sk-writing)

;;;;;;;;;;;;;;;;;;;;;;;
;;    Programming    ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; better tabs and spaces convention
(use-package editorconfig
  :ensure t
  :demand t
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

;; shell interaction
(require 'sk-shell)
;; (ryo-modal-key "g $" 'sk/call-terminal)
(ryo-modal-key "g Z" 'sk/zoom-tmux)

;; realgud - improved debugger
(use-package realgud
  :ensure t
  :commands (realgud:gdb
			 realgud:ipdb
			 realgud:pdb))

;;; Languages

;; Emacs lisp
(require 'sk-elisp)

;; Python family of languages
(require 'sk-python)

;; Matlab
(require 'sk-matlab)

;; Julia, R, S and SAS support
(require 'sk-stats)

;; C family languages
(require 'sk-c)
(require 'sk-cpp)
(require 'sk-cuda)

;; Web
(require 'sk-js)
(require 'sk-web)

;;; more language/library support

;; YAML mode
(use-package yaml-mode
  :ensure t
  :mode ("\\.yml$" "\\.yaml$"))

;; TOML mode
(use-package toml-mode
  :ensure t
  :mode ("\\.tml$" "\\.toml$"))

;; csv mode
(use-package csv-nav
  :ensure t
  :mode ("\\.csv$" . csv-nav-mode)
  :config
  (sk/enable-ryo-modal-mode))

;; VimL syntax
(use-package vimrc-mode
  :ensure t
  :mode (("\\.vimrc\\'"	. vimrc-mode)
		 ("\\.vim\\'"	. vimrc-mode)
		 ("\\vimrc"     . vimrc-mode))
  :commands (vimrc-mode))

;;; sys admin type packages

;; start services
(use-package prodigy
  :ensure t
  :bind* (("C-c g" . prodigy))
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
(ryo-modal-key "SPC z p" 'prodigy)

;; puppet support
(use-package puppet-mode
  :ensure t
  :mode (("\\.pp\\'" . puppet-mode))
  :config
  (ryo-modal-key "m v" 'puppet-validate :mode 'puppet-mode)
  (ryo-modal-key "m e" 'puppet-lint :mode 'puppet-mode)
  (ryo-modal-key "m a" 'puppet-align-block :mode 'puppet-mode))

(which-key-add-major-mode-key-based-replacements 'puppet-mode
  "m v" "validate"
  "m e" "errors"
  "m a" "align")

;; docker support
;; https://github.com/Silex/docker.el

;;;;;;;;;;;;;;;
;;    Fun    ;;
;;;;;;;;;;;;;;;

;; visually helpful settings/packages
(require 'sk-visual)

;; google stuff
(use-package google-this
  :ensure t
  :config
  :bind* (("C-c \\" . sk/google-this)))
(ryo-modal-key "g /" 'google-this)

;; creepy function
(defun hello-human ()
  "The scratch function for fun"
  (interactive)
  (message (concat "I know who you are, " user-full-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Round up bindings    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; default bindings
(require 'sk-ryo-bindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Local configuration    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; isolate the clipboard from emacs
(setq-default interprogram-cut-function nil
			  interprogram-paste-function nil
			  select-enable-clipboard nil)

;; load the local configuration if it exists
(when (file-exists-p (concat user-emacs-directory "local.el"))
  (load-file (concat user-emacs-directory "local.el")))

;;;;;;;;;;;;;;;;;;;;;;;;
;;    Start server    ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package server
  :config
  (unless (server-running-p)
	(setq server-name "default")
	(server-start)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Reduce GC threshold    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1 MB now
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 1 1024 1024))))

;;; The end

;; TODO

;; Sublimity
;; Replace region/line with yank ring
