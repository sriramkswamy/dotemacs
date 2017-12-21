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

;; how to interpret the command key and the option key on a Mac
(when (eq system-type 'darwin)
  ;; (setq mac-command-key-is-meta nil)
  (setq mac-command-modifier 'meta)
  ;; (setq mac-option-key-is-meta t)
  ;; (setq mac-option-modifier 'meta)
  )

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Reduce GC threshold    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1 MB now
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 1 1024 1024))))

