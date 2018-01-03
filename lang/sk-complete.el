;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Completion framework    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package company
  :ensure t
  :demand t
  :hook ((prog-mode-hook . global-company-mode)
		 (text-mode-hook . global-company-mode)
		 (org-mode-hook . global-company-mode)
		 (matlab-mode-hook . global-company-mode)
		 (markdown-mode-hook . global-company-mode))
  :init
  (setq company-minimum-prefix-length 0
		company-require-match 0
		company-selection-wrap-around t
		company-tooltip-limit 10
		company-idle-delay 0.05)
  (setq company-dabbrev-ignore-buffers "\\.pdf\\'"
		company-dabbrev-downcase nil
		company-dabbrev-code-modes t
		company-dabbrev-code-other-buffers 'all
		company-dabbrev-other-buffers 'all
		company-dabbrev-code-everywhere t)

  :bind* (("C-d"		. company-complete)
		  ("C-j C-f"	. company-files)
		  ("C-j C-s"	. company-ispell)
		  ("C-j C-c"	. company-clang)
		  ("C-j C-e"	. company-elisp)
		  ("C-j C-a"	. company-dabbrev))
  :bind (:map company-active-map
			  ("C-n"    . company-select-next)
			  ("C-p"    . company-select-previous)
			  ([return] . company-complete-selection)
			  ([tab]    . yas-expand)
			  ("TAB"    . yas-expand)
			  ("C-w"    . backward-kill-word)
			  ("C-c"    . company-abort)
			  ("C-c"    . company-search-abort))
  :diminish (company-mode . " ς")

  :config
  ;; set default backends
  (setq company-backends
		'((;; Generic backends
		   company-files          ; files & directory
           company-keywords       ; keywords
		   company-dabbrev-code   ; code abbrev
		   company-elisp          ; emacs-lisp code
		   ;; company-etags          ; etags
		   ;; company-gtags          ; gtags
		   company-semantic       ; semantic
		   ;; company-bbdb           ; bbdb
		   company-eclim          ; eclim
           company-capf)))
  (global-company-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Completion suggestions    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; LSP support
(use-package company-lsp
  :ensure t
  :after (company)
  :bind* (("C-j C-l"	. company-lsp)))

;; install ycmd first - https://github.com/Valloric/ycmd
;; cd ycmd, git submodule update --init --recursive and ./build.py --system-libclang --all

(use-package ycmd
  :ensure t
  :diminish ycmd-mode
  :init
  ;; FIXME: set based on location
  (cond
   ((eq system-type 'darwin)
	(progn
	  (set-variable 'ycmd-server-command
					'("python" "/Users/sriramkswamy/.emacs.d/ycmd/ycmd"))
	  (set-variable 'ycmd-extra-conf-whitelist
					"/Users/sriramkswamy/.emacs.d/.ycm_extra_conf.py")
	  (set-variable 'ycmd-global-config
					"/Users/sriramkswamy/.emacs.d/.ycm_extra_conf.py")))
   ((eq system-type 'gnu/linux)
	(progn
	  (set-variable 'ycmd-server-command
					'("python" "/Users/sriramkswamy/.emacs.d/ycmd/ycmd"))
	  (set-variable 'ycmd-extra-conf-whitelist
					"/Users/sriramkswamy/.emacs.d/.ycm_extra_conf.py")
	  (set-variable 'ycmd-global-config
					"/Users/sriramkswamy/.emacs.d/.ycm_extra_conf.py"))))
  (setq ycmd-force-semantic-completion t)
  :hook ((c++-mode . ycmd-mode)
		 (c-mode . ycmd-mode)
		 (python-mode . ycmd-mode)
		 (org-mode . ycmd-mode)
		 (markdown-mode . ycmd-mode)
		 (ycmd-mode . ycmd-eldoc-setup))

  :commands
  (ycmd-goto
   ycmd-open
   ycmd-close
   ycmd-fixit
   ycmd-setup
   ycmd-version
   ycmd-completer
   ycmd-get-type
   ycmd-goto-type
   ycmd-get-parent
   ycmd-eldoc-setup
   ycmd-goto-include
   ycmd-parse-buffer
   ycmd-goto-imprecise
   ycmd-goto-references
   ycmd-refactor-rename
   ycmd-goto-implementation
   ycmd-load-conf-file
   ycmd-toggle-log-enabled
   ycmd-restart-semantic-server
   ycmd-display-completions
   ycmd-goto-definition
   ycmd-goto-declaration
   ycmd-show-documentation
   ycmd-clear-compilation-flag-cache
   ycmd-toggle-force-semantic-completion
   ycmd-show-debug-info
   ycmd-mode
   ycmd-mode-menu
   ycmd-view-mode
   ycmd-eldoc-mode
   ycmd-fixit-mode))

(use-package company-ycmd
  :ensure t
  :after (ycmd)
  :hook ((ycmd-mode . company-ycmd-setup)))

(use-package flycheck-ycmd
  :ensure t
  :after (ycmd)
  :hook ((ycmd-mode . flycheck-ycmd-setup)))

;; provide the config
(provide 'sk-complete)
