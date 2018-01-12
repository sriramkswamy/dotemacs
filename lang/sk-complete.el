;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Completion framework    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package company
  :ensure t
  :demand t
  :hook ((prog-mode-hook . global-company-mode)
		 (text-mode-hook . global-company-mode)
		 (prog-mode-hook . sk/company-prog-fallback)
		 (text-mode-hook . sk/company-text-fallback)
		 (org-mode-hook . global-company-mode)
		 (matlab-mode-hook . global-company-mode)
		 (markdown-mode-hook . global-company-mode))
  :init
  (setq company-minimum-prefix-length 1
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

  :bind* (("C-t"	. company-complete)
		  ("C-j f"	. company-files)
		  ("C-j s"	. company-ispell)
		  ("C-j e"	. company-elisp)
		  ("C-j y"	. company-yasnippet)
		  ("C-j c"	. company-dabbrev-code)
		  ("C-j d"	. company-dabbrev))
  :bind (:map prog-mode-map
			  ("C-d" . company-dabbrev-code))
  :bind (:map text-mode-map
			  ("C-d" . company-dabbrev))
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
  ;; set default backends and list all possible backends
  (setq company-backends
		'((;; generic backends
		   company-files          ; files & directory
           company-keywords       ; keywords
		   company-dabbrev-code   ; code words
		   company-dabbrev        ; words
		   ;; code backends
		   ;; company-elisp          ; emacs-lisp code
		   ;; company-semantic       ; semantic
		   ;; company-shell       ; shell
		   ;; company-eclim          ; eclim
		   ;; company-clang          ; clang
		   ;; company-rtags          ; rtags
		   ;; company-ycmd           ; ycmd
		   ;; company-matlab         ; matlab
		   ;; company-matlab-shell   ; matlab-shell
		   ;; company-anaconda       ; anaconda
           ;; tag backends
		   ;; company-etags          ; etags
		   ;; company-gtags          ; gtags
		   ;; company-bbdb           ; bbdb
		   ;; completion at point
		   company-capf)))
  (global-company-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Language completions    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; prog fallback
(defun sk/company-prog-fallback ()
  "Add backends for default completions in programming mode"
  (interactive)
  (require 'company)
  (setq company-backends
		'((;; list of backends
		   company-files          ; files & directory
           company-yasnippet      ; snippets
           company-keywords       ; keywords
		   company-dabbrev-code   ; code words
		   company-capf))))

;; text fallback
(defun sk/company-text-fallback ()
  "Add backends for default completion in text modes"
  (interactive)
  (require 'company)
  (setq company-backends
		'((;; list of backends
		   company-files          ; files & directory
           company-yasnippet      ; snippets
		   company-dabbrev        ; words
		   company-ispell         ; spelling
		   company-capf))))

;; elisp
(defun sk/company-elisp ()
  "Add backends for elisp completion in company mode"
  (interactive)
  (require 'company)
  (setq company-backends
		'((;; list of backends
		   company-files          ; files & directory
           company-yasnippet      ; snippets
		   company-keywords       ; keywords
		   company-dabbrev-code   ; code words
		   company-elisp          ; emacs-lisp code
		   company-capf))))

;; matlab
(defun sk/company-matlab ()
  "Add backends for matlab completion in company mode"
  (interactive)
  (require 'company)
  (setq company-backends
		'((;; list of backends
		   company-files          ; files & directory
           company-yasnippet      ; snippets
           company-keywords       ; keywords
		   company-dabbrev-code   ; code words
		   company-capf))))

;; shell
(defun sk/company-shell ()
  "Add backends for shell completion in company mode"
  (interactive)
  (require 'company)
  (setq company-backends
		'((;; list of backends
		   company-files          ; files & directory
           company-yasnippet      ; snippets
           ;; company-keywords       ; keywords
		   ;; company-dabbrev-code   ; code words
		   company-shell          ; matlab shell commands
		   company-capf))))

;; matlab shell
(defun sk/company-matlab-shell ()
  "Add backends for matlab-shell completion in company mode"
  (interactive)
  (require 'company)
  (setq company-backends
		'((;; list of backends
		   company-files          ; files & directory
           company-yasnippet      ; snippets
           ;; company-keywords       ; keywords
		   ;; company-dabbrev-code   ; code words
		   company-matlab-shell   ; matlab shell commands
		   company-capf))))

;; auctex
(defun sk/company-auctex ()
  "Add backends for latex completion"
  (interactive)
  (require 'company)
  (setq company-backends
		'((;; list of backends
		   company-files			   ; files & directory
           company-yasnippet		   ; snippets
		   company-dabbrev			   ; words
		   company-ispell			   ; spelling
		   company-auctex-labels	   ; latex labels
		   company-auctex-macros	   ; latex macros
		   company-auctex-symbols	   ; latex symbols
		   company-auctex-environments ; latex environments
		   company-auctex-bibs		   ; latex bibs
		   company-capf))))

;; python
(defun sk/company-python ()
  "Add backends for python completion in company mode"
  (interactive)
  (require 'company)
  (setq company-backends
		'((;; list of backends
		   ;; company-files          ; files & directory
           company-yasnippet      ; snippets
           ;; company-keywords       ; keywords
		   company-dabbrev-code   ; code words
		   ;; company-ycmd           ; python ycmd completion
		   ;; company-lsp            ; python lsp completion
		   company-anaconda       ; python anaconda completion
		   company-capf))))

;; clang
(defun sk/company-clang ()
  "Add backends for clang completion in company mode"
  (interactive)
  (require 'company)
  (setq company-backends
		'((;; list of backends
		   ;; company-files          ; files & directory
           company-yasnippet      ; snippets
           ;; company-keywords       ; keywords
		   ;; company-dabbrev-code   ; code words
		   ;; company-ycmd           ; python ycmd completion
		   ;; company-lsp            ; python lsp completion
		   company-rtags          ; clang rtags completion
		   ;; company-irony          ; clang irony completion
		   company-capf))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Completion suggestions    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; LSP support
(use-package company-lsp
  :ensure t
  :after (company)
  :bind* (("C-j l"	. company-lsp)))

;; install ycmd first - https://github.com/Valloric/ycmd
;; cd ycmd, git submodule update --init --recursive and ./build.py --system-libclang --all

(use-package ycmd
  :ensure t
  :diminish ycmd-mode
  :init
  ;; FIXME: set based on location
  (set-variable 'ycmd-server-command
				(list "python" (file-truename (concat user-emacs-directory "ycmd/ycmd"))))
  ;; (set-variable 'ycmd-extra-conf-whitelist
  ;; 					(file-truename (concat user-emacs-directory "ycmd/.ycm_extra_conf.py")))
  (set-variable 'ycmd-global-config
				(file-truename (concat user-emacs-directory "ycmd/.ycm_extra_conf.py")))
  (setq ycmd-force-semantic-completion t)

  ;; :hook ((c++-mode . ycmd-mode)
  ;; 		 (c-mode . ycmd-mode)
  ;; 		 (python-mode . ycmd-mode)
  ;; 		 (org-mode . ycmd-mode)
  ;; 		 (markdown-mode . ycmd-mode)
  ;; 		 (ycmd-mode . ycmd-eldoc-setup))

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
