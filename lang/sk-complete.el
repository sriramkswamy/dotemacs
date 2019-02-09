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
		company-tooltip-limit 15
		company-idle-delay 0)
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

;; ;; use a child frame
;; (use-package company-childframe
;;   :ensure t
;;   :diminish company-childframe-mode
;;   :after company
;;   :config
;;   (company-childframe-mode 1)
;;   (require 'desktop)
;;   (push '(company-childframe-mode . nil)
;;       desktop-minor-mode-table))

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
		   ;; company-dabbrev			   ; words
		   ;; company-ispell			   ; spelling
		   company-auctex-labels	   ; latex labels
		   company-auctex-macros	   ; latex macros
		   company-auctex-symbols	   ; latex symbols
		   company-auctex-environments ; latex environments
		   company-auctex-bibs		   ; latex bibs
		   company-reftex-citations    ; reftex citations
		   company-reftex-labels       ; reftex labels
		   company-capf))))

;; python
(defun sk/company-python ()
  "Add backends for python completion in company mode"
  (interactive)
  (require 'company)
  (setq company-backends
		'((;; list of backends
		   company-files          ; files & directory
           company-yasnippet      ; snippets
           company-keywords       ; keywords
		   company-dabbrev-code   ; code words
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
		   company-files          ; files & directory
           company-yasnippet      ; snippets
           company-keywords       ; keywords
		   company-dabbrev-code   ; code words
		   ;; company-lsp            ; clangd lsp completion
		   company-rtags          ; clang rtags completion
		   ;; company-irony          ; clang irony completion
		   company-capf))))

;; provide the config
(provide 'sk-complete)
