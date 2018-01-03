;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Completion framework    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package company
  :ensure t
  :hook ((prog-mode-hook . company-mode)
		 (org-mode-hook . company-mode)
		 (LaTeX-mode-hook . company-mode)
		 (markdown-mode-hook . company-mode))
  :init
  (setq company-minimum-prefix-length 1
		company-require-match 0
		company-selection-wrap-around t
		company-tooltip-limit 20                       ; bigger popup window
		company-tooltip-align-annotations 't           ; align annotations to the right tooltip border
		company-idle-delay .2                          ; decrease delay before autocompletion popup shows
		company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
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
		   ;; company-etags          ; etags
		   ;; company-gtags          ; gtags
		   company-semantic       ; semantic
		   ;; company-bbdb           ; bbdb
		   company-eclim          ; eclim
           company-capf)))
  (global-company-mode))

;; LSP support
(use-package company-lsp
  :ensure t
  :after (company)
  :bind* (("C-j C-l"	. company-lsp)))

;;; some language specific functions to add to hooks later

;; elisp
(defun sk/company-elisp ()
  "Add backends for elisp completion in company mode"
  (interactive)
  (require 'company)
  (push 'company-elisp company-backends))
;; push company backend
(add-hook 'elisp-mode-hook 'sk/company-elisp)
(add-hook 'lisp-interaction-mode-hook 'sk/company-elisp)

;; C
(defun sk/company-c ()
  "Add backends for C completion in company mode"
  (interactive)
  (require 'company)
  ;; (push 'company-ycmd company-backends)
  ;; (push 'company-clang company-backends)
  (push 'company-rtags company-backends))
;; push company backend
;; (add-hook 'c-mode-hook 'sk/company-c)

;; C++
(defun sk/company-cpp ()
  "Add backends for C++ completion in company mode"
  (interactive)
  (require 'company)
  ;; (push 'company-ycmd company-backends)
  ;; (push 'company-clang company-backends)
  (push 'company-rtags company-backends))
;; push company backend
;; (add-hook 'c++-mode-hook 'sk/company-cpp)

;; Python
(defun sk/company-python ()
  "Add backends for python completion in company mode"
  (interactive)
  (require 'company)
  (push 'company-lsp company-backends)
  (push 'company-anaconda company-backends))
;; push company backend
(add-hook 'python-mode-hook 'sk/company-python)

;; Cython
(defun sk/company-cython ()
  "Add backends for cython completion in company mode"
  (interactive)
  (require 'company)
  (push 'company-lsp company-backends)
  (push 'company-anaconda company-backends))
;; push company backend
(add-hook 'cython-mode-hook 'sk/company-cython)

;; ESS
(defun sk/company-ess ()
  "Add backends for ESS completion in company mode"
  (interactive)
  (require 'company)
  (push 'company-ess company-backends))
;; push company backend
(add-hook 'ess-mode-hook 'sk/company-ess)

;; Org mode
(defun sk/company-org ()
  "Add backends for Org completion in company mode"
  (interactive)
  (require 'company)
  ;; (push 'company-ispell company-backends)
  (push 'company-dabbrev company-backends))
;; push company backend
(add-hook 'org-mode-hook 'sk/company-org)

;; Markdown mode
(defun sk/company-markdown ()
  "Add backends for Markdown completion in company mode"
  (interactive)
  (require 'company)
  ;; (push 'company-ispell company-backends)
  (push 'company-dabbrev company-backends))
;; push company backend
(add-hook 'markdown-mode-hook 'sk/company-markdown)

;; LaTeX mode
(defun sk/company-latex ()
  "Add backends for LaTeX completion in company mode"
  (interactive)
  (require 'company)
  ;; (push 'company-auctex company-backends)
  ;; (push 'company-ispell company-backends)
  (push 'company-dabbrev company-backends))
;; push company backend
(add-hook 'latex-mode-hook 'sk/company-latex)

;; web mode
(defun sk/company-web ()
  "Add backends for web completion in company mode"
  (interactive)
  (require 'company)
  (push 'company-nxml company-backends)
  (push 'company-css-html-tags company-backends)
  (push 'company-css company-backends))
;; push company backend
(add-hook 'web-mode-hook 'sk/company-web)

;; shell mode
(defun sk/company-shell ()
  "Add backends for shell completion in company mode"
  (interactive)
  (require 'company)
  (push 'company-shell company-backends))
;; push company backend
(add-hook 'shell-mode-hook 'sk/company-shell)
(add-hook 'term-mode-hook 'sk/company-shell)
(add-hook 'eshell-mode-hook 'sk/company-shell)

;; matlab shell mode
(defun sk/company-matlab-shell ()
  "Add backends for shell completion in company mode"
  (interactive)
  (require 'company)
  (push 'company-matlab-shell company-backends))
(if (fboundp 'matlab-shell-mode)
	(add-hook 'matlab-shell-mode-hook 'sk/company-matlab-shell))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Completion suggestions    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; install ycmd first - https://github.com/Valloric/ycmd

;; provide the config
(provide 'sk-complete)
