;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Completion framework    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package company
  :ensure t
  :demand t
  :hook ((prog-mode-hook . global-company-mode)
		 (text-mode-hook . global-company-mode)
		 (text-mode-hook . sk/company-text)
		 (org-mode-hook . global-company-mode)
		 (matlab-mode-hook . global-company-mode)
		 (markdown-mode-hook . global-company-mode))
  :init
  (setq company-minimum-prefix-length 1
		company-require-match 0
		company-selection-wrap-around t
		company-tooltip-limit 15
		company-idle-delay 0.1)
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
  ;; set default backends
  (setq company-backends
		'(company-files          ; files & directory
          company-keywords       ; keywords
		  company-dabbrev-code   ; code words
		  company-yasnippet      ; snippets
		  ;; company-etags          ; etags
		  ;; company-gtags          ; gtags
		  ;; company-bbdb           ; bbdb
		  company-capf           ; complete at point
		  ))
  (global-company-mode))

;; use a child frame
(when (display-graphic-p)
  (use-package company-posframe
	:ensure t
	:diminish company-posframe-mode
	:after company
	:config
	(company-posframe-mode 1)
	(require 'desktop)
	(push '(company-posframe-mode . nil)
		  desktop-minor-mode-table)))

;; LSP completion support
(use-package company-lsp
  :ensure t
  :after (lsp-mode)
  :init
  (setq company-lsp-async t
		company-lsp-cache-candidates t)
  :bind* (("C-j l"	. company-lsp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Language completions    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; text
(defun sk/company-text ()
  "Add backends for default completion in text modes"
  (interactive)
  (require 'company)
  (add-to-list 'company-backends 'company-dabbrev))

;; elisp
(defun sk/company-elisp ()
  "Add backends for elisp completion in company mode"
  (interactive)
  (require 'company)
  (add-to-list 'company-backends 'company-elisp))

;; matlab
(defun sk/company-matlab ()
  "Add backends for matlab completion in company mode"
  (interactive)
  (require 'company)
  (add-to-list 'company-backends 'company-keywords))

;; shell
(defun sk/company-shell ()
  "Add backends for shell completion in company mode"
  (interactive)
  (require 'company)
  (add-to-list 'company-backends 'company-shell))

;; matlab shell
(defun sk/company-matlab-shell ()
  "Add backends for matlab-shell completion in company mode"
  (interactive)
  (require 'company)
  (add-to-list 'company-backends 'company-matlab-shell))

;; auctex
(defun sk/company-auctex ()
  "Add backends for latex completion"
  (interactive)
  (require 'company)
  (add-to-list 'company-backends 'company-auctex-bibs)
  (add-to-list 'company-backends 'company-auctex-labels)
  (add-to-list 'company-backends 'company-auctex-macros)
  (add-to-list 'company-backends 'company-auctex-symbols)
  (add-to-list 'company-backends 'company-auctex-citations)
  (add-to-list 'company-backends 'company-auctex-environments))

;; python
(defun sk/company-python ()
  "Add backends for python completion in company mode"
  (interactive)
  (require 'company)
  (add-to-list 'company-backends 'company-anaconda)
  (add-to-list 'company-backends 'company-lsp)
  )

;; clang
(defun sk/company-clang ()
  "Add backends for clang completion in company mode"
  (interactive)
  (require 'company)
  (add-to-list 'company-backends 'company-rtags)
  ;; (add-to-list 'company-backends 'company-irony)
  (add-to-list 'company-backends 'company-lsp)
  )

;; sql
(defun sk/company-sql ()
  "Add backends for sql completion in company mode"
  (interactive)
  (require 'company)
  (add-to-list 'company-backends 'company-keywords))

;; provide the config
(provide 'sk-complete)
