;;; sk-company.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Auto completion

;;; Code:

;; Functions
(defun sk/company-abort ()
  (interactive)
  (company-abort)
  (when (and (bound-and-true-p evil-mode)
             (eq evil-state 'insert))
    (evil-force-normal-state)))

;; Company
(use-package company
  :ensure t
  :commands (company-mode
	     company-complete
	     company-complete-common
	     company-complete-common-or-cycle
	     company-files
	     company-dabbrev
	     company-ispell
	     company-c-headers
	     company-jedi
	     company-tern
	     company-web-html
	     company-auctex)
  :chords (
	   ("JA" . company-dabbrev)
	   ("JD" . company-ispell)
	   ("JF" . company-files)
	   ("JJ" . company-jedi)
	   ("JS" . company-tern)
	   ("JT" . company-auctex)
	   ("JC" . company-c-headers)
	   ("JW" . company-web-html)
	   )
  :init
  (setq company-minimum-prefix-length 2
	company-require-match 0
	company-selection-wrap-around t
	company-dabbrev-downcase nil
	company-tooltip-limit 20                      ; bigger popup window
	company-tooltip-align-annotations 't          ; align annotations to the right tooltip border
	company-idle-delay .4                         ; decrease delay before autocompletion popup shows
	company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
  (eval-after-load 'company
    '(add-to-list 'company-backends '(
				      company-files
				      company-capf
				      )))
  :bind (
	 ("C-c a f" . company-files)
	 ("C-c a a" . company-dabbrev)
	 ("C-c a d" . company-ispell)
	 :map company-active-map
	      ("C-n" . company-select-next)
	      ("C-p" . company-select-previous)
	      ([return] . company-complete-selection)
	      ("C-w" . backward-kill-word)
	      ("C-c" . sk/company-abort)
	      ("C-c" . company-search-abort)
	      )
  :diminish (company-mode . "ς")
  :config
  (global-company-mode)
  ;; C++ header completion
  (use-package company-c-headers
    :ensure t
    :bind (
	   ("C-c a c" . company-c-headers)
	   )
    :config
    (add-to-list 'company-backends 'company-c-headers))
  ;; Python auto completion
  (use-package company-jedi
    :ensure t
    :bind (
	   ("C-c a j" . company-jedi)
	   )
    :config
    (add-to-list 'company-backends 'company-jedi))
  ;; Tern for JS
  (use-package company-tern
    :ensure t
    :bind (
	   ("C-c a s" . company-tern)
	   )
    :init
    (setq company-tern-property-marker "")
    (setq company-tern-meta-as-single-line t)
    :config
    (add-to-list 'company-backends 'company-tern))
  ;; HTML completion
  (use-package company-web
    :ensure t
    :bind (
	   ("C-c a w" . company-web-html)
	   )
    :config
    (add-to-list 'company-backends 'company-web-html))
  ;; LaTeX autocompletion
  (use-package company-auctex
    :ensure t
    :bind (
	   ("C-c a t" . company-auctex)
	   )
    :config
    (add-to-list 'company-backends 'company-auctex))
  )

(provide 'sk-company)
;;; sk-company.el ends here
