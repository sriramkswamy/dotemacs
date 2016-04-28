;;; sk-company.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Auto completion

;;; Code:

;; Company
(sk/require-package 'company)
(setq company-minimum-prefix-length 1
      company-require-match 0
      company-selection-wrap-around t
      company-dabbrev-downcase nil
      company-tooltip-limit 20                      ; bigger popup window
      company-tooltip-align-annotations 't          ; align annotations to the right tooltip border
      company-idle-delay .2                         ; decrease delay before autocompletion popup shows
      company-begin-commands '(self-insert-command) ; start autocompletion only after typing
)
(eval-after-load 'company
  '(add-to-list 'company-backends '(
				    company-files
				    company-capf
				    )))


;; Maps
(defun sk/company-abort ()
  (interactive)
  (company-abort)
  (when (and (bound-and-true-p evil-mode)
             (eq evil-state 'insert))
    (evil-force-normal-state)))
(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map [return] 'company-complete-selection)
  (define-key company-active-map (kbd "C-w") 'backward-kill-word)
  (define-key company-active-map (kbd "<escape>") 'sk/company-abort)
  (define-key company-search-map (kbd "<escape>") 'company-search-abort))

;; Diminish
(defun sk/company-hook ()
  (interactive)
  (diminish 'company-mode " ς"))
(add-hook 'company-mode-hook 'sk/company-hook)
(add-hook 'prog-mode-hook 'company-mode)

;; Tooltips
(sk/require-package 'company-quickhelp)
(company-quickhelp-mode 1)

;; Company C headers
(sk/require-package 'company-c-headers)
(defun sk/c-mode-hook ()
  (add-to-list 'company-backends 'company-c-headers))
(add-hook 'c-mode-hook 'sk/c-mode-hook)

;; Company web
(sk/require-package 'company-web)

;; Company anaconda
(sk/require-package 'company-anaconda)

;; Company completion for python
(sk/require-package 'company-jedi)
(defun sk/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))
(add-hook 'python-mode-hook 'sk/python-mode-hook)

;; Auctex
(sk/require-package 'company-auctex)

;; Tern
(sk/require-package 'company-tern)

;; Maps
(global-set-key (kbd "M-]") 'company-complete-common-or-cycle)

(provide 'sk-company)
;;; sk-company.el ends here
