;;; sk-company.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Auto completion

;;; Code:

;; Company
(sk/require-package 'company)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-files))
(setq company-minimum-prefix-length 1
      company-require-match 0
      company-selection-wrap-around t
      company-dabbrev-downcase nil
      company-tooltip-limit 20                      ; bigger popup window
      company-tooltip-align-annotations 't          ; align annotations to the right tooltip border
      company-idle-delay .3                         ; decrease delay before autocompletion popup shows
      company-begin-commands '(self-insert-command) ; start autocompletion only after typing
)

;; Diminish
(defun sk/company-hook ()
  (interactive)
  (diminish 'company-mode " ς")
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map [return] 'company-complete-selection)
  )
(add-hook 'company-mode-hook 'sk/company-hook)
(add-hook 'prog-mode-hook 'company-mode)

;; Tooltips
(sk/require-package 'company-quickhelp)
(company-quickhelp-mode 1)

;; Company C headers
(sk/require-package 'company-c-headers)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-c-headers))

;; Company web
(sk/require-package 'company-web)

;; Company jedi
(sk/require-package 'company-jedi)
(defun sk/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))
(add-hook 'python-mode-hook 'sk/python-mode-hook)

;; Company anaconda
(sk/require-package 'company-anaconda)
(eval-after-load "company"
 '(add-to-list 'company-backends '(company-anaconda :with company-capf)))

;; Auctex
(sk/require-package 'company-auctex)

;; Tern
(sk/require-package 'company-tern)

;; Maps
(global-set-key (kbd "M-]") 'company-complete-common-or-cycle)

(provide 'sk-company)
;;; sk-company.el ends here
