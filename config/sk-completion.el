;; Flx with company
(sk/require-package 'flx)
(sk/require-package 'company-flx)

;; Company
(sk/require-package 'company)
;; (with-eval-after-load 'company
;;   (company-flx-mode +1))
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-files))
(setq company-idle-delay 0
      company-minimum-prefix-length 1
      company-require-match 0
      company-selection-wrap-around t
      company-dabbrev-downcase nil)

;; Maps
(global-set-key [(control return)] 'company-complete-common-or-cycle)
(defun sk/company-hook ()
  (interactive)
  (diminish 'company-mode " ς")
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map [tab] 'company-complete-common-or-cycle))
(add-hook 'company-mode-hook 'sk/company-hook)
(add-hook 'prog-mode-hook 'company-mode)

;; Company C headers
(sk/require-package 'company-c-headers)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-c-headers))

(provide 'sk-completion)

;;; sk-completion.el ends here
