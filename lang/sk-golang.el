;; golang support
(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :hook ((go-mode . sk/company-golang)))

;; provide this config
(provide 'sk-golang)
