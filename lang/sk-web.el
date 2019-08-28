;; web mode for many languages
(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
		 ("\\.phtml\\'" . web-mode)
		 ("\\.tpl\\.php\\'" . web-mode)
		 ("\\.[agj]sp\\'" . web-mode)
		 ("\\.as[cp]x\\'" . web-mode)
		 ("\\.erb\\'" . web-mode)
		 ("\\.mustache\\'" . web-mode)
		 ("\\.djhtml\\'" . web-mode)
		 ;; ("\\.js[x]\\'" . web-mode)
		 ("\\.[s]css\\'" . web-mode))
  :hook ((web-mode . sk/company-web)))

;; js
(use-package js
  :ensure t
  :mode ("\\.js[x]\\'" . web-mode)
  :hook ((js-mode . sk/company-js)))

;; provide this configuration
(provide 'sk-web)
