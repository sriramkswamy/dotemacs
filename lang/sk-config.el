;; JSON configuration
(use-package json-mode
  :ensure t
  :mode (("\\.json\\'" . json-mode)))

;; XML configuration
(use-package nxml-mode
  :mode (("\\.xml\\'" . nxml-mode)
		 ("\\.launch\\'" . nxml-mode)))

;; YAML configuration
(use-package yaml-mode
  :ensure t
  :mode (("\\.yaml\\'" . yaml-mode)
		 ("\\.yml\\'" . yaml-mode)))

;; HAML configuration
(use-package haml-mode
  :ensure t
  :mode (("\\.haml\\'" . yaml-mode)
		 ("\\.hml\\'" . yaml-mode)))

;; VIMRC configuration
(use-package vimrc-mode
  :ensure t
  :mode (("\\.vimrc\\'" . vimrc-mode)
		 ("\\.vim\\'" . vimrc-mode)))

;; provide the file
(provide 'sk-config)
