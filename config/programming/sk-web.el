;;; sk-web.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Web stuff

;;; Code:

;; Web mode
(use-package web-mode
  :ensure t
  :mode ("\\.html$" . web-mode))

;; JavaScript
(use-package js3-mode
  :ensure t
  :mode ("\\.js$" . js3-mode))

;; JS navigation
(use-package tern
  :ensure t
  :diminish tern-mode
  :commands (tern-mode
	     tern-find-definition
	     tern-find-definition-by-name
	     tern-get-docs
	     tern-get-type
	     tern-use-server
	     tern-highlight-refs
	     tern-rename-variable))

;; Node js repl
(use-package nodejs-repl
  :ensure t
  :commands (nodejs-repl
	     nodejs-repl-send-buffer
	     nodejs-repl-switch-to-repl
	     nodejs-repl-send-region
	     nodejs-repl-send-last-sexp
	     nodejs-repl-execute
	     nodejs-repl-load-file))

;; JSON
(use-package json-mode
  :ensure t
  :mode "\\.json$")

;; Get JSON path
(use-package json-snatcher
  :ensure t
  :commands (jsons-print-path))

;; Coffeescript support
(use-package coffee-mode
  :ensure t
  :mode "\\.coffee$")

;; Nginx mode
(use-package nginx-mode
  :ensure t
  :commands (nginx-mode))

;; Beautify
(use-package web-beautify
  :ensure t
  :commands (web-beautify-css
	     web-beautify-css-buffer
	     web-beautify-html
	     web-beautify-html-buffer
	     web-beautify-js
	     web-beautify-js-buffer))

;; SCSS mode
(use-package scss-mode
  :ensure t
  :mode "\\.scss$")

;; Fast HTML/CSS
(use-package emmet-mode
  :ensure t
  :diminish (emmet-mode . "ε")
  :bind (
	 ("C-c v C-y" . emmet-next-edit-point)
	 ("C-c v C-S-y" . emmet-prev-edit-point)
	 )
  :commands (emmet-mode
	     emmet-next-edit-point
	     emmet-prev-edit-point))

;; Immediate HTML rendering
(use-package impatient-mode
  :ensure t
  :diminish (impatient-mode . "ι")
  :commands (impatient-mode))

;; aux requirements
(require 'sk-web-modalka)
(require 'sk-web-hydra)

(provide 'sk-web)
;;; sk-web.el ends here
