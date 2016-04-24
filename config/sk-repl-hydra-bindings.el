;;; sk-repl-hydra-bindings.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; For REPL modal hydra bindings

;;; Code:

;; Emamux
(global-set-key (kbd "C-c h t") 'sk/hydra-for-emamux/body)

;; aux requirements
(require 'sk-repl-hydra-modalka)

(provide 'sk-repl-hydra-bindings)
;;; sk-repl-hydra-bindings.el ends here
