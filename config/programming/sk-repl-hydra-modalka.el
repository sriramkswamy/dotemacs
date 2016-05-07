;;; sk-repl-hydra-modalka.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; For REPL modal hydra bindings

;;; Code:

;; Emamux
(modalka-define-kbd "m t" "C-c h t")

;; aux requirements
(require 'sk-repl-hydra-modalka-which-key)

(provide 'sk-repl-hydra-modalka)
;;; sk-repl-hydra-modalka.el ends here
