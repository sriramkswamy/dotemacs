;;; sk-writing-bindings.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Writing bindings

;;; Code:

;; Olivetti
(global-set-key (kbd "C-c v g D") 'olivetti-mode)

;; Modal bindings
(require 'sk-writing-modalka)

(provide 'sk-writing-bindings)
;;; sk-writing-bindings.el ends here
