;;; sk-writing-bindings.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Writing bindings

;;; Code:

;; Olivetti
(global-set-key (kbd "C-c v g D") 'olivetti-mode)

;; PDF Tools
(global-set-key (kbd "C-S-n") 'sk/pdf-tools-pdf-next)
(global-set-key (kbd "C-S-p") 'sk/pdf-tools-pdf-previous)

;; Modal bindings
(require 'sk-writing-modalka)

(provide 'sk-writing-bindings)
;;; sk-writing-bindings.el ends here
