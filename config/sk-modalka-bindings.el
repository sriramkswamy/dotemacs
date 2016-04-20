;;; sk-modalka-bindings.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Global bindings for modalka mode

;;; Code:

(global-set-key (kbd "<escape>") #'modalka-mode)

;; Which key explanations for bindings
(require 'sk-modalka-which-key)

(provide 'sk-modalka-bindings)
;;; sk-modalka-bindings.el ends here
