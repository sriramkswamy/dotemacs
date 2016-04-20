;;; sk-navigation-hydra-modalka.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Modalka mode bindings for hydras

;;; Code:

(modalka-define-kbd "W" "C-c h w")
(modalka-define-kbd "+" "C-c h b")

;; which key for modalka bindings
(require 'sk-navigation-hydra-modalka-which-key)

(provide 'sk-navigation-hydra-modalka)
;;; sk-navigation-hydra-modalka.el ends here
