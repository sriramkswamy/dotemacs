;;; sk-navigation-hydra-modalka.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Modalka mode bindings for hydras

;;; Code:

;; Window
(modalka-define-kbd "W" "C-c h w")

;; Bookmark
(modalka-define-kbd "`" "C-c h b")

;; which key for modalka bindings
(require 'sk-navigation-hydra-modalka-which-key)

(provide 'sk-navigation-hydra-modalka)
;;; sk-navigation-hydra-modalka.el ends here
