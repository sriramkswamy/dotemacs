;;; sk-navigation-hydra-bindings.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs bindings for hydras defined in sk-navigation-hydra.el

;;; Code:

(global-set-key (kbd "C-c h b") 'sk/hydra-bookmarks/body)
(global-set-key (kbd "C-c h w") 'sk/hydra-of-windows/body)

;; Modal bindings
(require 'sk-navigation-hydra-modalka)

(provide 'sk-navigation-hydra-bindings)
;;; sk-navigation-hydra-bindings.el ends here
