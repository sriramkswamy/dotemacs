;;; sk-ess-hydra-modalka.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Modal hydras bindings for ESS

;;; Code:

;; Make hydra
(modalka-define-kbd "m s" "C-c h m s")

;; Code jump hydra
(modalka-define-kbd "c s" "C-c h c s")

;; aux requirements
(require 'sk-ess-hydra-modalka-which-key)

(provide 'sk-ess-hydra-modalka)
;;; sk-ess-hydra-modalka.el ends here
