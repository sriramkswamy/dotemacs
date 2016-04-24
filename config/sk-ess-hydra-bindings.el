;;; sk-ess-hydra-bindings.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Hydras bindings for C++

;;; Code:

;; Make hydra
(global-set-key (kbd "C-c h m s") 'sk/hydra-for-ess/body)

;; code jump
(global-set-key (kbd "C-c h c s") 'sk/hydra-for-py-jump/body)

;; aux requirements
(require 'sk-ess-hydra-modalka)

(provide 'sk-ess-hydra-bindings)
;;; sk-ess-hydra-bindings.el ends here
