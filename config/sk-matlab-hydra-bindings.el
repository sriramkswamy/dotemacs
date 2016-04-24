;;; sk-matlab-hydra-bindings.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Hydras bindings for matlab

;;; Code:

;; Make hydra
(global-set-key (kbd "C-c h m m") 'sk/hydra-for-matlab/body)

;; aux requirements
(require 'sk-matlab-hydra-modalka)

(provide 'sk-matlab-hydra-bindings)
;;; sk-matlab-hydra-bindings.el ends here
