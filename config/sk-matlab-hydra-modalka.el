;;; sk-matlab-hydra-modalka.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Modal bindings for matlab

;;; Code:

;; Make hydra
(modalka-define-kbd "m m" "C-c h m m")

;; aux requirements
(require 'sk-matlab-hydra-modalka-which-key)

(provide 'sk-matlab-hydra-modalka)
;;; sk-matlab-hydra-modalka.el ends here
