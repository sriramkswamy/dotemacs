;;; sk-python-hydra-modalka.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Modal ydras bindings for C++

;;; Code:

;; Make hydra
(modalka-define-kbd "m p" "C-c h m p")

;; Code jump hydra
(modalka-define-kbd "c p" "C-c h c p")

;; aux requirements
(require 'sk-python-hydra-modalka-which-key)

(provide 'sk-python-hydra-modalka)
;;; sk-python-hydra-modalka.el ends here
