;;; sk-cpp-hydra-bindings.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Hydras bindings for C++

;;; Code:

;; Make hydra
(global-set-key (kbd "C-c h m c") 'sk/hydra-for-cpp/body)

;; code jump
(global-set-key (kbd "C-c h c c") 'sk/hydra-for-cpp-jump/body)

;; aux requirements
(require 'sk-cpp-hydra-modalka)

(provide 'sk-cpp-hydra-bindings)
;;; sk-cpp-hydra-bindings.el ends here
