;;; sk-python-hydra-bindings.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Hydras bindings for python

;;; Code:

;; Make hydra
(global-set-key (kbd "C-c h m p") 'sk/hydra-for-py/body)

;; code jump
(global-set-key (kbd "C-c h c p") 'sk/hydra-for-py-jump/body)

;; aux requirements
(require 'sk-python-hydra-modalka)

(provide 'sk-python-hydra-bindings)
;;; sk-python-hydra-bindings.el ends here
