;;; sk-modal.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; My custom maps for modal editing based on Modalka and Hydras. This follows a
;; Vi-style editing

;;; Code:

;; Amazing sticky bindings - sort of modal editing
(sk/require-package 'hydra)

;; True modal editing
(sk/require-package 'modalka)
(setq modalka-cursor-type 'box)

;; aux requirements
(require 'sk-modal-diminish)
(require 'sk-modal-bindings)
(require 'sk-modal-which-key)
(require 'sk-modal-modalka)
(require 'sk-modal-modalka-which-key)

(provide 'sk-modal)
;;; sk-modal.el ends here
