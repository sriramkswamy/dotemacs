;;; sk-modal.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; My custom maps for modal editing based on Modalka and Hydras which follows a
;; Vi-style editing

;;; Code:

;; Amazing sticky bindings - sort of modal editing
(use-package hydra
  :ensure t)

;; True modal editing
(use-package modalka
  :ensure t
  :diminish (modalka-mode . "μ")
  :init
  (setq modalka-cursor-type 'box)
  :config
  (global-set-key (kbd "<escape>") #'modalka-mode)
  (which-key-add-key-based-replacements
    "C-c h" "hydra prefix"
    "C-c v" "vi prefix"
    "C-c v g" "vi global prefix"
    "C-c v [" "vi previous prefix"
    "C-c v ]" "vi next prefix"))

;; aux requirements
(require 'sk-modal-modalka)

(provide 'sk-modal)
;;; sk-modal.el ends here
