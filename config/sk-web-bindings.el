;;; sk-web-bindings.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Web stuff bindings

;;; Code:

;; Emmet expand
(global-set-key (kbd "C-c v C-y") 'emmet-next-edit-point)
(global-set-key (kbd "C-c v C-S-y") 'emmet-prev-edit-point)

;; aux requirements
(require 'sk-web-modalka)

(provide 'sk-web-bindings)
;;; sk-web-bindings.el ends here
