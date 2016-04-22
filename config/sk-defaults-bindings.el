;;; sk-defaults-bindings.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Some global bindings

;;; Code:

(global-set-key (kbd "C-c c") 'load-theme)
(global-set-key (kbd "C-S-d") 'kill-whole-line)
(global-set-key (kbd "C-c v g m") 'make-frame)
(global-set-key (kbd "C-c v g f") 'find-file-at-point)
(global-set-key (kbd "C-c v g u") 'downcase-region)
(global-set-key (kbd "C-c v g U") 'upcase-region)
(global-set-key (kbd "C-c v g T") 'upcase-initials-region)
(global-set-key (kbd "C-c v R") 'overwrite-mode)
(global-set-key (kbd "C-c v C-v") 'rectangle-mark-mode)

(provide 'sk-defaults-bindings)
;;; sk-defaults-bindings.el ends here
