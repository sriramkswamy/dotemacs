;;; sk-defaults-bindings.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Some global bindings

;;; Code:

;; Backward kill word - used mostly while writing
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-S-w") 'kill-region)
;; Backward kill line - so used to this in the readline, so C-u is used up
(global-set-key (kbd "C-c u") 'universal-argument)
;; Minibuffer too
(define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word)

;; Unbound important stuff
(global-set-key (kbd "C-c c") 'load-theme)
(global-set-key (kbd "C-S-d") 'kill-whole-line)
(global-set-key (kbd "C-c v g m") 'make-frame)
(global-set-key (kbd "C-c v g k") 'delete-frame)
(global-set-key (kbd "C-c v g j") 'select-frame-by-name)
(global-set-key (kbd "C-c v g M") 'set-frame-name)
(global-set-key (kbd "C-c v =") 'indent-region)
(global-set-key (kbd "C-c v g f") 'find-file-at-point)
(global-set-key (kbd "C-c v g u") 'downcase-region)
(global-set-key (kbd "C-c v g U") 'upcase-region)
(global-set-key (kbd "C-c v g T") 'capitalize-region)
(global-set-key (kbd "C-c v R") 'overwrite-mode)
(global-set-key (kbd "C-c v C-v") 'rectangle-mark-mode)
(global-set-key (kbd "C-c v g q") 'fill-paragraph)
(global-set-key (kbd "C-c v C-o") 'pop-to-mark-command)
(global-set-key (kbd "C-c v C-t") 'pop-tag-mark)
(global-set-key (kbd "C-c v C-S-o") 'pop-global-mark)

(provide 'sk-defaults-bindings)
;;; sk-defaults-bindings.el ends here
