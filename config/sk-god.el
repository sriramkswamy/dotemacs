;; Cursor type change
(setq-default cursor-type 'bar)

;; God mode
(sk/require-package 'god-mode)
(require 'god-mode)
(global-set-key (kbd "<escape>") 'god-local-mode)
(global-set-key (kbd "<escape>") 'god-mode-all)
(setq god-exempt-major-modes nil
      god-exempt-predicates nil)
(defun sk/update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'box
                      'bar)))
(defun sk/god-binding-mode () (interactive)
       (if god-local-mode (god-mode-all)
         (setq god-mod-alist '((nil . "C-x C-z C-")))
         (god-mode-all)))
(add-hook 'god-mode-enabled-hook 'sk/update-cursor)
(add-hook 'god-mode-disabled-hook 'sk/update-cursor)
(define-key god-local-mode-map (kbd ".") 'repeat)
(define-key god-local-mode-map (kbd "i") 'god-local-mode)
(add-to-list 'god-exempt-major-modes 'paradox-menu-mode)
(add-to-list 'god-exempt-major-modes 'magit-status)
(add-to-list 'god-exempt-major-modes 'help-mode)
(add-to-list 'god-exempt-major-modes 'dired-mode)

(provide 'sk-god)

;;; sk-god.el ends here
