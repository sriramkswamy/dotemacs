;; flyspell mode
(use-package flyspell
  :diminish (flyspell-mode . " φ"))

;; eshell
(use-package eshell
  :bind* (("C-c u" . sk/eshell))
  :init
  (setq eshell-glob-case-insensitive t
		eshell-scroll-to-bottom-on-input 'this
		eshell-buffer-shorthand t
		eshell-history-size 1024
		eshell-cmpl-ignore-case t
		eshell-prompt-function (lambda () (concat " $ "))
		eshell-aliases-file (concat user-emacs-directory ".eshell-aliases")
		eshell-last-dir-ring-size 512)
  :config
  (add-hook 'shell-mode-hook 'goto-address-mode)
  ;; eshell in a vertical split
  (defun sk/eshell ()
	"eshell vertical split"
	(interactive)
	(split-window-horizontally)
	(eshell)))

;; diminish auto-revert mode
(defun sk/diminish-auto-revert ()
  (interactive)
  (diminish 'auto-revert-mode ""))
(add-hook 'auto-revert-mode-hook 'sk/diminish-auto-revert)

;; diminish eldoc mode
(defun sk/diminish-eldoc ()
  (interactive)
  (diminish 'eldoc-mode ""))
(add-hook 'eldoc-mode-hook 'sk/diminish-eldoc)

;; provide this
(provide 'sk-included)
