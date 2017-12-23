;; vim style char navigation
(use-package iy-go-to-char
  :ensure t
  :init
  (setq iy-go-to-char-continue-when-repeating nil)
  :commands
  (iy-go-to-char
   iy-go-to-char-backward
   iy-go-up-to-char
   iy-go-up-to-char-backward
   iy-go-to-or-up-to-continue
   iy-go-to-or-up-to-continue-backward))

;; wrapper functions based on iy-go-to-char
(defun sk/mark-to-char ()
  "mark to char using iy-go-to-char"
  (interactive)
  (set-mark (point))
  (call-interactively #'iy-go-to-char))
(defun sk/mark-to-char-backward ()
  "mark to char using iy-go-to-char-backward"
  (interactive)
  (push-mark (point))
  (call-interactively #'iy-go-to-char-backward))
(defun sk/mark-up-to-char ()
  "mark to char using iy-up-go-to-char"
  (interactive)
  (push-mark (point))
  (call-interactively #'iy-go-up-to-char))
(defun sk/mark-up-to-char-backward ()
  "mark to char using iy-up-go-to-char-backward"
  (interactive)
  (push-mark (point))
  (call-interactively #'iy-go-up-to-char-backward))
(defun sk/mark-continue ()
  "mark to char using iy-up-go-to-char-backward"
  (interactive)
  (push-mark (point))
  (iy-go-to-or-up-to-continue 1))
(defun sk/mark-continue-backward ()
  "mark to char using iy-up-go-to-char-backward"
  (interactive)
  (push-mark (point))
  (iy-go-to-or-up-to-continue-backward 1))

;; simulating mouse click
(use-package avy
  :ensure t
  :commands
  (avy-goto-char-2
   avy-goto-word-1
   avy-goto-char-2-above
   avy-goto-char-2-below
   avy-goto-char-in-line
   avy-goto-char-timer
   avy-goto-line)
  :init
  (setq avy-keys-alist
		`((avy-goto-char-2			. (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))
		  (avy-goto-word-1			. (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))
		  (avy-goto-char-in-line	. (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))
		  (avy-goto-char-2-above	. (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))
		  (avy-goto-char-2-below	. (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))
		  (avy-goto-char-timer  	. (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))
		  (avy-goto-line			. (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))))
  ;; (setq avy-style 'pre)
  (setq avy-background t)
  (defface avy-lead-face-0
	'((t (:foreground "white" :background "color-21")))
	"Face used for first non-terminating leading chars."))

;; wrapper around avy
(defun sk/mark-avy-char-timer ()
  "mark to char using avy-char-timer"
  (interactive)
  (push-mark (point))
  (call-interactively #'avy-goto-char-timer))
(defun sk/mark-avy-goto-line ()
  "mark to char using avy-goto-line"
  (interactive)
  (push-mark (point))
  (call-interactively #'avy-goto-line))

;; jump to windows quickly
(use-package ace-window
  :ensure t
  :bind* (("C-x o" . ace-window)))

;; jump and open links fast
(use-package ace-link
  :ensure t
  :after (avy ace-window)
  :commands
  (ace-link-org)
  :config
  (ace-link-setup-default))

;; moving across changes
(use-package goto-chg
  :ensure t
  :commands (goto-last-change
			 goto-last-change-reverse))

;; smart beginning and end in buffers
(use-package beginend
  :ensure t
  :hook ((dired-mode . beginend-global-mode)
         (text-mode . beginend-global-mode)
         (prog-mode . beginend-global-mode))
  :diminish ((beginend-global-mode . "")
             (beginend-prog-mode . ""))
  :config
  (beginend-global-mode))

;; moving across marks
(use-package back-button
  :ensure t
  :commands
  (back-button-local-backward
   back-button-local-forward
   back-button-global-backward
   back-button-global-forward))

;; dash documentation
(use-package dash-at-point
  :ensure t
  :commands
  (dash-at-point-with-docset))

;; dumb jumping
(use-package dumb-jump
  :ensure t
  :ensure-system-package (ag)
  :commands
  (dumb-jump-go
   dumb-jump-goto-file-line)
  :config
  (dumb-jump-mode))

;; folding
(use-package vimish-fold
  :ensure t
  :commands
  (vimish-fold-toggle
   vimish-fold
   vimish-fold-unfold
   vimish-fold-delete
   vimish-fold-refold)
  :config
  (vimish-fold-global-mode 1))

;; provide navigation related packages
(provide 'sk-nav)
