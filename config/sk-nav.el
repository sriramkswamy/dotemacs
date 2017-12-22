;; simulating mouse click
(use-package avy
  :ensure t
  :commands
  (avy-goto-char-2
   avy-goto-word-1
   avy-goto-char-2-above
   avy-goto-char-2-below
   avy-goto-char-in-line
   avy-goto-line)
  :init
  (setq avy-keys-alist
		`((avy-goto-char-2			. (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))
		  (avy-goto-word-1			. (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))
		  (avy-goto-char-in-line	. (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))
		  (avy-goto-char-2-above	. (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))
		  (avy-goto-char-2-below	. (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))
		  (avy-goto-line			. (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))))
  ;; (setq avy-style 'pre)
  (setq avy-background t)
  (defface avy-lead-face-0
	'((t (:foreground "white" :background "color-21")))
	"Face used for first non-terminating leading chars."))

;; jump to windows quickly
(use-package ace-window
  :ensure t
  :after (avy)
  :commands
  (ace-window))

;; jump and open links fast
(use-package ace-link
  :ensure t
  :after (avy)
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
  :ensure-system-package ("ag")
  :commands
  (dumb-jump-go
   dumb-jump-goto-file-line)
  :config
  (dumb-jump-mode))

;; provide navigation related packages
(provide 'sk-nav)
