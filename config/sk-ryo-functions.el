;; backward change word or region
(defun sk/change-region-or-backward-word ()
  (interactive)
  (sk/kill-region-or-backward-word)
  (ryo-modal-mode -1))

;; smartparens change current linke
(defun sk/sp-change-line ()
  (interactive)
  (move-beginning-of-line 1)
  (sp-kill-hybrid-sexp 1)
  (ryo-modal-mode -1))

;; smartparens kill current linke
(defun sk/sp-kill-line ()
  (interactive)
  (move-beginning-of-line 1)
  (sp-kill-hybrid-sexp 1)
  (sp-kill-hybrid-sexp 1))

;; upcase line
(defun sk/upcase-line ()
  (interactive)
  (sk/select-inside-line)
  (upcase-region))

;; downcase line
(defun sk/downcase-line ()
  (interactive)
  (sk/select-inside-line)
  (downcase-region))

;; capitalize line
(defun sk/capitalize-line ()
  (interactive)
  (sk/select-inside-line)
  (capitalize-region))

;; fill region
(defun sk/fill-region (beginning end)
  (interactive "r")
  (if (use-region-p)
	  (fill-region beginning end)))

;; unfill region
(defun sk/unfill-region (beginning end)
  (interactive "r")
  (if (use-region-p)
	  (unfill-region (beginning end))))

;; change region or line
(defun sk/change-region-or-line ()
  (interactive)
  (if (region-active-p)
	  (sk/change-region-or-backward-word)
	(sk/sp-change-line)))

;; change region or line
(defun sk/kill-region-or-line ()
  (interactive)
  (if (region-active-p)
	  (sk/kill-region-or-backward-word)
	(sk/sp-kill-line)))

;; upcase region or line
(defun sk/upcase-region-or-line ()
  (interactive)
  (if (region-active-p)
	  (upcase-region)
	(sk/upcase-line)))

;; downcase region or line
(defun sk/downcase-region-or-line ()
  (interactive)
  (if (region-active-p)
	  (downcase-region)
	(sk/downcase-line)))

;; capitalize region or line
(defun sk/capitalize-region-or-line ()
  (interactive)
  (if (region-active-p)
	  (capitalize-region)
	(sk/capitalize-line)))

;; iedit bookmark set
(defun sk/iedit-mark-set ()
  (interactive)
  (bookmark-set "iedit"))

;; iedit bookmark jump
(defun sk/iedit-mark-jump ()
  (interactive)
  (bookmark-jump "iedit"))

;; provide this configuration
(provide 'sk-ryo-functions)
