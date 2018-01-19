;; add gud mode
(defun sk/gud-mode ()
  "add gud mode"
  (interactive)
  ;; (require 'gdb)
  (require 'realgud)
  (require 'gud))

;; gud set breakpoint
(defun sk/gud-break ()
  "add gud break point"
  (interactive)
  (require 'gud)
  (sk/breakpoint-icon-set)
  (call-interactively #'gud-break))

;; gud remove breakpoint
(defun sk/gud-remove ()
  "remove gud break point"
  (interactive)
  (require 'gud)
  (sk/breakpoint-icon-remove)
  (call-interactively #'gud-remove))

;; gud step next
(defun sk/gud-next ()
  "step next"
  (interactive)
  (require 'gud)
  (call-interactively #'gud-next))

;; gud step
(defun sk/gud-step ()
  "step"
  (interactive)
  (require 'gud)
  (call-interactively #'gud-step))

;; gud up
(defun sk/gud-up ()
  "gud up"
  (interactive)
  (require 'gud)
  (call-interactively #'gud-up))

;; gud down
(defun sk/gud-down ()
  "gud down"
  (interactive)
  (require 'gud)
  (call-interactively #'gud-down))

;; gud finish
(defun sk/gud-finish ()
  "gud down"
  (interactive)
  (require 'gud)
  (call-interactively #'gud-finish))

;; gud cont
(defun sk/gud-cont ()
  "gud cont"
  (interactive)
  (require 'gud)
  (call-interactively #'gud-cont))

;; gud print
(defun sk/gud-print ()
  "gud print"
  (interactive)
  (require 'gud)
  (call-interactively #'gud-print))

;; gud refresh
(defun sk/gud-refresh ()
  "gud refresh"
  (interactive)
  (require 'gud)
  (call-interactively #'gud-refresh))

;; gud find-c-expr
(defun sk/gud-find-c-expr ()
  "gud find-c-expr"
  (interactive)
  (require 'gud)
  (call-interactively #'gud-find-c-expr))

;; provide debugging wrapper functions
(provide 'sk-debug-defuns)
