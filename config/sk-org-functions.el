;;; sk-org-functions.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Org mode specific functions

;;; Code:

;; Org load languages
(defun sk/org-custom-load ()
  (interactive)
  (require 'org-page)
  (require 'ox-reveal)
  (require 'org-ref)
  (require 'org-ref-latex)
  (require 'org-ref-pdf)
  (require 'org-ref-url-utils)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     ;; (dot . t)
     ;; (ditaa . t)
     (latex . t)
     ;; (gnuplot . t)
     ;; (sh . t)
     ;; (C . t)
     ;; (R . t)
     ;; (octave . t)
     (matlab . t)
     (python . t))))

(provide 'sk-org-functions)
;;; sk-org-functions.el ends here
