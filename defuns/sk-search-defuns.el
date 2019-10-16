;; occur at point
(defun sk/occur-at-point ()
  "occur with the thing at point"
  (interactive)
  (occur (thing-at-point 'symbol)))

;; search TODOs in project
(defun sk/search-todos-in-project ()
  "search for the string TODO in a project"
  (interactive)
  (counsel-ag "TODO"))

;; search FIXMEs in project
(defun sk/search-fixmes-in-project ()
  "search for the string FIXME in a project"
  (interactive)
  (counsel-ag "FIXME"))

;; provide these functions
(provide 'sk-search-defuns)
