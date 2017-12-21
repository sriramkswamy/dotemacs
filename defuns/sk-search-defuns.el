;; occur at point
(defun sk/occur-at-point ()
  "occur with the thing at point"
  (interactive)
  (occur (thing-at-point 'symbol)))

;; provide these functions
(provide 'sk-search-defuns)
