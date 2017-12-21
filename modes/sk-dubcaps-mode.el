;; Correct the DOuble capitals
(defun sk/dcaps-to-scaps ()
  "Convert word in DOuble CApitals to Single Capitals."
  (interactive)
  (and (= ?w (char-syntax (char-before)))
       (save-excursion
         (and (if (called-interactively-p)
                  (skip-syntax-backward "w")
                (= -3 (skip-syntax-backward "w")))
              (let (case-fold-search)
                (looking-at "\\b[[:upper:]]\\{2\\}[[:lower:]]"))
              (capitalize-word 1)))))
(define-minor-mode sk/dubcaps-mode
  "Toggle `sk/dubcaps-mode'.  Converts words in DOuble CApitals to
Single Capitals as you type."
  :init-value nil
  (if sk/dubcaps-mode
      (add-hook 'post-self-insert-hook #'sk/dcaps-to-scaps nil 'local)
    (remove-hook 'post-self-insert-hook #'sk/dcaps-to-scaps 'local)))
(add-hook 'text-mode-hook #'sk/dubcaps-mode)

;; provide the minor mode
(provide 'sk-dubcaps-mode)
