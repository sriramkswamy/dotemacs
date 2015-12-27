;; Correct those DOuble capitals
(defun dcaps-to-scaps ()
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
(define-minor-mode dubcaps-mode
  "Toggle `dubcaps-mode'.  Converts words in DOuble CApitals to
Single Capitals as you type."
  :init-value nil
  :lighter (" DC")
  (if dubcaps-mode
      (add-hook 'post-self-insert-hook #'dcaps-to-scaps nil 'local)
    (remove-hook 'post-self-insert-hook #'dcaps-to-scaps 'local)))
(defun sk/diminish-dubcaps ()
  (interactive)
  (diminish 'dubcaps-mode ""))
(add-hook 'dubcaps-mode-hook 'sk/diminish-dubcaps)
(add-hook 'org-mode-hook #'dubcaps-mode)

;; Flyspell
(defun flyspell-goto-previous-error (arg)
  "Go to arg previous spelling error."
  (interactive "p")
  (while (not (= 0 arg))
    (let ((pos (point))
          (min (point-min)))
      (if (and (eq (current-buffer) flyspell-old-buffer-error)
               (eq pos flyspell-old-pos-error))
          (progn
            (if (= flyspell-old-pos-error min)
                ;; goto beginning of buffer
                (progn
                  (message "Restarting from end of buffer")
                  (goto-char (point-max)))
              (backward-word 1))
            (setq pos (point))))
      ;; seek the next error
      (while (and (> pos min)
                  (let ((ovs (overlays-at pos))
                        (r '()))
                    (while (and (not r) (consp ovs))
                      (if (flyspell-overlay-p (car ovs))
                          (setq r t)
                        (setq ovs (cdr ovs))))
                    (not r)))
        (backward-word 1)
        (setq pos (point)))
      ;; save the current location for next invocation
      (setq arg (1- arg))
      (setq flyspell-old-pos-error pos)
      (setq flyspell-old-buffer-error (current-buffer))
      (goto-char pos)
      (if (= pos min)
          (progn
            (message "No more miss-spelled word!")
            (setq arg 0))
        (forward-word)))))

;; Spell check
(add-hook 'text-mode-hook 'flyspell-mode)
(defun sk/diminish-flyspell ()
  (interactive)
  (diminish 'flyspell-mode ""))
(add-hook 'flyspell-mode-hook 'sk/diminish-flyspell)
(defun sk/diminish-aspell ()
  (interactive)
  (diminish 'aspell-mode ""))
(add-hook 'aspell-mode-hook 'sk/diminish-aspell)
(defun sk/diminish-ispell ()
  (interactive)
  (diminish 'ispell-mode ""))
(add-hook 'ispell-mode-hook 'sk/diminish-ispell)

;; Markdown
(sk/require-package 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Pandoc mode
(sk/require-package 'pandoc-mode)
(add-hook 'markdown-mode-hook 'pandoc-mode)

;; LaTeX-mode
(sk/require-package 'auctex)
(sk/require-package 'auctex-latexmk)

;; Support auctex
(sk/require-package 'company-auctex)

(provide 'sk-writing)

;;; sk-writing.el ends here
