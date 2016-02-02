;; Cursor type change
(setq-default cursor-type 'bar)

;; God mode
(sk/require-package 'god-mode)
(require 'god-mode)
(global-set-key (kbd "<escape>") 'god-local-mode)
(global-set-key (kbd "<escape>") 'god-mode-all)
(setq god-exempt-major-modes nil
      god-exempt-predicates nil)
(defun sk/god-mode-update-cursor ()
  (let ((limited-colors-p (> 257 (length (defined-colors)))))
    (cond (god-local-mode (progn
                            (set-face-background 'mode-line (if limited-colors-p "white" "#e9e2cb"))
                            (set-face-background 'mode-line-inactive (if limited-colors-p "white" "#e9e2cb"))))
          (t (progn
               (set-face-background 'mode-line (if limited-colors-p "black" "#0a2832"))
               (set-face-background 'mode-line-inactive (if limited-colors-p "black" "#0a2832")))))))
(define-key god-local-mode-map (kbd ".") 'repeat)
(define-key god-local-mode-map (kbd "i") 'god-local-mode)
(add-to-list 'god-exempt-major-modes 'paradox-menu-mode)
(add-to-list 'god-exempt-major-modes 'magit-status)
(add-to-list 'god-exempt-major-modes 'help-mode)

;; Key chords
(sk/require-package 'key-chord)
(sk/require-package 'key-seq)
(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.5
      key-chord-one-key-delay 0.5)

(key-seq-define-global "//" 'counsel-locate)
(key-seq-define-global "==" 'indent-region)
(key-seq-define-global "[[" 'kmacro-start-macro)
(key-seq-define-global "]]" 'kmacro-end-macro)
(key-seq-define-global "`-" 'org-edit-src-code)
(key-seq-define-global "`." 'sk/hydra-of-macros/body)
(key-seq-define-global "`=" 'sk/hydra-of-langs/body)
(key-seq-define-global "``" 'sk/hydra-of-hydras/body)
(key-seq-define-global "`a" 'sk/hydra-of-activate/body)
(key-seq-define-global "`b" 'switch-to-buffer)
(key-seq-define-global "`c" 'sk/hydra-comments/body)
(key-seq-define-global "`d" 'avy-goto-char-in-line)
(key-seq-define-global "`e" 'sk/hydra-wgrep/body)
(key-seq-define-global "`f" 'find-file)
(key-seq-define-global "`g" 'sk/hydra-of-git/body)
(key-seq-define-global "`h" 'sk/hydra-of-help/body)
(key-seq-define-global "`i" 'sk/hydra-of-edits/body)
(key-seq-define-global "`j" 'sk/hydra-of-jump/body)
(key-seq-define-global "`k" 'kill-buffer)
(key-seq-define-global "`l" 'avy-goto-line)
(key-seq-define-global "`m" 'sk/hydra-of-motion/body)
(key-seq-define-global "`n" 'sk/hydra-of-navigation/body)
(key-seq-define-global "`o" 'sk/hydra-of-org/body)
(key-seq-define-global "`p" 'clipboard-yank)
(key-seq-define-global "`r" 'ivy-recentf)
(key-seq-define-global "`s" 'sk/hydra-of-search/body)
(key-seq-define-global "`t" 'sk/hydra-tags/body)
(key-seq-define-global "`u" 'sk/hydra-of-undo/body)
(key-seq-define-global "`v" 'sk/hydra-of-windows/body)
(key-seq-define-global "`w" 'save-buffer)
(key-seq-define-global "`x" 'counsel-M-x)
(key-seq-define-global "`y" 'clipboard-kill-ring-save)
(key-seq-define-global "`z" 'sk/hydra-origami/body)

(provide 'sk-interface)

;;; sk-interface.el ends here
