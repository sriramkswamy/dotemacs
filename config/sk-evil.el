;; evil - vim emulation
(use-package evil				; extensible vi layer - evil
  :ensure t				        ; ensure the package is present
  :demand t
  :diminish undo-tree mode
  :bind* (:map evil-normal-state-map
	       ("\\" . nil))
  :bind* (:map evil-visual-state-map
	       ("\\" . nil))
  :init				    		; settings to be loaded before the package is loaded
  (setq evil-want-C-u-scroll t)		        ; scroll like Vim when in normal mode
  (setq evil-want-Y-yank-to-eol t)	        ; yank 'Y' like 'D' and 'C'
  (setq evil-move-beyond-eol t)                 ; go beyond the end of line - useful when evaluating functions
  :config				        ; settings after the package is loaded
  (use-package undo-tree
    :ensure t
    :diminish undo-tree-mode
    :general
    (general-nmap "U" 'undo-tree-visualize))
  (setq evil-normal-state-modes (append evil-emacs-state-modes evil-normal-state-modes))
  (setq evil-emacs-state-modes nil)	        ; let there be no emacs state modes. add them all to normal state
  (evil-mode 1))
;; remove conflicting maps
(general-nmap "gw" nil)
;; global leader maps
(general-nmap :prefix sk--evil-global-leader  ; create a global leader map
	      "" '(nil :which-key "user prefix")
	      "q" '(evil-quit :which-key "quit window/emacs")
	      "k" '(evil-delete-buffer :which-key "kill buffer")
	      "r" (general-simulate-keys "C-x b" t "switch buffers")
	      "f" (general-simulate-keys "C-x C-f" t "find files")
	      "j" (general-simulate-keys "M-x" t "command")
	      "y" (general-simulate-keys "M-y" t "yank ring")
	      "h" (general-simulate-keys "C-h" t "help")
	      "v" 'clone-indirect-buffer-other-window
	      "w" 'save-buffer)
;; maps under 'co' and 'c'
(general-nmap "c" (general-key-dispatch #'evil-change
		    "ow" #'toggle-word-wrap
		    "ov" #'visual-line-mode
		    "os" #'flyspell-mode
		    "ob" #'display-battery-mode
		    "ot" #'display-time-mode
		    "og" #'writegood-mode
		    "oo" #'sk/org-custom-load
		    "d" #'dired
		    "c" #'evil-change-whole-line))
(general-vmap "c" 'evil-change)
;; other remaps
(general-nvmap "'" (general-simulate-keys "C-c '" t "C-c '"))
(general-nvmap "`" (general-simulate-keys "C-c C-c" t "C-c C-c"))
(general-nvmap "\\" (general-simulate-keys "C-c C-k" t "C-c C-k"))
(general-nvmap "gn" 'narrow-to-region)
(general-nvmap "gN" 'narrow-to-defun)
(general-nvmap "gs" 'widen)
(general-nmap "go" (general-simulate-keys "C-x C-e" t "eval"))
(general-nmap "w" (general-simulate-keys "C-x o" t "other window"))
(general-nmap "gz" (general-simulate-keys "C-x 1" t "only window"))
(general-nmap "W" 'winner-undo)
(general-nmap "gW" 'winner-redo)
(general-nmap "gS" (general-simulate-keys "C-j" t "split line"))
(general-nmap "gJ" 'evil-join)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Evil editing integration    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; better commenting
(use-package evil-commentary
  :ensure t
  :diminish evil-commentary-mode
  :commands (evil-commentary
	     evil-commentary-line
	     evil-commentary-yank
	     evil-commentary-yank-line
	     evil-commentary/org-comment-or-uncomment-region)
  :general
  (general-nmap "gc" 'evil-commentary)
  (general-nmap "gy" 'evil-commentary-yank)
  :config
  (evil-commentary-mode))

;; '*' in visual mode
(use-package evil-visualstar
  :ensure t
  :general
  (general-vmap "*" 'evil-visualstar/begin-search-forward)
  (general-vmap "#" 'evil-visualstar/begin-search-backward))

;; exchange like in Vim
(use-package evil-exchange
  :ensure t
  :config
  (evil-exchange-cx-install))

;; argument text object
(use-package evil-args
  :ensure t
  :general
  (general-nmap "gr" 'evil-jump-out-args)
  (general-nvmap "[r" 'evil-backward-arg)
  (general-nvmap "]r" 'evil-forward-arg)
  (general-mmap "[r" 'evil-backward-arg)
  (general-mmap "]r" 'evil-forward-arg)
  (general-itomap "r" 'evil-inner-arg)
  (general-otomap "r" 'evil-outer-arg))

;; indent text object
(use-package evil-indent-plus
  :ensure t
  :general
  (general-itomap "i" 'evil-indent-plus-i-indent)
  (general-otomap "i" 'evil-indent-plus-a-indent)
  (general-itomap "I" 'evil-indent-plus-i-indent-up)
  (general-otomap "I" 'evil-indent-plus-a-indent-up)
  (general-itomap "j" 'evil-indent-plus-i-indent-up-down)
  (general-otomap "j" 'evil-indent-plus-a-indent-up-down))

;; column word text object
(use-package evil-textobj-column
  :ensure t
  :general
  (general-itomap "k" 'evil-textobj-column-word)
  (general-itomap "K" 'evil-textobj-column-WORD))

;; evil multiple cursors
(use-package evil-multiedit
  :ensure t
  :demand t
  :general
  (general-evil-define-key 'normal evil-multiedit-state-map
    "RET" 'evil-multiedit-toggle-or-restrict-region
    "M-n" 'evil-multiedit-next
    "M-p" 'evil-multiedit-prev)
  (general-evil-define-key 'normal evil-multiedit-insert-state-map
    "M-n" 'evil-multiedit-next
    "M-p" 'evil-multiedit-prev)
  (general-nvmap "C-n" 'evil-multiedit-match-and-next)
  (general-nvmap "C-p" 'evil-multiedit-match-and-prev)
  (general-nvmap "C-y" 'evil-multiedit-match-all)
  (general-nvmap "C-e" 'evil-multiedit-restore)
  (general-mmap "RET" 'evil-multiedit-toggle-or-restrict-region))

;; evil-smartparens - semantic navigation, especially for lisps
(use-package evil-smartparens
  :ensure t
  :demand t
  :diminish smartparens-mode
  :diminish smartparens-strict-mode
  :diminish (evil-smartparens-mode . " ()")
  :general
  (general-evil-define-key '(normal visual) smartparens-mode-map :prefix "S"
			   "j" 'sp-down-sexp
			   "k" 'sp-backward-up-sexp
			   "h" 'sp-backward-down-sexp
			   "l" 'sp-up-sexp
			   "f" 'sp-forward-sexp
			   "b" 'sp-backward-sexp
			   "a" 'sp-beginning-of-sexp
			   "e" 'sp-end-of-sexp
			   "n" 'sp-next-sexp
			   "p" 'sp-previous-sexp
			   ">" 'sp-forward-barf-sexp
			   "<" 'sp-backward-barf-sexp
			   ")" 'sp-forward-slurp-sexp
			   "(" 'sp-backward-slurp-sexp
			   "x" 'sp-transpose-sexp
			   "d" 'sp-kill-sexp
			   "y" 'sp-copy-sexp
			   "u" 'sp-unwrap-sexp
			   "U" 'sp-backward-unwrap-sexp
			   "C" 'sp-convolute-sexp
			   "r" 'sp-raise-sexp
			   "s" 'sp-split-sexp
			   "S" 'sp-splice-sexp
			   "F" 'sp-splice-sexp-killing-forward
			   "B" 'sp-splice-sexp-killing-backward
			   "A" 'sp-splice-sexp-killing-around)
  :config
  (defun evil-sp--add-bindings ()
    (when smartparens-strict-mode
      (evil-define-key 'normal evil-smartparens-mode-map
	(kbd "d") #'evil-sp-delete
	(kbd "c") #'evil-sp-change
	(kbd "y") #'evil-sp-yank
	(kbd "X") #'evil-sp-backward-delete-char
	(kbd "x") #'evil-sp-delete-char)
      (evil-define-key 'visual evil-smartparens-mode-map
	(kbd "X") #'evil-sp-delete
	(kbd "x") #'evil-sp-delete))
    (evil-define-key 'normal evil-smartparens-mode-map
      (kbd "D") #'evil-sp-delete-line
      (kbd "Y") #'evil-sp-yank-line
      (kbd "C") #'evil-sp-change-line)
    (evil-define-key 'insert evil-smartparens-mode-map
      (kbd "DEL") 'sp-backward-delete-char)
    (evil-define-key 'visual evil-smartparens-mode-map
      (kbd "o") #'evil-sp-override)
    (evil-normalize-keymaps))
  (require 'smartparens-config)
  (smartparens-global-mode)
  (smartparens-global-strict-mode)
  (show-smartparens-global-mode)
  (add-hook 'smartparens-mode-hook 'evil-smartparens-mode))

;; execute some god state commands if need be
(use-package evil-god-state
  :ensure t
  :general
  (general-nmap "\\" 'evil-execute-in-god-state)
  :config
  (add-hook 'evil-god-state-entry-hook (lambda () (diminish 'god-local-mode)))
  (add-hook 'evil-god-state-exit-hook (lambda () (diminish-undo 'god-local-mode)))
  (evil-define-key 'god global-map [escape] 'evil-god-state-bail))

;; provide the configuration
(provide 'sk-evil)
