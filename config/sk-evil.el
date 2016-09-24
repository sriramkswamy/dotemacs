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
  (setq evil-normal-state-modes (append evil-emacs-state-modes evil-motion-state-modes evil-normal-state-modes))
  (setq evil-emacs-state-modes nil)	        ; let there be no emacs state modes until I later add magit
  (setq evil-motion-state-modes nil)	        ; let there be no emacs state modes
  (evil-set-initial-state 'magit-popup-mode 'emacs)
  (evil-set-initial-state 'deft-mode 'insert)
  (evil-set-initial-state 'ag-mode 'normal)
  (evil-set-initial-state 'wgrep-mode 'normal)
  (evil-mode 1))
;; remove conflicting maps
(general-nvmap "m" nil)
(general-nvmap "Z" nil)
(general-nvmap "gw" nil)
;; global leader maps
(general-nvmap :prefix sk--evil-global-leader  ; create a global leader map
			   "q" '(evil-quit :which-key "quit window/emacs")
			   "k" (general-simulate-keys "C-x k" t "kill buffers")
			   "r" (general-simulate-keys "C-x b" t "switch buffers")
			   "f" (general-simulate-keys "C-x C-f" t "find files")
			   "j" (general-simulate-keys "M-x" t "command")
			   "J" (general-simulate-keys "M-X" t "major mode command")
			   "y" (general-simulate-keys "M-y" t "yank ring")
			   "h" (general-simulate-keys "C-h" t "help")
			   "w" (general-simulate-keys "C-x C-s" t "save buffers")
			   "v" '(clone-indirect-buffer-other-window :which-key "clone buffer")
			   "TAB" '(mode-line-other-buffer :which-key "previous buffer"))
;; local leader maps
(general-nvmap :prefix sk--evil-local-leader
			   "-" (general-simulate-keys "C-c C-k" t "abort")
			   "`" (general-simulate-keys "C-c C-g" t "big abort"))
;; other remaps
(general-nvmap "j" '(evil-next-visual-line :which-key "next line"))
(general-nvmap "k" '(evil-previous-visual-line :which-key "prev line"))
(general-nvmap "`" (general-simulate-keys "C-c C-c" t "C-c C-c"))
(general-nvmap "g>" '(evil-shift-right :which-key "indent"))
(general-nvmap "g<" '(evil-shift-left :which-key "dedent"))
(general-nvmap "g=" '(flyspell-mode :which-key "spellcheck"))
(general-nvmap "g+" '(flyspell-prog-mode :which-key "programming spellcheck"))
(general-nvmap "g-" '(visual-line-mode :which-key "soft wrap"))
(general-nvmap "gn" '(narrow-to-region :which-key "narrow to region"))
(general-nvmap "gN" '(narrow-to-defun :which-key "narrow to func"))
(general-nvmap "gs" '(widen :which-key "widen buffer"))
(general-nvmap "go" (general-simulate-keys "C-x C-e" t "eval"))
(general-nvmap "Z" (general-simulate-keys "C-x 1" t "only window"))
(general-nvmap "W" '(winner-undo :which-key "undo window confing"))
(general-nvmap "gW" '(winner-redo :which-key "redo window config"))
(general-nvmap "gS" (general-simulate-keys "C-j" t "split line"))
(general-nvmap "gJ" '(evil-join :which-key "join lines"))
(general-nvmap "H" '(scroll-right :which-key "scroll left"))
(general-nvmap "L" '(scroll-left :which-key "scroll right"))
(general-nvmap "g!" '(async-shell-command :which-key "async command"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Evil editing integration    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; better '%' command
(use-package evil-matchit
  :ensure t
  :demand t
  :config
  (global-evil-matchit-mode 1))

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
  (general-nvmap "gc" '(evil-commentary :which-key "comment"))
  (general-nvmap "gy" '(evil-commentary-yank :which-key "yank comment"))
  :config
  (evil-commentary-mode))

;; '*' in visual mode
(use-package evil-visualstar
  :ensure t
  :general
  (general-vmap "*" '(evil-visualstar/begin-search-forward :which-key "search for region")))

;; exchange like in Vim
(use-package evil-exchange
  :ensure t
  :config
  (evil-exchange-cx-install))

;; argument text object
(use-package evil-args
  :ensure t
  :general
  (general-nmap "gr" '(evil-jump-out-args :which-key "jump out of args"))
  (general-nvmap "[r" '(evil-backward-arg :which-key "backward argument"))
  (general-nvmap "]r" '(evil-forward-arg :which-key "forward argument"))
  (general-mmap "[r" '(evil-backward-arg :which-key "backward argument"))
  (general-mmap "]r" '(evil-forward-arg :which-key "forward argument"))
  (general-itomap "r" '(evil-inner-arg :which-key "argument"))
  (general-otomap "r" '(evil-outer-arg :which-key "argument")))

;; indent text object
(use-package evil-indent-plus
  :ensure t
  :general
  (general-itomap "i" '(evil-indent-plus-i-indent :which-key "indent"))
  (general-otomap "i" '(evil-indent-plus-a-indent :which-key "indent"))
  (general-itomap "I" '(evil-indent-plus-i-indent-up :which-key "indent + up"))
  (general-otomap "I" '(evil-indent-plus-a-indent-up :which-key "indent + up"))
  (general-itomap "j" '(evil-indent-plus-i-indent-up-down :which-key "indent + up-down"))
  (general-otomap "j" '(evil-indent-plus-a-indent-up-down :which-key "indent + up-down")))

;; column word text object
(use-package evil-textobj-column
  :ensure t
  :general
  (general-itomap "k" '(evil-textobj-column-word :which-key "column word"))
  (general-itomap "K" '(evil-textobj-column-WORD :which-key "column WORD")))

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
  (general-nvmap "C-n" '(evil-multiedit-match-and-next :which-key "multicursor next"))
  (general-nvmap "C-p" '(evil-multiedit-match-and-prev :which-key "multicursor prev"))
  (general-nvmap "C-y" '(evil-multiedit-match-all :which-key "multicursor all"))
  (general-nvmap "C-e" '(evil-multiedit-restore :which-key "multicursor restore")))

;; evil surround functionality
(use-package evil-surround
  :ensure t
  :demand t
  :general
  (general-nmap "s" '(evil-surround-region :which-key "surround"))
  (general-nmap "S" '(evil-Surround-region :which-key "surround in new line"))
  (general-omap "s" '(evil-surround-edit :which-key "surround"))
  (general-vmap "s" '(evil-surround-region :which-key "surround"))
  (general-omap "S" '(evil-Surround-edit :which-key "surround in new line"))
  (general-vmap "S" '(evil-Surround-region :which-key "surround in new line"))
  (general-evil-define-key '(normal) evil-surround-mode-map
	"s" '(evil-surround-region :which-key "surround"))
  (general-evil-define-key '(normal) evil-surround-mode-map
	"S" '(evil-Surround-region :which-key "surround in new line"))
  (general-evil-define-key '(visual) evil-surround-mode-map
	"s" '(evil-surround-region :which-key "surround"))
  (general-evil-define-key '(visual) evil-surround-mode-map
	"S" '(evil-Surround-region :which-key "surround in new line"))
  (general-evil-define-key '(operator) evil-surround-mode-map
	"s" '(evil-surround-edit :which-key "surround"))
  (general-evil-define-key '(operator) evil-surround-mode-map
	"S" '(evil-Surround-edit :which-key "surround in new line"))
  :config
  (global-evil-surround-mode 1))

;; evil-smartparens - semantic navigation, especially for lisps
(use-package evil-smartparens
  :ensure t
  :demand t
  :diminish smartparens-mode
  :diminish smartparens-strict-mode
  :diminish (evil-smartparens-mode . " ()")
  :general
  (general-evil-define-key '(normal visual) smartparens-mode-map :prefix "\""
			   "" '(nil :which-key "smartparens")
			   "j" '(sp-down-sexp :which-key "down sexp")
			   "k" '(sp-backward-up-sexp :which-key "back up sexp")
			   "h" '(sp-backward-down-sexp :which-key "back down sexp")
			   "l" '(sp-up-sexp :which-key "up sexp")
			   "f" '(sp-forward-sexp :which-key "forward sexp")
			   "b" '(sp-backward-sexp :which-key "backward sexp")
			   "a" '(sp-beginning-of-sexp :which-key "start of sexp")
			   "e" '(sp-end-of-sexp :which-key "end of sexp")
			   "n" '(sp-next-sexp :which-key "next sexp")
			   "p" '(sp-previous-sexp :which-key "previous sexp")
			   ">" '(sp-forward-barf-sexp :which-key "forward barf")
			   "<" '(sp-backward-barf-sexp :which-key "forward barf")
			   ")" '(sp-forward-slurp-sexp :which-key "forward slurp")
			   "(" '(sp-backward-slurp-sexp :which-key "backward slurp")
			   "x" '(sp-transpose-sexp :which-key "exchange sexp")
			   "d" '(sp-kill-sexp :which-key "delete sexp")
			   "y" '(sp-copy-sexp :which-key "copy sexp")
			   "u" '(sp-unwrap-sexp :which-key "unwrap sexp")
			   "U" '(sp-backward-unwrap-sexp :which-key "backward unwrap sexp")
			   "c" '(sp-convolute-sexp :which-key "convolute sexp")
			   "r" '(sp-raise-sexp :which-key "raise sexp")
			   "s" '(sp-split-sexp :which-key "split sexp")
			   "S" '(sp-splice-sexp :which-key "splice sexp")
			   "F" '(sp-splice-sexp-killing-forward :which-key "splice forward")
			   "B" '(sp-splice-sexp-killing-backward :which-key "splice back")
			   "A" '(sp-splice-sexp-killing-around :which-key "splice around"))
  :config
  ;; redefine function with the bindings I want
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
;; smartparens hydra
(defhydra hydra-smartparens (:color red :hint nil)
  "
 ^Move^              ^Edit^                                              ^Splice^
^^^^^^^^^^^^^--------------------------------------------------------------------------------------------------
 ^ ^ _k_ ^ ^    ^ ^ _p_ ^ ^    _<_: barf backward    _u_: unwrap       _x_: transpose  _S_: splice   _q_: quit
 _h_ ^+^ _l_    _b_ ^+^ _f_    _>_: barf forward     _U_: unwrap back  _c_: convolute  _F_: forward
 ^ ^ _j_ ^ ^    ^ ^ _n_ ^ ^    _)_: slurp forward    _d_: delete       _r_: raise      _B_: backward
 _a_: start _e_: end   _(_: slurp backward   _y_: copy         _s_: split      _A_: around
"
  ("h" sp-backward-down-sexp)
  ("l" sp-up-sexp)
  ("j" sp-down-sexp)
  ("k" sp-backward-up-sexp)
  ("b" sp-backward-sexp)
  ("f" sp-forward-sexp)
  ("n" sp-next-sexp)
  ("p" sp-previous-sexp)
  ("a" sp-beginning-of-sexp)
  ("e" sp-end-of-sexp)
  ("<" sp-backward-barf-sexp)
  (">" sp-forward-barf-sexp)
  ("(" sp-backward-slurp-sexp)
  (")" sp-forward-slurp-sexp)
  ("u" sp-unwrap-sexp)
  ("U" sp-backward-unwrap-sexp)
  ("d" sp-kill-sexp)
  ("y" sp-copy-sexp)
  ("x" sp-transpose-sexp)
  ("c" sp-convolute-sexp)
  ("r" sp-raise-sexp)
  ("s" sp-split-sexp)
  ("S" sp-splice-sexp)
  ("F" sp-splice-sexp-killing-forward)
  ("B" sp-splice-sexp-killing-backward)
  ("A" sp-splice-sexp-killing-around)
  ("q" nil :color blue))
(general-nvmap "'"
			   '(hydra-smartparens/body :which-key "smartparens"))

;; provide the configuration
(provide 'sk-evil)
