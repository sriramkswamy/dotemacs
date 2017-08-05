;; better modeline
(use-package spaceline
  :ensure t
  :demand t
  :init
  (setq powerline-default-separator 'bar)
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)
  (spaceline-helm-mode))

;; fancy battery
(use-package fancy-battery
  :ensure t
  :commands (fancy-battery-mode)
  :init
  (setq fancy-battery-show-percentage t))

;; cleanup whitespace
(use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :config
  (ws-butler-global-mode))

;; setup pdf tools
(defun sk/setup-pdf-tools ()
  "setup bindings for pdf tools because use-package isn't able to load"
  (interactive)
  (bind-keys :map pdf-view-mode-map
			 ("w"   . ace-window)
			 ("o"   . pdf-outline)
			 ("S"   . image-save)
			 ("/"   . pdf-occur)
			 ("a a" . pdf-annot-attachment-dired)
             ("a s" . pdf-annot-add-squiggly-markup-annotation)
			 ("a u" . pdf-annot-add-underline-markup-annotation)
			 ("a h" . pdf-annot-add-highlight-markup-annotation)
			 ("a o" . pdf-annot-add-strikeout-markup-annotation)
			 ("a t" . pdf-annot-add-text-annotation)
			 ("a d" . pdf-annot-delete)
			 ("a l" . pdf-annot-list-annotations)
			 ("j"   . pdf-view-scroll-up-or-next-page)
			 ("k"   . pdf-view-scroll-down-or-previous-page)
			 ("g"   . pdf-view-first-page)
			 ("G"   . pdf-view-last-page)))

;; PDF tools for better pdf viewing
		   (use-package pdf-tools
			 :ensure t
			 :mode ("\\.pdf\\'" . pdf-view-mode)
			 :config
			 (unless (package-installed-p 'pdf-tools)
			   (pdf-tools-install))
			 (add-hook 'pdf-view-mode-hook 'sk/setup-pdf-tools)
			 (add-hook 'pdf-view-mode-hook 'pdf-annot-minor-mode)
			 (add-hook 'pdf-view-mode-hook 'pdf-view-fit-page-to-window))

;; rainbow paranthesis for easier viewing
(use-package rainbow-delimiters
  :ensure t
  :demand t
  :config
  (rainbow-delimiters-mode-enable))
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode-enable)
(add-hook 'text-mode-hook 'rainbow-delimiters-mode-enable)

;; warn me if I go over a character limit
(use-package column-enforce-mode
  :ensure t
  :diminish column-enforce-mode
  :init
  (setq column-enforce-column 99)
  :config
  (progn
	(add-hook 'prog-mode-hook 'column-enforce-mode)))

;; highlight indentation levels
(use-package indent-guide
  :ensure t
  :diminish indent-guide-mode
  :bind* (("C-c o i" . indent-guide-mode)))
(if (fboundp 'indent-guide-mode)
	(add-hook 'prog-mode-hook 'indent-guide-mode))


;; blink cursor when asked
(use-package beacon
  :ensure t
  :diminish beacon-mode
  :commands (beacon-mode
			 beacon-blink))

;; indicate margins
(use-package fill-column-indicator
  :ensure t
  :bind* (("C-c o m" . fci-mode))
  :init
  (setq fci-rule-width 5
		fci-rule-column 79)
  (setq fci-rule-color "#bebebe"))

;; ;; toggle line numbers
;; (use-package nlinum
;;   :ensure t
;;   :commands (nlinum-mode
;; 			 global-nlinum-mode)
;;   :bind* (("C-c o n" . nlinum-mode)))

;; visual regexp substitution
(use-package visual-regexp
  :ensure t
  :commands (vr/query-replace
			 vr/replace))
;; mapping for query replace
(ryo-modal-key "d v x" 'er/expand-region :then '(vr/query-replace))
(sk/ryo-operator-object vr-regexp "d v" "v" vr/query-replace)
(sk/ryo-vr-regexp-bindings)

;; which key hints
(which-key-add-key-based-replacements
  "d v" "replace"
  "d v v" "in line"

  ;; send
  "d v i" "inside"
  "d v a" "around"
  "d v g" "global"
  "d v i a" "all"
  "d v a a" "all"
  "d v i w" "word"
  "d v a w" "word"
  "d v i p" "para"
  "d v a p" "para"
  "d v i s" "sentence"
  "d v a s" "sentence"
  "d v i l" "line"
  "d v a l" "line"
  "d v i y" "symbol"
  "d v a y" "symbol"
  "d v i c" "comment"
  "d v a c" "comment"
  "d v i f" "function"
  "d v a f" "function"
  "d v i q" "quote"
  "d v a q" "quote"
  "d v i b" "block/pairs"
  "d v a b" "block/pairs"
  "d v i o" "org code"
  "d v a o" "org code"
  "d v i u" "org subtree"
  "d v a u" "org subtree"
  "d v i e" "latex env"
  "d v a e" "latex env"
  "d v i r" "method call"
  "d v a r" "method call"
  "d v i d" "ruby block"
  "d v a d" "ruby block"
  "d v i g" "python string"
  "d v a g" "python string"
  "d v i m" "python block"
  "d v a m" "python block"
  "d v i n" "python statement"
  "d v a n" "python block and dec"
  "d v i h" "diff hunk"
  "d v a h" "diff hunk"
  "d v i x" "latex section"
  "d v a x" "latex section"
  "d v i $" "latex math"
  "d v a $" "latex math"
  "d v f" "to char"
  "d v F" "to char back"
  "d v t" "till char"
  "d v T" "till char back"
  "d v ;" "find on screen"
  "d v g ;" "till line"
  "d v h" "prev char"
  "d v j" "next line"
  "d v k" "prev line"
  "d v l" "next char"
  "d v 0" "till start of line"
  "d v $" "till end of line"
  "d v {" "till start of para"
  "d v }" "till end of para"
  "d v (" "till start of sentence"
  "d v )" "till end of sentence"
  "d v e" "end of word"
  "d v b" "start of word"
  "d v g g" "start of buffer"
  "d v G" "end of buffer")

;; restrict text to certain places - in my case the center of the screen
(use-package visual-fill-column
  :ensure t
  :defer t
  :bind (("C-c o o" . visual-fill-column-mode))
  :config
  (setq-default visual-fill-column-center-text t
				visual-fill-column-width 100
				visual-fill-column-fringes-outside-margins nil))

;; folding
(use-package vimish-fold
  :ensure t
  :bind* (("C-c z z" . vimish-fold-toggle)
		  ("C-c z f" . vimish-fold)
		  ("C-c z u" . vimish-fold-unfold)
		  ("C-c z d" . vimish-fold-delete)
		  ("C-c z r" . vimish-fold-refold))
  :config
  (vimish-fold-global-mode 1))

;; mapping for folding
(ryo-modal-keys
 ("="
  (("d" vimish-fold-delete)
   ("r" vimish-fold-refold)
   ("u" vimish-fold-unfold)
   ("n" vimish-fold-next-fold)
   ("p" vimish-fold-previous-fold)
   ("=" vimish-fold-toggle)
   ("-" vimish-fold))))
(sk/ryo-operator-object fold "=" "c" vimish-fold t)
(sk/ryo-fold-bindings)

;; which key
(which-key-add-key-based-replacements
  "=" "fold"
  "= c" "close"
  "= d" "delete"
  "= r" "refold"
  "= u" "unfold"
  "= n" "next"
  "= p" "previous"
  "= =" "toggle"
  "= -" "fold"

  ;; send
  "= i" "inside"
  "= a" "around"
  "= g" "global"
  "= i a" "all"
  "= a a" "all"
  "= i w" "word"
  "= a w" "word"
  "= i p" "para"
  "= a p" "para"
  "= i s" "sentence"
  "= a s" "sentence"
  "= i l" "line"
  "= a l" "line"
  "= i y" "symbol"
  "= a y" "symbol"
  "= i c" "comment"
  "= a c" "comment"
  "= i f" "function"
  "= a f" "function"
  "= i q" "quote"
  "= a q" "quote"
  "= i b" "block/pairs"
  "= a b" "block/pairs"
  "= i o" "org code"
  "= a o" "org code"
  "= i u" "org subtree"
  "= a u" "org subtree"
  "= i e" "latex env"
  "= a e" "latex env"
  "= i r" "method call"
  "= a r" "method call"
  "= i d" "ruby block"
  "= a d" "ruby block"
  "= i g" "python string"
  "= a g" "python string"
  "= i m" "python block"
  "= a m" "python block"
  "= i n" "python statement"
  "= a n" "python block and dec"
  "= i h" "diff hunk"
  "= a h" "diff hunk"
  "= i x" "latex section"
  "= a x" "latex section"
  "= i $" "latex math"
  "= a $" "latex math"
  "= f" "to char"
  "= F" "to char back"
  "= t" "till char"
  "= T" "till char back"
  "= ;" "find on screen"
  "= g ;" "till line"
  "= h" "prev char"
  "= j" "next line"
  "= k" "prev line"
  "= l" "next char"
  "= 0" "till start of line"
  "= $" "till end of line"
  "= {" "till start of para"
  "= }" "till end of para"
  "= (" "till start of sentence"
  "= )" "till end of sentence"
  "= e" "end of word"
  "= b" "start of word"
  "= g g" "start of buffer"
  "= G" "end of buffer")

;; provide this configuration
(provide 'sk-visual)
