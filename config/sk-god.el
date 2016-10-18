;; wrapper functions for god mode
(defun sk/disable-god-mode ()
  "Disables god mode"
  (interactive)
  (god-local-mode -1))
(defun sk/enable-god-mode ()
  "Enable god mode"
  (interactive)
  (god-local-mode 1))

;; god mode installation
(use-package god-mode
  :ensure t
  :demand t
  :diminish (god-local-mode . " ψ")
  :bind (("<escape>" . sk/enable-god-mode)
		 :map god-local-mode-map
		 ("i" . sk/disable-god-mode)
		 ("z" . repeat))
  :config
  (add-to-list 'god-exempt-major-modes 'ag-mode)
  (add-to-list 'god-exempt-major-modes 'occur-mode)
  (add-to-list 'god-exempt-major-modes 'ivy-occur-mode)
  (add-to-list 'god-exempt-major-modes 'help-mode)
  (add-to-list 'god-exempt-major-modes 'view-mode)
  (add-to-list 'god-exempt-major-modes 'special-mode)
  (add-to-list 'god-exempt-major-modes 'deft-mode)
  (add-to-list 'god-exempt-major-modes 'package-menu-mode)
  (add-to-list 'god-exempt-major-modes 'debugger-mode)
  (add-to-list 'god-exempt-major-modes 'man-mode)
  (add-to-list 'god-exempt-major-modes 'info-mode)
  (add-to-list 'god-exempt-major-modes 'eww-mode)
  (add-to-list 'god-exempt-major-modes 'doc-view-mode)
  (add-to-list 'god-exempt-major-modes 'org-agenda-mode)
  ;; don't use this on overwrite mode
  (defun god-toggle-on-overwrite ()
  "Toggle god-mode on overwrite-mode."
  (if (bound-and-true-p overwrite-mode)
	  (god-local-mode-pause)
	(god-local-mode-resume)))
  (add-hook 'overwrite-mode-hook 'god-toggle-on-overwrite)
  ;; change cursor shape
  (defun sk/update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
						'box
					  'bar)))
  (add-hook 'god-mode-enabled-hook 'sk/update-cursor)
  (add-hook 'god-mode-disabled-hook 'sk/update-cursor)
  (god-mode))

;; multiple cursors
(use-package multiple-cursors
  :ensure t
  :commands (mc/edit-lines
			 mc/edit-ends-of-lines
			 mc/edit-beginnings-of-lines
			 mc/mark-more-like-this-extended
			 mc/mark-next-like-this
			 mc/mark-previous-like-this
			 mc/unmark-next-like-this
			 mc/unmark-previous-like-this
			 mc/skip-to-next-like-this
			 mc/skip-to-previous-like-this
			 mc/mark-sgml-tag-pair
			 mc/mark-all-like-this
			 mc/mark-all-in-region
			 mc/mark-all-in-region-regexp
			 mc/insert-letters
			 mc/insert-numbers
			 mc/vertical-align-with-space
			 mc/vertical-align
			 mc/sort-regions
			 mc/reverse-regions)
  :bind* (("C-," . mc/mark-all-like-this)
		  ("C->" . mc/mark-next-like-this)
		  ("C-<" . mc/mark-previous-like-this)
		  ("C-:" . mc/skip-to-previous-like-this)
		  ("C-\"". mc/skip-to-next-like-this)
		  ("C-}" . mc/edit-ends-of-lines)
		  ("C-{" . mc/edit-beginnings-of-lines)))

(use-package iedit
  :ensure t
  :commands (iedit-mode
			 iedit-rectangle-mode)
  :bind* (("C-." . iedit-mode)))

;; wrapper functions for expand regions
(defun sk/mark-inside-org-code ()
  "Select inside an Org code block without the org specific syntax"
  (interactive)
  (er/mark-org-code-block)
  (next-line 1)
  (exchange-point-and-mark)
  (previous-line 1)
  (end-of-line 1))

(defun sk/mark-around-LaTeX-environment ()
  "Select around a LaTeX environment with both the begin and end keywords"
  (interactive)
  (er/mark-LaTeX-inside-environment)
  (previous-line 1)
  (exchange-point-and-mark)
  (next-line 1)
  (end-of-line 1))

(defun sk/mark-around-word ()
  "Mark the word and the adjacent whitespace"
  (interactive)
  (er/mark-word)
  (exchange-point-and-mark)
  (forward-char 1))

(defun sk/mark-around-text-paragraph ()
  "Mark the paragraph and the newline"
  (interactive)
  (er/mark-text-paragraph)
  (exchange-point-and-mark)
  (next-line 1))

(defun sk/mark-inside-LaTeX-math ()
  "Mark inside the latex math"
  (interactive)
  (er/mark-LaTeX-math)
  (forward-char 1)
  (exchange-point-and-mark)
  (backward-char 1))

(defun sk/mark-inside-python-block ()
  "Mark inside a python block"
  (interactive)
  (er/mark-python-block)
  (next-line 1))

(defun sk/mark-inside-ruby-block ()
  "Mark inside a ruby/julia block"
  (interactive)
  (er/mark-ruby-block-up)
  (next-line 1)
  (exchange-point-and-mark)
  (previous-line 1))

(defun sk/mark-around-symbol ()
  "Mark around a symbol including the nearby whitespace"
  (interactive)
  (er/mark-symbol)
  (exchange-point-and-mark)
  (forward-char 1))

;; expand regions
(use-package expand-region
  :ensure t
  :bind* (("C-=" . er/expand-region)
		  ("C-r a a" . mark-whole-buffer)
		  ("C-r i a" . mark-whole-buffer)
		  ("C-r i p" . er/mark-text-paragraph)
		  ("C-r a p" . sk/mark-around-text-paragraph)
		  ("C-r i s" . er/mark-text-sentence)
		  ("C-r a s" . er/mark-text-sentence)
		  ("C-r i y" . er/mark-symbol)
		  ("C-r a y" . sk/mark-around-symbol)
		  ("C-r i c" . er/mark-comment)
		  ("C-r a c" . er/mark-comment)
		  ("C-r i w" . er/mark-word)
		  ("C-r a w" . sk/mark-around-word)
		  ("C-r i f" . er/mark-defun)
		  ("C-r a f" . er/mark-defun)
		  ("C-r i q" . er/mark-inside-quotes)
		  ("C-r a q" . er/mark-outside-quotes)
		  ("C-r i o" . sk/mark-inside-org-code)
		  ("C-r a o" . er/mark-org-code-block)
		  ("C-r i e" . er/mark-LaTeX-inside-environment)
		  ("C-r a e" . sk/mark-around-LaTeX-environment)
		  ("C-r i r" . er/mark-method-call)
		  ("C-r a r" . er/mark-method-call)
		  ("C-r i d" . sk/mark-inside-ruby-block)
		  ("C-r a d" . er/ruby-block-up)
		  ("C-r i ;" . er/mark-inside-python-string)
		  ("C-r a ;" . er/mark-outside-python-string)
		  ("C-r i m" . sk/mark-inside-python-block)
		  ("C-r a m" . er/mark-outer-python-block)
		  ("C-r i :" . er/mark-python-statement)
		  ("C-r a :" . er/mark-python-block-and-decorator)
		  ("C-r i $" . er/mark-LaTeX-math)
		  ("C-r a $" . sk/mark-inside-LaTeX-math)
		  ("C-r i b" . er/mark-inside-pairs)
		  ("C-r a b" . er/mark-outside-pairs)
		  ("C-r C-a C-a" . mark-whole-buffer)
		  ("C-r C-i C-a" . mark-whole-buffer)
		  ("C-r C-i C-p" . er/mark-text-paragraph)
		  ("C-r C-a C-p" . sk/mark-around-text-paragraph)
		  ("C-r C-i C-s" . er/mark-text-sentence)
		  ("C-r C-a C-s" . er/mark-text-sentence)
		  ("C-r C-i C-y" . er/mark-symbol)
		  ("C-r C-a C-y" . sk/mark-around-symbol)
		  ("C-r C-i C-c" . er/mark-comment)
		  ("C-r C-a C-c" . er/mark-comment)
		  ("C-r C-i C-w" . er/mark-word)
		  ("C-r C-a C-w" . sk/mark-around-word)
		  ("C-r C-i C-f" . er/mark-defun)
		  ("C-r C-a C-f" . er/mark-defun)
		  ("C-r C-i C-q" . er/mark-inside-quotes)
		  ("C-r C-a C-q" . er/mark-outside-quotes)
		  ("C-r C-i C-o" . sk/mark-inside-org-code)
		  ("C-r C-a C-o" . er/mark-org-code-block)
		  ("C-r C-i C-e" . er/mark-LaTeX-inside-environment)
		  ("C-r C-a C-e" . sk/mark-around-LaTeX-environment)
		  ("C-r C-i C-r" . er/mark-method-call)
		  ("C-r C-a C-r" . er/mark-method-call)
		  ("C-r C-i C-d" . sk/mark-inside-ruby-block)
		  ("C-r C-a C-d" . er/ruby-block-up)
		  ("C-r C-i C-;" . er/mark-inside-python-string)
		  ("C-r C-a C-;" . er/mark-outside-python-string)
		  ("C-r C-i C-m" . sk/mark-inside-python-block)
		  ("C-r C-a C-m" . er/mark-outer-python-block)
		  ("C-r C-i C-:" . er/mark-python-statement)
		  ("C-r C-a C-:" . er/mark-python-block-and-decorator)
		  ("C-r C-i C-$" . er/mark-LaTeX-math)
		  ("C-r C-a C-$" . sk/mark-inside-LaTeX-math)
		  ("C-r C-i C-b" . er/mark-inside-pairs)
		  ("C-r C-a C-b" . er/mark-outside-pairs)))

;; surrounding changing based on expand region
(use-package embrace
  :ensure t
  :bind* (("C-c s" . embrace-commander)))

;; undo history
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :bind* (("C-/" . undo-tree-undo)
		  ("M-/" . undo-tree-redo)
		  ("C-x u" . undo-tree-visualize))
  :config
  (undo-tree-mode)
  ;; to mitigate that data corruption bug in undo tree
  (use-package undohist
	:ensure t
	:demand t
	:config
	(undohist-initialize)))

;; commenting easily
(use-package comment-dwim-2
  :ensure t
  :bind* (("M-;" . comment-dwim-2)))

;; moving across marks
(use-package back-button
  :ensure t
  :diminish back-button-mode
  :commands (back-button-local-backward
			 back-button-local-forward
			 back-button-global-backward
			 back-button-global-forward)
  :config
  (back-button-mode))

;; smartparens - safe operators
(use-package smartparens
  :ensure t
  :demand t
  :diminish smartparens-strict-mode
  :diminish smartparens-mode
  :bind* (("C-c o s" . smartparens-strict-mode)
		  ("C-c o S" . smartparens-mode)
		  ("C-1" . hydra-smartparens/body))
  :config
  (require 'smartparens-config)
  (smartparens-global-mode)
  (smartparens-global-strict-mode)
  (show-smartparens-global-mode))
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
  ("h" sp-backward-sexp)
  ("l" sp-forward-sexp)
  ("j" sp-next-sexp)
  ("k" sp-previous-sexp)
  ("p" sp-backward-down-sexp)
  ("n" sp-up-sexp)
  ("f" sp-down-sexp)
  ("b" sp-backward-up-sexp)
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

;; modal with emacs bindings
(provide 'sk-god)
