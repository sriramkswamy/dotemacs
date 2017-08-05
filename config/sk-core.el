;;;;;;;;;;;;;;;;;;;;;
;;    Functions    ;;
;;;;;;;;;;;;;;;;;;;;;

;; awesome core functions
(require 'sk-macros)
(require 'sk-buffer-functions)
(require 'sk-marking-functions)
(require 'sk-editing-functions)
(require 'sk-navigation-functions)

;;;;;;;;;;;;;;;;;;;;
;;    Bindings    ;;
;;;;;;;;;;;;;;;;;;;;

;; hint for bindings
(use-package which-key
  :ensure t
  :demand t
  :diminish which-key-mode
  :bind* (("C-c ?" . which-key-show-top-level))
  :config
  ;; workaround for emacs 26
  (if (version< emacs-version "26")
	  (message "Tracking stable Emacs")
	(defalias 'display-buffer-in-major-side-window 'window--make-major-side-window))
  ;; turn on which key and add some names for default/common prefixes
  (which-key-enable-god-mode-support)
  (which-key-mode)
  (which-key-add-key-based-replacements
	"C-x ESC" "complex command"
	"C-x RET" "file encoding"
	"C-x 4" "window"
	"C-x 5" "frame"
	"C-x 6" "2C prefix"
	"C-x 8" "unicode"
	"C-x @" "event"
	"C-x X" "edebug"
	"C-x C-a" "edebug set"
	"C-x n" "narrow"
	"C-x r" "rect/reg/bookmarks"
	"C-x a" "abbrev"
	"C-x a i" "inverse"
	"M-s h" "highlight"
	"C-c !" "flycheck"
	"C-c &" "yasnippets"))

;;;;;;;;;;;;;;;;;;;;;;
;;    Navigation    ;;
;;;;;;;;;;;;;;;;;;;;;;

;; simulating mouse click
(use-package avy
  :ensure t
  :bind* (("C-c ;" . avy-goto-line))
  :init
  (setq avy-keys-alist
		`((avy-goto-char-2			. (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))
		  (avy-goto-word-1			. (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))
		  (avy-goto-char-2	. (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))
		  (avy-goto-char-in-line	. (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))
		  (avy-goto-char-2			. (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))
		  (avy-goto-char-2-above	. (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))
		  (avy-goto-char-2-below	. (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))
		  (avy-goto-line			. (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))))
  ;; (setq avy-style 'pre)
  (setq avy-background t)
  (defface avy-lead-face-0
	'((t (:foreground "white" :background "color-21")))
	"Face used for first non-terminating leading chars."))

;; jump to windows quickly
(use-package ace-window
  :ensure t
  :bind* (("C-x C-o" . ace-window)))

;; jump and open links fast
(use-package ace-link
  :ensure t
  :demand t
  :config
  (ace-link-setup-default))

;; switch window configs
(use-package eyebrowse
  :ensure t
  :diminish eyebrowse-mode
  :init
  (setq eyebrowse-wrap-around t)
  :commands (eyebrowse-switch-to-window-config-0
             eyebrowse-switch-to-window-config-1
             eyebrowse-switch-to-window-config-2
             eyebrowse-switch-to-window-config-3
             eyebrowse-switch-to-window-config-4
             eyebrowse-switch-to-window-config-5
             eyebrowse-switch-to-window-config-6
             eyebrowse-switch-to-window-config-7
             eyebrowse-switch-to-window-config-8
             eyebrowse-switch-to-window-config-9
             eyebrowse-switch-to-window-config
             eyebrowse-close-window-config
             eyebrowse-prev-window-config
             eyebrowse-next-window-config
             eyebrowse-last-window-config
             eyebrowse-create-window-config
             eyebrowse-rename-window-config)
  :config
  (eyebrowse-mode t))

;; moving across changes
(use-package goto-chg
  :ensure t
  :commands (goto-last-change
			 goto-last-change-reverse))

;; smart beginning and end in buffers
(use-package beginend
  :ensure t
  :diminish (beginend-global-mode . "")
  :diminish (beginend-prog-mode . "")
  :demand t
  :config
  (beginend-global-mode))

;; moving across marks
(use-package back-button
  :ensure t
  :commands (back-button-local-backward
			 back-button-local-forward
			 back-button-global-backward
			 back-button-global-forward))

;; dash documentation
(use-package dash-at-point
  :ensure t
  :bind* (("C-M-h" . dash-at-point-with-docset)))

;; dumb jumping
(use-package dumb-jump
  :ensure t
  :commands (dumb-jump-go
			 dumb-jump-goto-file-line
			 dumb-jump-mode)
  :config
  (dumb-jump-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Editing helpers    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; commenting easily
(use-package comment-dwim-2
  :ensure t
  :bind* (("M-;" . comment-dwim-2)))

;; counting/replacing with ease
(use-package anzu
  :ensure t
  :diminish anzu-mode
  :commands (anzu-query-replace-at-cursor
			 anzu-query-replace-at-cursor-thing
			 anzu-query-replace-regexp)
  :config
  (global-anzu-mode))

;; undo history
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :bind* (("C-/"	. undo-tree-undo)
		  ("M-/"	. undo-tree-redo)
		  ("C-x u"	. undo-tree-visualize))
  :config
  (undo-tree-mode))

;; snippets
(use-package yasnippet
  :ensure t
  :commands (yas-insert-snippet
             yas-abort-snippet
             yas-tryout-snippet
             yas-minor-mode
             yas-visit-snippet-file
             yas-load-snippet-buffer
             yas-load-snippet-buffer-and-close
             yas-reload-all
             yas-new-snippet)
  :bind* (("C-l"		. yas-insert-snippet)
		  ("C-c o y"	. yas-global-mode))
  :diminish (yas-minor-mode . " γ")
  :config
  (setq yas/triggers-in-field t); Enable nested triggering of snippets
  (setq yas-prompt-functions '(yas-completing-prompt))
  (add-hook 'snippet-mode-hook '(lambda () (setq-local require-final-newline nil)))
  (yas-global-mode))

;; select arbitrary regions
(use-package expand-region
  :ensure t
  :bind* (("C-="		. er/expand-region)
          ("C-c v ="	. er/expand-region)
		  ("C-c v a a"	. mark-whole-buffer)
		  ("C-c v i a"	. mark-whole-buffer)
		  ("C-c v i p"	. er/mark-text-paragraph)
		  ("C-c v a p"	. mark-paragraph)
		  ("C-c v i l"	. sk/select-inside-line)
		  ("C-c v a l"	. sk/select-around-line)
		  ("C-c v i s"	. er/mark-text-sentence)
		  ("C-c v a s"	. er/mark-text-sentence)
		  ("C-c v i y"	. er/mark-symbol)
		  ("C-c v a y"	. sk/mark-around-symbol)
		  ("C-c v i c"	. er/mark-comment)
		  ("C-c v a c"	. er/mark-comment)
		  ("C-c v i w"	. er/mark-word)
		  ("C-c v a w"	. sk/mark-around-word)
		  ("C-c v i f"	. er/mark-defun)
		  ("C-c v a f"	. er/mark-defun)
		  ("C-c v i q"	. er/mark-inside-quotes)
		  ("C-c v a q"	. er/mark-outside-quotes)
		  ("C-c v i o"	. sk/mark-inside-org-code)
		  ("C-c v a o"	. er/mark-org-code-block)
		  ("C-c v i u"	. sk/mark-inside-subtree)
		  ("C-c v a u"	. org-mark-subtree)
		  ("C-c v i e"	. er/mark-LaTeX-inside-environment)
		  ("C-c v a e"	. LaTeX-mark-environment)
		  ("C-c v i r"	. er/mark-method-call)
		  ("C-c v a r"	. er/mark-method-call)
		  ("C-c v i d"	. sk/mark-inside-ruby-block)
		  ("C-c v a d"	. er/ruby-block-up)
		  ("C-c v i g"	. er/mark-inside-python-string)
		  ("C-c v a g"	. er/mark-outside-python-string)
		  ("C-c v i m"	. sk/mark-inside-python-block)
		  ("C-c v a m"	. er/mark-outer-python-block)
		  ("C-c v i n"	. er/mark-python-statement)
		  ("C-c v a n"	. er/mark-python-block-and-decorator)
		  ("C-c v i $"	. er/mark-LaTeX-math)
		  ("C-c v a $"	. sk/mark-inside-LaTeX-math)
		  ("C-c v i b"	. er/mark-inside-pairs)
		  ("C-c v a b"	. er/mark-outside-pairs))
  :config
  (require 'sk-er-functions))

;; work on indentation
(use-package indent-tools
  :ensure t
  :bind* (("C-c i" . indent-tools-hydra/body))
  :commands (indent-tools-comment
			 indent-tools-indent
			 indent-tools-demote
			 indent-tools-minor-mode
			 indent-tools-kill-tree
			 indent-tools-goto-child
			 indent-tools-kill-level
			 indent-tools-goto-parent
			 indent-tools-indent-paragraph
			 indent-tools-select
			 indent-tools-end-of-level
			 indent-tools-indent-space
			 indent-tools-goto-end-of-tree
			 indent-tools-copy
			 indent-tools-indent-end-of-defun
			 indent-tools-indent-end-of-level
			 indent-tools-goto-next-sibling
			 indent-tools-goto-previous-sibling
			 indent-tools-select-end-of-tree)
  :config
  ;; indent tools hydra
(defhydra indent-tools-hydra (:color red :hint nil)
  "
 ^Indent^         | ^Navigation^        | ^Actions^
------------------+---------------------+-----------
 _>_ indent       | _j_ v               | _d_ kill
 _<_ de-indent    | _k_ ʌ               | _i_ imenu
 _e_ end of level | _n_ next sibling    | _y_ Copy…
 _f_ defun        | _N_ previous sibling| _c_ comment
 _p_ paragraph    | _h_ up parent       | _u_ uncomment (paragraph)
 _SPC_ space      | _l_ down child      | _r_ fold
 _s_ select       | _t_ end of tree     | _q_ quit
"

  (">" indent-tools-indent)
  ("<" indent-tools-demote)
  ("f" indent-tools-indent-end-of-defun)
  ("c" indent-tools-comment)
  ("u" indent-tools-uncomment)
  ("p" indent-tools-indent-paragraph)
  ("e" indent-tools-indent-end-of-level)
  ("d" indent-tools-kill-hydra/body :color blue)
  ("y" indent-tools-copy-hydra/body :color blue)
  ("s" indent-tools-select)
  ("t" indent-tools-goto-end-of-tree)
  ("h" indent-tools-goto-parent)
  ("l" indent-tools-goto-child)
  ("t" indent-tools-select-end-of-tree)
  ("n" indent-tools-goto-next-sibling)
  ("N" indent-tools-goto-previous-sibling)
  ("i" imenu)
  ("j" forward-line)
  ("k" previous-line)
  ("SPC" indent-tools-indent-space)
  ("r" hs-toggle-hiding)
  ("q" nil))

(indent-tools-minor-mode))

;; some wrapper functions for indent-tools
(defun sk/select-indent-tree ()
	"select the indent tree"
	(interactive)
	(indent-tools-select-end-of-tree)
	(exchange-point-and-mark)
	(indent-tools-select))
  (defun sk/copy-indent-level ()
	"copies the indent level"
	(interactive)
	(indent-tools-copy "level"))
  (defun sk/copy-indent-tree ()
	"copies the indent tree"
	(interactive)
	(indent-tools-copy "tree"))

;; surrounding changing based on expand region
(use-package embrace
  :ensure t
  :bind* (("C-c S" . embrace-commander)))

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
  :bind* (("C-c \""			. mc/mark-previous-like-this)
		  ("C-c '"			. mc/mark-next-like-this)
		  ("C-c ,"			. mc/mark-all-like-this)
		  ("C-c g ,"		. mc/mark-sgml-tag-pairs)
		  ("C-c <left>"		. mc/skip-to-previous-like-this)
		  ("C-c <right>"	. mc/skip-to-next-like-this)
		  ("C-c <up>"		. mc/unmark-previous-like-this)
		  ("C-c <down>"		. mc/unmark-next-like-this)
		  ("C-c >"			. mc/edit-ends-of-lines)
		  ("C-c <"			. mc/edit-beginnings-of-lines)))

;; quick multiple editing
(use-package iedit
  :ensure t
  :bind* (("C-c g i" . iedit-mode)))

;; safe operators
(use-package smartparens
  :ensure t
  :demand t
  :diminish smartparens-strict-mode
  :diminish smartparens-mode
  :bind* (("C-c o s" . smartparens-strict-mode)
		  ("C-c o p" . smartparens-mode))
  :config
  (require 'smartparens-config)
  (smartparens-global-mode)
  (smartparens-global-strict-mode)
  (show-smartparens-global-mode))

;; convert cases
(use-package string-inflection
  :ensure t
  :commands (string-inflection-all-cycle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    version control     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; best git wrapper ever
(use-package magit
  :ensure t
  :bind* (("C-c e"		. magit-status)
		  ("C-c g b"	. magit-blame))
  :config
  ;; Github integration - press '@' in Magit status
  ;; (use-package magithub
  ;; 	:ensure t)
  )

;; highlight diffs
(use-package diff-hl
  :ensure t
  :bind* (("C-c v i h" . diff-hl-mark-hunk)
		  ("C-c v a h" . diff-hl-mark-hunk))
  :commands (global-diff-hl-mode
			 diff-hl-mode
			 diff-hl-next-hunk
			 diff-hl-previous-hunk
			 diff-hl-mark-hunk
			 diff-hl-diff-goto-hunk
			 diff-hl-revert-hunk)
  :config
  (global-diff-hl-mode)
  ;; (diff-hl-flydiff-mode)
  (diff-hl-margin-mode)
  (diff-hl-dired-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Error checking     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; error checking
(use-package flycheck
  :ensure t
  :defer 2
  :diminish flycheck-mode
  :commands (flycheck-buffer
			 flycheck-previous-error
			 flycheck-next-error
			 flycheck-list-errors
			 flycheck-explain-error-at-point
			 flycheck-display-error-at-point
			 flycheck-select-checker
			 flycheck-verify-setup)
  :config
  (global-flycheck-mode))

;; jump to flycheck error messages
(use-package avy-flycheck
  :ensure t
  :bind* (("C-c g TAB" . avy-flycheck-goto-error)))

;; debugging with gud
(use-package gud
  :commands (gud-down
			 gud-display-frame
			 gud-tooltip-dereference
			 gud-up
			 gud-gdb
			 gud-next
			 gud-cont
			 gud-mode
			 gud-step
			 gud-print
			 gud-break
			 gud-finish
			 gud-remove
			 gud-refresh
			 gud-goto-info
			 gud-basic-call
			 gud-stop-subjob
			 gud-tooltip-mode
			 gud-find-c-expr
			 gud-tooltip-mouse-motion)
  :config
  (require 'gdb-mi))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Semi-modal states    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; hydra
(use-package hydra
  :ensure t
  :demand t)

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
(bind-key* "C-c s" 'hydra-smartparens/body)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Multi-modal buffers     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;
;;    Modal states    ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; getting "ESC" to work correctly in the terminal
(defvar sk/fast-keyseq-timeout 50)
(defun sk/tty-ESC-filter (map)
  (if (and (equal (this-single-command-keys) [?\e])
		   (sit-for (/ sk/fast-keyseq-timeout 1000.0)))
	  [escape] map))
(defun sk/lookup-key (map key)
  (catch 'found
	(map-keymap (lambda (k b) (if (equal key k) (throw 'found b))) map)))
(defun sk/catch-tty-ESC ()
  "Setup key mappings of current terminal to turn a tty's ESC into `escape'."
  (interactive)
  (when (memq (terminal-live-p (frame-terminal)) '(t pc))
	(let ((esc-binding (sk/lookup-key input-decode-map ?\e)))
	  (define-key input-decode-map
		[?\e] `(menu-item "" ,esc-binding :filter sk/tty-ESC-filter)))))
(add-hook 'after-init-hook 'sk/catch-tty-ESC)
(add-hook 'prog-mode-hook 'sk/catch-tty-ESC)
(add-hook 'text-mode-hook 'sk/catch-tty-ESC)
(bind-key* "C-]" 'sk/catch-tty-ESC)

;; wrapper for entering and exiting ryo modal mode
(defun sk/enable-ryo-modal-mode ()
  "Explicitly enables ryo-modal mode"
  (interactive)
  (ryo-modal-mode 1))
(defun sk/disable-ryo-modal-mode ()
  "Explicitly disables ryo-modal mode"
  (interactive)
  (ryo-modal-mode -1))
(add-hook 'with-editor-mode-hook 'sk/disable-ryo-modal-mode)

;; open and edit line below
(defun sk/open-and-edit-line-below ()
  (interactive)
  (sk/open-line-below)
  (ryo-modal-mode -1))

;; open and edit line above
(defun sk/open-and-edit-line-above ()
  (interactive)
  (sk/open-line-above)
  (ryo-modal-mode -1))

;; roll your own modal mode
(use-package ryo-modal
  :ensure t
  :demand t
  :diminish (ryo-modal-mode . " μ")
  :bind* (("<escape>" . sk/enable-ryo-modal-mode))
  :bind (:map ryo-modal-mode-map
			  ;; common
			  ("."         	. repeat)
              ("c x"		. sk/doc-fit)
			  ("d x"		. sk/other-doc-fit)
			  ("SPC v"		. sk/remove-mark)
			  ("m :"		. eval-expression)
			  ("m SPC"		. org-store-link)
			  ("v z"		. customize-group)
			  ;; navigating
			  ("m ."        . dumb-jump-go)
			  ;; editing
			  ("i"			. sk/disable-ryo-modal-mode)
			  ("o"			. sk/open-and-edit-line-below)
			  ("O"			. sk/open-and-edit-line-above)
			  ("p"			. yank)
			  ("x"			. sp-delete-char)
			  ("s"			. embrace-commander)
			  ("X"			. exchange-point-and-mark)
			  ("~"			. sk/toggle-letter-case)
			  ("y n"		. sk/occur-at-point)
			  ("R"		    . overwrite-mode)
			  ("g S"		. electric-newline-and-maybe-indent)
			  ("g J"		. join-line)
			  ("c c"		. sk/change-region-or-line)
			  ("d d"		. sk/kill-region-or-line)
			  ("y y"		. sk/copy-region-or-line)
			  ("g U U"		. sk/upcase-region-or-line)
			  ("g u u"		. sk/downcase-region-or-line)
			  ("g C C"		. sk/capitalize-region-or-line)
			  ("g q q"		. sk/fill-region)
			  ("g Q Q"		. sk/unfill-region)
			  ("y v"		. sk/find-init)
			  ("c v"		. anzu-replace-at-cursor-thing)
			  ("g +"		. sk/change-number-at-point)
			  ("g -"		. sk/subtract-number-at-point)
			  ("g ="		. sk/goto-closest-number)
			  ;; bookmarks
			  ("y u"		. bookmark-jump)
			  ("y d"		. bookmark-delete)
			  ("y c"		. bookmark-save)
			  ;; debugging
			  ("m g SPC"    . sk/breakpoint-icon-set)
			  ("m g DEL"    . sk/breakpoint-icon-remove)
			  ;; macros
			  ("c q"		. kmacro-edit-macro)
			  ("v q"		. apply-macro-to-region-lines)
			  ("d q"		. kmacro-cycle-ring-previous)
			  ("y q"		. kmacro-cycle-ring-next)
			  ;; motion
			  ("b"			. backward-word)
			  ("e"			. forward-word)
			  ("{"			. backward-paragraph)
			  ("}"			. forward-paragraph)
			  ("("			. backward-sentence)
			  (")"			. forward-sentence)
			  ("f"			. avy-goto-char-in-line)
			  ("F"			. avy-goto-char-2)
			  ("'"  		. avy-resume)
			  (";"  		. avy-goto-line)
			  ("\""  		. avy-pop-mark)
			  ("M"			. move-to-window-line-top-bottom)
			  ;; rectangle mode
			  ("v n"		. rectangle-number-lines)
			  ("v p"		. open-rectangle)
			  ("P"			. yank-rectangle)
			  ("c r"		. string-rectangle)
			  ("d r"		. kill-rectangle)
			  ("y r"		. copy-rectangle-as-kill)
			  ("v r"		. rectangle-mark-mode)
			  ;; window
			  ("w u"		. winner-undo)
			  ("w U"		. winner-redo)
			  ;; exchange
			  ("c z l"	. sk/transpose-words-forward)
			  ("c z h"	. sk/transpose-words-backward)
			  ("c z j"	. sk/move-text-down)
			  ("c z k"	. sk/move-text-up)
			  ("d z k"	. sk/blank-line-above)
			  ("d z j"	. sk/blank-line-below)
			  ;; org mode
			  ("m <"		. org-edit-src-abort)
			  ("m >"		. org-edit-special)
			  ("m ="		. org-edit-src-exit)
			  ;; comint mode maps
			  ("m ! q"		. comint-quit-subjob)
			  ("m ! a"		. comint-bol-or-process-mark)
			  ("m ! b"		. shell-backward-command)
			  ("m ! c"		. comint-interrupt-subjob)
			  ("m ! d"		. comint-send-eof)
			  ("m ! e"		. comint-show-maximum-output)
			  ("m ! f"		. shell-forward-command)
			  ("m ! l"		. comint-dynamic-list-input-ring)
			  ("m ! n"		. comint-next-prompt)
			  ("m ! o"		. comint-delete-prompt)
			  ("m ! p"		. comint-previous-prompt)
			  ("m ! r"		. comint-show-output)
			  ("m ! s"		. comint-write-output)
			  ("m ! u"		. comint-kill-input)
			  ("m ! x"		. comint-next-from-history)
			  ("m ! z"		. comint-stop-subjob)
			  ;; display
			  ("v m"		. redraw-display)
			  ;; scrolling
			  ("c n"		. scroll-other-window)
			  ("c p"		. scroll-other-window-down)
			  ("d n"		. sk/other-doc-down)
			  ("d p"		. sk/other-doc-up))

  :init
  ;; default cursor shape
  (setq-default cursor-type '(bar . 1))
  ;; cursor for ryo modal mode
  (defvar ryo-modal-cursor-color "#a9a9a9"
  	"The cursor color used in `ryo-modal-mode'.  If nil then use default color.")

  :config
  ;; remove the prefix 'ryo' from which-key
  ;; (push '((nil . "ryo:") . (nil . "")) which-key-replacement-alist)

  ;; cursor when not in ryo modal mode
  (defconst ryo-modal-default-cursor-color "#7f7f7f"
  	"Default color of cursor.")
  ;; some helper functions
  (require 'sk-ryo-functions))

;; activate it globally
(add-hook 'text-mode-hook 'sk/enable-ryo-modal-mode)
(add-hook 'prog-mode-hook 'sk/enable-ryo-modal-mode)
(add-hook 'fundamental-mode-hook 'sk/enable-ryo-modal-mode)
(add-hook 'doc-view-mode-hook 'sk/enable-ryo-modal-mode)
(add-hook 'poly-head-tail-mode-hook 'sk/enable-ryo-modal-mode)

;; key chords for escaping out to ryo-modal mode
(defun sk/key-seq-ryo-escape ()
  "key sequence to trigger tty-escape and go to ryo modal mode"
  (interactive)
  (sk/catch-tty-ESC)
  (sk/enable-ryo-modal-mode))

;; use double space for escaping and going to ryo modal mode
(use-package key-seq
  :ensure t
  :demand t
  :config
  (key-chord-mode 1)
  (key-seq-define-global "  " 'sk/key-seq-ryo-escape))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Semantic parsing     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package semantic
  :commands (semantic-mode
			 semantic-debug
			 semantic-symref
			 semantic-lex-test
			 semantic-add-label
			 semantic-gcc-setup
			 semantic-lex-debug
			 semantic-show-label
			 semanticdb-ref-test
			 semanticdb-reset-log
			 semantic-describe-tag
			 semantic-symref-symbol
			 semantic-force-refresh
			 semantic-symref-regexp
			 semantic-adebug-analyze
			 semantic-describe-buffer
			 semantic-decoration-mode
			 semantic-speedbar-analysis
			 semantic-ia-fast-jump
			 semantic-narrow-to-tag
			 semanticdb-save-all-db
			 semantic-ia-show-summary
			 semantic-ia-show-variants
			 semantic-lex-spp-describe
			 semantic-symref-find-text
			 semantic-idle-summary-mode
			 semantic-highlight-func-menu
			 semantic-highlight-func-mode
			 semantic-dump-parser-warnings
			 semantic-highlight-edits-mode
			 semanticdb-toggle-global-mode
			 semanticdb-find-toggle-logging
			 semantic-toggle-decoration-style
			 semantic-complete-jump
			 semantic-complete-done
			 semantic-calculate-scope
			 semantic-complete-symbol
			 semantic-ia-fast-mouse-jump
			 semantic-show-parser-state-mode
			 semantic-analyze-proto-impl-toggle
			 semantic-highlight-func-popup-menu
			 semantic-show-unmatched-syntax-mode
			 semantic-show-unmatched-syntax-next
			 semanticdb-find-test-translate-path
			 semanticdb-enable-gnu-global-databases
			 semantic-sanity-check
			 semantic-complete-inline-up
			 semantic-complete-inline-TAB
			 semantic-complete-jump-local
			 semantic-clear-toplevel-cache
			 semantic-complete-inline-done
			 semantic-complete-inline-down
			 semantic-complete-inline-exit
			 semantic-complete-inline-quit
			 semantic-complete-self-insert
			 semantic-complete-complete-tab
			 semanticdb-cleanup-cache-files
			 semantic-c-describe-environment
			 semantic-complete-analyze-inline
			 semantic-complete-complete-space
			 semantic-complete-inline-project
			 semantic-symref-find-tags-by-name
			 semantic-symref-find-tags-by-regexp
			 semantic-lex-spp-enable-debug-symbol
			 semantic-symref-find-references-by-name
			 semanticdb-save-current-db
			 semantic-analyze-current-tag
			 semantic-analyze-current-context
			 semantic-test-data-cache
			 semantic-analyze-possible-completions
			 semantic-c-debug-mode-init
			 global-semanticdb-minor-mode
			 global-semantic-decoration-mode
			 semantic-c-add-preprocessor-symbol
			 semantic-complete-jump-local-members
			 semantic-complete-analyze-and-replace
			 semantic-complete-analyze-inline-idle
			 semantic-customize-system-include-path
			 semantic-symref-find-file-references-by-name
			 semanticdb-find-test-translate-path-no-loading
			 global-semantic-idle-summary-mode
			 global-semantic-mru-bookmark-mode
			 global-semantic-highlight-func-mode
			 global-semantic-highlight-edits-mode
			 semantic-c-evaluate-symbol-for-hideif
			 semantic-ia-complete-tip
			 semantic-ia-complete-symbol
			 semantic-idle-completions-mode
			 semantic-ia-describe-class
			 global-semantic-show-parser-state-mode
			 global-semantic-show-unmatched-syntax-mode
			 semantic-ia-complete-symbol-menu
			 semantic-symref-find-tags-by-completion
			 global-semantic-idle-completions-mode
			 semantic-idle-scheduler-mode
			 semantic-ia-show-doc
			 semantic-idle-breadcrumbs-mode
			 semantic-decoration-include-menu
			 semantic-decoration-include-visit
			 semantic-decoration-include-describe
			 semantic-debug-idle-function
			 semantic-add-system-include
			 semantic-reset-system-include
			 semantic-remove-system-include
			 semantic-idle-breadcrumbs-popup-menu
			 semantic-idle-breadcrumbs--popup-menu
			 semantic-idle-local-symbol-highlight-mode
			 semantic-decoration-on-include-menu
			 semantic-decoration-all-include-summary
			 semantic-decoration-unknown-include-menu
			 semantic-decoration-fileless-include-menu
			 semantic-decoration-unparsed-include-menu
			 semantic-decoration-unknown-include-describe
			 semantic-decoration-fileless-include-describe
			 semantic-decoration-unparsed-include-describe
			 semantic-debug-idle-work-function
			 semanticdb-find-adebug-lost-includes
			 semanticdb-find-adebug-scanned-includes
			 semantic-decoration-unparsed-include-parse-include
			 global-semantic-idle-scheduler-mode
			 semantic-decoration-on-unknown-include-menu
			 semantic-decoration-on-fileless-include-menu
			 semantic-decoration-on-unparsed-include-menu
			 semantic-decoration-unparsed-include-parse-all-includes
			 global-semantic-idle-breadcrumbs-mode
			 semantic-stickyfunc-menu
			 semantic-stickyfunc-mode
			 global-semantic-idle-local-symbol-highlight-mode
			 semantic-stickyfunc-popup-menu
			 global-semantic-stickyfunc-mode

			 ;; senator
			 senator-copy-tag
			 senator-kill-tag
			 senator-next-tag
			 senator-yank-tag
			 senator-previous-tag
			 senator-transpose-tags-up
			 senator-transpose-tags-down
			 senator-go-to-up-reference
			 senator-copy-tag-to-register)

  :config
  ;; include semantic
  (require 'semantic)

  ;; add extra c++ sources
  (cond ((eq system-type 'darwin)
		 (let* ((sk/boost-location "/usr/local/include/boost")
				(sk/armadillo-location "/usr/local/include/armadillo")
				(sk/armadillo-bits-location "/usr/local/include/armadillo_bits")
				(sk/gsl-location "/usr/local/include/gsl"))
		   (if (file-exists-p sk/boost-location)
			   (semantic-add-system-include sk/boost-location 'c++-mode)
			 (message "boost not found"))
		   (if (file-exists-p sk/armadillo-location)
			   (semantic-add-system-include sk/armadillo-location 'c++-mode)
			 (message "armadillo not found"))
		   (if (file-exists-p sk/armadillo-bits-location)
			   (semantic-add-system-include sk/armadillo-bits-location 'c++-mode)
			 (message "armadillo-bits not found"))
		   (if (file-exists-p sk/gsl-location)
			   (semantic-add-system-include sk/gsl-location 'c++-mode)
			 (message "gsl not found"))))
		((eq system-type 'gnu/linux)
		 (let* ((sk/boost-location "/usr/include/boost")
				(sk/armadillo-location "/usr/include/armadillo")
				(sk/armadillo-bits-location "/usr/include/armadillo_bits")
				(sk/gsl-location "/usr/include/gsl"))
		   (if (file-exists-p sk/boost-location)
			   (semantic-add-system-include sk/boost-location 'c++-mode)
			 (message "boost not found"))
		   (if (file-exists-p sk/armadillo-location)
			   (semantic-add-system-include sk/armadillo-location 'c++-mode)
			 (message "armadillo not found"))
		   (if (file-exists-p sk/armadillo-bits-location)
			   (semantic-add-system-include sk/armadillo-bits-location 'c++-mode)
			 (message "armadillo-bits not found"))
		   (if (file-exists-p sk/gsl-location)
			   (semantic-add-system-include sk/gsl-location 'c++-mode)
			 (message "gsl not found")))))

  ;; add extra c sources
  (cond ((eq system-type 'darwin)
		 (let* ((sk/gsl-location "/usr/local/include/gsl"))
		   (if (file-exists-p sk/gsl-location)
			   (semantic-add-system-include sk/gsl-location 'c-mode)
			 (message "gsl not found"))))
		((eq system-type 'gnu/linux)
		 (let* ((sk/gsl-location "/usr/include/gsl"))
		   (if (file-exists-p sk/gsl-location)
			   (semantic-add-system-include sk/gsl-location 'c-mode)
			 (message "gsl not found")))))

  ;; activate semantic mode
  (global-semanticdb-minor-mode 1)
  (global-semantic-idle-scheduler-mode 1)
  (semantic-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Auto completion     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package company
  :ensure t
  :init
  (setq company-minimum-prefix-length 2
		company-require-match 0
		company-selection-wrap-around t
		company-tooltip-limit 20                       ; bigger popup window
		company-tooltip-align-annotations 't           ; align annotations to the right tooltip border
		company-idle-delay .2                          ; decrease delay before autocompletion popup shows
		company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
  (setq company-dabbrev-ignore-buffers "\\.pdf\\'"
		company-dabbrev-downcase nil
		company-dabbrev-code-modes t
		company-dabbrev-code-other-buffers 'all
		company-dabbrev-other-buffers 'all
		company-dabbrev-code-everywhere t)

  :bind* (("C-t t"		. company-complete)
		  ("C-t C-t"	. company-complete)
		  ("C-d"		. company-complete)
		  ("C-t f"		. company-files)
		  ("C-t C-f"	. company-files)
		  ("C-t s"		. company-ispell)
		  ("C-t C-s"	. company-ispell)
		  ("C-t a"		. company-dabbrev)
		  ("C-t C-a"	. company-dabbrev))
  :bind (:map company-active-map
			   ("C-n"    . company-select-next)
			   ("C-p"    . company-select-previous)
			   ([return] . company-complete-selection)
			   ([tab]    . yas-expand)
			   ("TAB"    . yas-expand)
			   ("C-w"    . backward-kill-word)
			   ("C-c"    . company-abort)
			   ("C-c"    . company-search-abort))
  :diminish (company-mode . " ς")

  :config
  ;; set default backends
  (setq company-backends
      '((company-files          ; files & directory
         company-keywords       ; keywords
		 company-dabbrev-code   ; code abbrev
		 company-etags          ; etags
		 company-gtags          ; gtags
		 company-semantic       ; semantic
		 company-bbdb           ; bbdb
		 company-eclim          ; eclim
         company-capf)))
  (global-company-mode))

;;; some language specific functions to add to hooks later

;; C
(defun sk/company-c ()
  "Add backends for C completion in company mode"
  (interactive)
  (require 'company)
  (make-local-variable 'company-backends)
  (setq company-backends
		'((;; Generic backends
		   company-files
		   ;; company-keywords
		   ;; company-dabbrev-code
		   company-gtags
		   company-semantic
		   company-capf
		   ;; C++ specific backends
		   company-clang
		   company-rtags
		   company-irony
		   company-irony-c-headers
		   company-cmake))))
;; push company backend
(add-hook 'c-mode-hook 'sk/company-c)

;; C++
(defun sk/company-cpp ()
  "Add backends for C++ completion in company mode"
  (interactive)
  (require 'company)
  (make-local-variable 'company-backends)
  (setq company-backends
		'((;; Generic backends
		   company-files
		   ;; company-keywords
		   ;; company-dabbrev-code
		   company-gtags
		   company-semantic
		   company-capf
		   ;; C++ specific backends
		   company-clang
		   company-rtags
		   company-irony
		   company-irony-c-headers
		   company-cmake))))
;; push company backend
(add-hook 'c++-mode-hook 'sk/company-cpp)

;; Python/Cython
(defun sk/company-python ()
  "Add backends for python completion in company mode"
  (interactive)
  (require 'company)
  (make-local-variable 'company-backends)
  (setq company-backends
		'((;; Generic backends
		   company-files
		   ;; company-keywords
		   ;; company-dabbrev-code
		   company-gtags
		   company-semantic
		   company-capf
		   ;; python specific backends
		   ;; company-jedi
		   company-anaconda))))
(defun sk/company-cython ()
  "Add backends for cython completion in company mode"
  (interactive)
  (require 'company)
  (make-local-variable 'company-backends)
  (setq company-backends
		'((;; Generic backends
		   company-files
		   ;; company-keywords
		   ;; company-dabbrev-code
		   company-gtags
		   company-semantic
		   company-capf
		   ;; cython specific backends
		   ;; company-jedi
		   company-anaconda
		   company-clang))))
;; push company backend
(add-hook 'python-mode-hook 'sk/company-python)
;; push company backend
(add-hook 'cython-mode-hook 'sk/company-cython)

;; ESS
(defun sk/company-ess ()
  "Add backends for ESS completion in company mode"
  (interactive)
  (require 'company)
  (make-local-variable 'company-backends)
  (setq company-backends
		'((;; Generic backends
		   company-files
		   ;; company-keywords
		   ;; company-dabbrev-code
		   company-gtags
		   company-semantic
		   company-capf
		   ;; ESS specific backends
		   company-ess))))
;; push company backend
(add-hook 'ess-mode-hook 'sk/company-ess)

;; Org mode
(defun sk/company-org ()
  "Add backends for Org completion in company mode"
  (interactive)
  (require 'company)
  (make-local-variable 'company-backends)
  (setq company-backends
		'((;; Generic backends
		   company-files
		   ;; company-keywords
		   company-dabbrev
		   ;; company-gtags
		   ;; company-capf
		   ;; org specific backends
		   ;; company-ispell
		   ))))
;; push company backend
(add-hook 'org-mode-hook 'sk/company-org)

;; Markdown mode
(defun sk/company-markdown ()
  "Add backends for Markdown completion in company mode"
  (interactive)
  (require 'company)
  (make-local-variable 'company-backends)
  (setq company-backends
		'((;; Generic backends
		   company-files
		   company-keywords
		   company-dabbrev
		   company-gtags
		   company-capf
		   ;; markdown specific backends
		   company-ispell))))
;; push company backend
(add-hook 'markdown-mode-hook 'sk/company-markdown)

;; LaTeX mode
(defun sk/company-latex ()
  "Add backends for LaTeX completion in company mode"
  (interactive)
  (require 'company)
  (make-local-variable 'company-backends)
  (setq company-backends
		'((;; Generic backends
		   company-files
		   company-keywords
		   company-dabbrev
		   company-gtags
		   company-capf
		   ;; latex specific backends
		   ;; company-auctex
		   company-ispell))))
;; push company backend
(add-hook 'latex-mode-hook 'sk/company-latex)

;; web mode
(defun sk/company-web ()
  "Add backends for web completion in company mode"
  (interactive)
  (require 'company)
  (make-local-variable 'company-backends)
  (setq company-backends
		'((;; Generic backends
		   company-files
		   company-keywords
		   company-dabbrev-code
		   company-gtags
		   company-semantic
		   company-capf
		   ;; web specific backends
		   company-nxml
		   company-css
		   company-css-html-tags))))

;; shell mode
(defun sk/company-shell ()
  "Add backends for shell completion in company mode"
  (interactive)
  (require 'company)
  (make-local-variable 'company-backends)
  (setq company-backends
		'((;; Generic backends
		   company-files
		   ;; company-keywords
		   ;; company-dabbrev-code
		   ;; company-gtags
		   ;; company-semantic
		   ;; company-capf
		   ;; shell specific backends
		   company-shell))))
;; push company backend
(add-hook 'shell-mode-hook 'sk/company-shell)

;; matlab shell mode
(defun sk/company-matlab-shell ()
  "Add backends for shell completion in company mode"
  (interactive)
  (require 'company)
  (make-local-variable 'company-backends)
  (setq company-backends
		'((;; Generic backends
		   company-files
		   ;; company-keywords
		   ;; company-dabbrev-code
		   ;; company-gtags
		   ;; company-semantic
		   ;; company-capf
		   ;; shell specific backends
		   company-matlab-shell))))
(if (fboundp 'matlab-shell-mode)
	(add-hook 'matlab-shell-mode-hook 'sk/company-matlab-shell))

;;;;;;;;;;;;;;;;;;;;;;
;;    Narrowing     ;;
;;;;;;;;;;;;;;;;;;;;;;

;; choose a narrowing framework
(require 'sk-helm)
;; (require 'sk-ivy)

;; hints for all the core bindings
(require 'sk-ryo-which-key)

;; provide this core configuration
(provide 'sk-core)
