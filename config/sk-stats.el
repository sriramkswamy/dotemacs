;; Emacs Speaks Statistics
(use-package ess
  :ensure t
  :mode (("\\.r\\'"	 . R-mode)
		 ("\\.R\\'"	 . R-mode)
		 ("\\.jl\\'" . julia-mode))
  :commands (ess-r-devtools-load-package
			 ess-display-package-index
			 ess-r-devtools-document-package
			 ess-install-library
			 ess-install.packages
			 ess-r-devtools-install-package
			 ess-r-package-set-package
			 ess-r-devtools-unload-package
			 ess-r-devtools-test-package
			 ess-r-devtools-check-package
			 ess-r-devtools-install-github
			 ess-r-devtools-revdep-check-cmd
			 ess-r-devtools-revdep-check-package
			 R
			 julia
			 ess-set-working-directory
			 ess-install-library
			 ess-load-library
			 ess-execute
			 ess-switch-to-ESS
			 ess-eval-region-or-function-or-paragraph-and-step
			 ess-eval-buffer
			 ess-eval-buffer-from-beg-to-here
			 ess-eval-buffer-from-here-to-end
			 ess-eval-chunk-and-step
			 ess-show-call-stack
			 ess-load-file
			 ess-r-load-file-namespaced
			 ess-watch
			 ess-eval-region-or-line-and-step
			 ess-load-library
			 ess-help
			 ess-describe-object-at-point
			 ess-display-demos
			 ess-display-vignettes
			 ess-help-web-search
			 ess-display-help-apropos)

  :init
  (setq ess-use-ido nil)

  :config
  (require 'ess-site)

  ;; open R in a split window
  (defun sk/R ()
	"open R REPL in other window"
	(interactive)
	(split-window-horizontally)
	(other-window 1)
	(R))

  ;; open julia in a split window
  (defun sk/julia ()
	"open julia REPL in other window"
	(interactive)
	(split-window-horizontally)
	(other-window 1)
	(julia))

  ;; top level bindings
  (ryo-modal-key "m r" 'sk/R :mode 'ess-mode)
  (ryo-modal-key "m j" 'sk/julia :mode 'ess-mode)
  (ryo-modal-key "m z" 'ess-switch-to-ESS :mode 'ess-mode)
  (ryo-modal-key "m m" 'ess-load-file :mode 'ess-mode)
  (ryo-modal-key "m l" 'ess-load-library :mode 'ess-mode)
  (ryo-modal-key "m n" 'ess-r-load-file-namespaced :mode 'ess-mode)
  (ryo-modal-key "m x" 'ess-dump-object-into-edit-buffer :mode 'ess-mode)
  (ryo-modal-key "m w" 'ess-describe-object-at-point :mode 'ess-mode)
  (ryo-modal-key "m c" 'ess-execute :mode 'ess-mode)
  (ryo-modal-key "m q" 'ess-set-style :mode 'ess-mode)
  (ryo-modal-key "m /" 'ess-set-working-directory :mode 'ess-mode)
  (ryo-modal-key "m i" 'ess-install-library :mode 'ess-mode)
  (ryo-modal-key "m o" 'ess-execute :mode 'ess-mode)

  ;; documentation bindings
  (ryo-modal-key "m d d" 'ess-help :mode 'ess-mode)
  (ryo-modal-key "m d e" 'ess-display-demos :mode 'ess-mode)
  (ryo-modal-key "m d v" 'ess-display-vignettes :mode 'ess-mode)
  (ryo-modal-key "m d s" 'ess-help-web-search :mode 'ess-mode)
  (ryo-modal-key "m d a" 'ess-display-help-apropos :mode 'ess-mode)

  ;; operator/textobject
  (add-hook 'ess-mode-hook
		  (sk/ryo-operator-object ess-eval "m s" "s" ess-eval-region t ess-mode))

  ;; installing packages
  (ryo-modal-key "m p x" 'ess-display-package-index :mode 'ess-mode)
  (ryo-modal-key "m p l" 'ess-install-library :mode 'ess-mode)
  (ryo-modal-key "m p p" 'ess-install.packages :mode 'ess-mode)
  (ryo-modal-key "m p i" 'ess-r-devtools-install-package :mode 'ess-mode)
  (ryo-modal-key "m p a" 'ess-r-devtools-load-package :mode 'ess-mode)
  (ryo-modal-key "m p d" 'ess-r-devtools-document-package :mode 'ess-mode)
  (ryo-modal-key "m p s" 'ess-r-package-set-package :mode 'ess-mode)
  (ryo-modal-key "m p u" 'ess-r-devtools-unload-package :mode 'ess-mode)
  (ryo-modal-key "m p t" 'ess-r-devtools-test-package :mode 'ess-mode)
  (ryo-modal-key "m p g" 'ess-r-devtools-install-github :mode 'ess-mode)
  (ryo-modal-key "m p e" 'ess-r-devtools-check-package :mode 'ess-mode)
  (ryo-modal-key "m p c" 'ess-r-devtools-revdep-check-cmd :mode 'ess-mode)
  (ryo-modal-key "m p k" 'ess-r-devtools-revdep-check-package :mode 'ess-mode)

  ;; ;; roxy bindings
  ;; (ryo-modal-key "m y j" 'ess-roxy-next-entry :mode 'ess-mode)
  ;; (ryo-modal-key "m y k" 'ess-roxy-previous-entry :mode 'ess-mode)
  ;; (ryo-modal-key "m y u" 'ess-roxy-update-entry :mode 'ess-mode)
  ;; (ryo-modal-key "m y t" 'ess-roxy-toggle-roxy-region :mode 'ess-mode)
  ;; (ryo-modal-key "m y a" 'ess-roxy-show-all :mode 'ess-mode)
  ;; (ryo-modal-key "m y c" 'ess-roxy-cycle-example :mode 'ess-mode)
  ;; (ryo-modal-key "m y p" 'ess-roxy-preview-HTML :mode 'ess-mode)
  ;; (ryo-modal-key "m y x" 'ess-roxy-preview-text :mode 'ess-mode)
  ;; (ryo-modal-key "m y r" 'ess-roxy-preview-Rd :mode 'ess-mode)

  ;; debug bindings
  (ryo-modal-key "m g s" 'ess-bp-set :mode 'ess-mode)
  (ryo-modal-key "m g u" 'ess-bp-kill :mode 'ess-mode)
  (ryo-modal-key "m g k" 'ess-bp-kill-all :mode 'ess-mode)
  (ryo-modal-key "m g l" 'ess-bp-set-logger :mode 'ess-mode)
  (ryo-modal-key "m g c" 'ess-bp-set-conditional :mode 'ess-mode)
  ;; (ryo-modal-key "m g v" 'ess-r-set-evaluation-env :mode 'ess-mode)
  (ryo-modal-key "m g n" 'ess-bp-next :mode 'ess-mode)
  (ryo-modal-key "m g p" 'ess-bp-previous :mode 'ess-mode)
  (ryo-modal-key "m g t" 'ess-bp-toggle-state :mode 'ess-mode)
  (ryo-modal-key "m g w" 'ess-watch :mode 'ess-mode)
  (ryo-modal-key "m g a" 'ess-show-call-stack :mode 'ess-mode)
  (ryo-modal-key "m g e" 'ess-toggle-tracebug :mode 'ess-mode)
  (ryo-modal-key "m g b" 'ess-tracebug-show-help :mode 'ess-mode)
  (ryo-modal-key "m g f" 'ess-debug-flag-for-debugging :mode 'ess-mode)
  (ryo-modal-key "m g d" 'ess-debug-unflag-for-debugging :mode 'ess-mode)
  (ryo-modal-key "m g i" 'ess-debug-goto-input-event-marker :mode 'ess-mode)
  (ryo-modal-key "m g r" 'ess-debug-toggle-error-action :mode 'ess-mode)
  (ryo-modal-key "m g o" 'ess-show-traceback :mode 'ess-mode))

(defun sk/ess-setup ()
  "Some bindings don't get evaluated in use-package config. Also, enable ryomodal mode"
  (interactive)
  ;; roxy bindings
  (ryo-modal-key "m y j" 'ess-roxy-next-entry :mode 'ess-mode)
  (ryo-modal-key "m y k" 'ess-roxy-previous-entry :mode 'ess-mode)
  (ryo-modal-key "m y u" 'ess-roxy-update-entry :mode 'ess-mode)
  (ryo-modal-key "m y t" 'ess-roxy-toggle-roxy-region :mode 'ess-mode)
  (ryo-modal-key "m y a" 'ess-roxy-show-all :mode 'ess-mode)
  (ryo-modal-key "m y c" 'ess-roxy-cycle-example :mode 'ess-mode)
  (ryo-modal-key "m y p" 'ess-roxy-preview-HTML :mode 'ess-mode)
  (ryo-modal-key "m y x" 'ess-roxy-preview-text :mode 'ess-mode)
  (ryo-modal-key "m y r" 'ess-roxy-preview-Rd :mode 'ess-mode)
  ;; debugging
  (ryo-modal-key "m g v" 'ess-r-set-evaluation-env :mode 'ess-mode))
(add-hook 'ess-mode-hook 'sk/ess-setup)
(add-hook 'ess-mode-hook 'sk/enable-ryo-modal-mode)

;; major mode bindings
(which-key-add-major-mode-key-based-replacements 'ess-mode

  "m j" "run julia"
  "m r" "run R"
  "m z" "back to shell"
  "m m" "load file"
  "m l" "load library"
  "m n" "load namespaced file"
  "m w" "describe object"
  "m c" "execute command"
  "m x" "dump object"
  "m q" "set style"
  "m /" "set dir"
  "m i" "install library"
  "m o" "execute"

  ;; documentation
  "m d" "documentation"
  "m d d" "documentation"
  "m d v" "vignettes"
  "m d e" "examples"
  "m d s" "search web"
  "m d a" "apropos"

  ;; packages
  "m p" "packages"
  "m p x" "index"
  "m p l" "install library"
  "m p p" "install package"
  "m p i" "devtools install package"
  "m p a" "devtools load package"
  "m p d" "devtools document package"
  "m p s" "set package"
  "m p u" "devtools unload package"
  "m p t" "devtools test package"
  "m p g" "devtools github package"
  "m p e" "devtools check package"
  "m p c" "devtools revdep check cmd"
  "m p k" "devtools revdep check package"

  ;; debugging
  "m g" "debug"
  "m g s" "set breakpoint"
  "m g u" "kill breakpoint"
  "m g k" "kill all breakpoint"
  "m g l" "set logger"
  "m g c" "set conditional"
  "m g v" "set evaluation env"
  "m g n" "next breakpoint"
  "m g p" "previous breakpoint"
  "m g t" "toggle state"
  "m g w" "watch"
  "m g a" "show call stack"
  "m g e" "toggle tracebug"
  "m g b" "tracebug show help"
  "m g f" "flag for debugging"
  "m g d" "unflag for debugging"
  "m g i" "goto input event marker"
  "m g r" "toggle error action"
  "m g o" "show traceback"

  ;; pdb
  "m y" "roxy"
  "m y j" "next"
  "m y k" "previous"
  "m y u" "update"
  "m y t" "toggle"
  "m y a" "show all"
  "m y c" "cycle example"
  "m y p" "preview html"
  "m y x" "preview text"
  "m y r" "preview Rd"

  ;; send
  "m s" "send"
  "m s s" "region"
  "m s i" "inside"
  "m s a" "around"
  "m s g" "global"
  "m s i a" "all"
  "m s a a" "all"
  "m s i w" "word"
  "m s a w" "word"
  "m s i p" "para"
  "m s a p" "para"
  "m s i s" "sentence"
  "m s a s" "sentence"
  "m s i l" "line"
  "m s a l" "line"
  "m s i y" "symbol"
  "m s a y" "symbol"
  "m s i c" "comment"
  "m s a c" "comment"
  "m s i f" "function"
  "m s a f" "function"
  "m s i q" "quote"
  "m s a q" "quote"
  "m s i b" "block/pairs"
  "m s a b" "block/pairs"
  "m s i h" "diff hunk"
  "m s a h" "diff hunk"
  "m s i x" "latex section"
  "m s a x" "latex section"
  "m s i o" "org code"
  "m s a o" "org code"
  "m s i u" "org subtree"
  "m s a u" "org subtree"
  "m s i e" "latex env"
  "m s a e" "latex env"
  "m s i r" "method call"
  "m s a r" "method call"
  "m s i d" "ruby block"
  "m s a d" "ruby block"
  "m s i g" "python string"
  "m s a g" "python string"
  "m s i m" "python block"
  "m s a m" "python block"
  "m s i n" "python statement"
  "m s a n" "python block and dec"
  "m s i $" "latex math"
  "m s a $" "latex math"
  "m s f" "to char"
  "m s F" "to char back"
  "m s t" "till char"
  "m s T" "till char back"
  "m s ;" "find on screen"
  "m s g ;" "till line"
  "m s h" "prev char"
  "m s j" "next line"
  "m s k" "prev line"
  "m s l" "next char"
  "m s 0" "till start of line"
  "m s $" "till end of line"
  "m s {" "till start of para"
  "m s }" "till end of para"
  "m s (" "till start of sentence"
  "m s )" "till end of sentence"
  "m s e" "end of word"
  "m s b" "start of word"
  "m s g g" "start of buffer"
  "m s G" "end of buffer"
  "m s c" "chunk")

;; provide the stats configuration
(provide 'sk-stats)
