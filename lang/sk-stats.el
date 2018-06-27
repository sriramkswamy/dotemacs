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

  ;; ESS operator bindings
  (defun sk/ess-operator ()
	"operator text object bindings for ess"
	(interactive)
	(eval `(ryo-modal-major-mode-keys
			'ess-mode
			("m s" ,text-objects :then '(ess-eval-region)))))

  ;; top level bindings
  (ryo-modal-major-mode-keys
   'ess-mode
   ;; general bindings
   ("m r" sk/R :name "start R")
   ("m j" sk/julia :name "start julia")
   ("m z" ess-switch-to-ESS :name "switch to shell")
   ("m m" ess-load-file :name "load file")
   ("m l" ess-load-library :name "load library")
   ("m n" ess-r-load-file-namespaced :name "load file namespaced")
   ("m x" ess-dump-object-into-edit-buffer :name "dump object to edit")
   ("m w" ess-describe-object-at-point :name "describe object")
   ("m c" ess-execute :name "execute command")
   ("m q" ess-set-style :name "set style")
   ("m /" ess-set-working-directory :name "set working directory")
   ("m i" ess-install-library :name "install library")
   ("m o" ess-execute :name "output")
   ;; documentation bindings
   ("m d d" ess-help :name "doc help")
   ("m d e" ess-display-demos :name "demos")
   ("m d v" ess-display-vignettes :name "vignettes")
   ("m d s" ess-help-web-search :name "search")
   ("m d a" ess-display-help-apropos :name "apropos")
   ;; installing packages
   ("m p x" ess-display-package-index :name "index")
   ("m p l" ess-install-library :name "library")
   ("m p p" ess-install.packages :name "packages")
   ("m p i" ess-r-devtools-install-package :name "install devtools")
   ("m p a" ess-r-devtools-load-package :name "devtools load")
   ("m p d" ess-r-devtools-document-package :name "devtools document")
   ("m p s" ess-r-package-set-package :name "set package")
   ("m p u" ess-r-devtools-unload-package :name "devtools unload")
   ("m p t" ess-r-devtools-test-package :name "devtools test package")
   ("m p g" ess-r-devtools-install-github :name "devtools install github")
   ("m p e" ess-r-devtools-check-package :name "devtools check package")
   ("m p c" ess-r-devtools-revdep-check-cmd :name "devtools revdep check command")
   ("m p k" ess-r-devtools-revdep-check-package :name "devtools revdep check package")

   ;; ;; roxy bindings
   ;; ("m y j" ess-roxy-next-entry :name "next entry")
   ;; ("m y k" ess-roxy-previous-entry :name "previous entry")
   ;; ("m y u" ess-roxy-update-entry :name "update entry")
   ;; ("m y t" ess-roxy-toggle-roxy-region :name "toggle roxy region")
   ;; ("m y a" ess-roxy-show-all :name "show all")
   ;; ("m y c" ess-roxy-cycle-example :name "cycle example")
   ;; ("m y p" ess-roxy-preview-HTML :name "preview html")
   ;; ("m y x" ess-roxy-preview-text :name "preview text")
   ;; ("m y r" ess-roxy-preview-Rd :name "preview Rd")

   ;; debug bindings
   ;; ("m g v" ess-r-set-evaluation-env :name "set eval environment")
   ("m g s" ess-bp-set :name "set breakpoint")
   ("m g u" ess-bp-kill :name "unset breakpoint")
   ("m g k" ess-bp-kill-all :name "kill all breakpoints")
   ("m g l" ess-bp-set-logger :name "set log breakpoint")
   ("m g c" ess-bp-set-conditional :name "set conditional breakpoint")
   ("m g n" ess-bp-next :name "next breakpoint")
   ("m g p" ess-bp-previous :name "previous breakpoint")
   ("m g t" ess-bp-toggle-state :name "toggle breakpoint")
   ("m g w" ess-watch :name "watch")
   ("m g a" ess-show-call-stack :name "call stack")
   ("m g e" ess-toggle-tracebug :name "toggle tracebug")
   ("m g b" ess-tracebug-show-help :name "tracebug show help")
   ("m g f" ess-debug-flag-for-debugging :name "flag for debugging")
   ("m g d" ess-debug-unflag-for-debugging :name "unflag for debugging")
   ("m g i" ess-debug-goto-input-event-marker :name "goto input event marker")
   ("m g r" ess-debug-toggle-error-action :name "toggle error action")
   ("m g o" ess-show-traceback :name "show traceback")))

(add-hook 'ess-mode-hook 'sk/enable-ryo-modal-mode)
(add-hook 'ess-mode-hook 'sk/ess-operator)

;; major mode bindings
(which-key-add-major-mode-key-based-replacements 'ess-mode
  "m d" "documentation"
  "m p" "packages"
  "m g" "debug"
  "m y" "roxy"
  "m s" "send")

;; provide the stats configuration
(provide 'sk-stats)
