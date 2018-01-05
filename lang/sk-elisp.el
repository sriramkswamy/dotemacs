;; split ielm
(defun sk/ielm ()
  "open ielm REPL in a split window"
  (interactive)
  (split-window-horizontally)
  (ielm)
  (other-window 1))

(defun sk/elisp-debug ()
  "calls edebug instead of using C-u C-M-x"
  (interactive)
  (let ((current-prefix-arg 4)) ;; emulate C-u
	(call-interactively 'eval-defun)))

;; edebug
(use-package edebug
  :commands
  (edebug-Trace-fast-mode
   edebug-top-level-nonstop
   edebug-Go-nonstop-mode
   edebug-Continue-fast-mode
   edebug-where
   edebug-top-level
   edebug-trace-mode
   edebug-go-mode
   edebug-set-breakpoint
   edebug-unset-breakpoint
   edebug-next-breakpoint
   edebug-continue-mode
   edebug-step-mode
   edebug-display-freq-count))

(use-package macrostep
  :ensure t
  :commands
  (macrostep-expand
   macrostep-collapse
   macrostep-collapse-all
   macrostep-next-macro
   macrostep-prev-macro))

(use-package emacs-lisp-mode
  :hook ((emacs-lisp-mode . sk/company-elisp)))

(use-package lisp-interaction-mode
  :hook ((lisp-interaction-mode . sk/company-elisp)))

(ryo-modal-major-mode-keys
 'emacs-lisp-mode
 ;; macrostep
 ("m e" macrostep-expand :name "macro expand")
 ("m c" macrostep-collapse :name "macro collapse")
 ("m a" macrostep-collapse-all :name "macro collapse all")
 ("m n" macrostep-next-macro :name "macro next")
 ("m p" macrostep-prev-macro :name "macro previous")

 ;; semantic navigation
 ("m j" find-function :name "jump to function")
 ("m d" describe-function :name "doc of function")
 ("m v" find-variable :name "jump to variable")
 ("m w" describe-variable :name "doc of variable")
 ("m l" find-library :name "jump to library")
 ("m x" eval-last-sexp :name "eval last expression")
 ("m i" eval-print-last-sexp :name "eval and print last expression")
 ("m r" sk/ielm :name "run repl")
 
 ;; eval
 ("m s s" eval-region :name "eval region")
 
 ;; debugging
 ("m g g" sk/elisp-debug :name "start debugging")
 ("m g o" edebug-go-mode :name "go")
 ("m g s" edebug-set-breakpoint :name "set breakpoint")
 ("m g x" edebug-unset-breakpoint :name "delete breakpoint")
 ("m g b" edebug-next-breakpoint :name "next breakpoint")
 ("m g c" edebug-continue-mode :name "continue")
 ("m g n" edebug-step-mode :name "step next")
 ("m g l" edebug-display-freq-count :name "list frequency")
 ("m g t" edebug-trace-mode :name "trace")
 ("m g q" edebug-top-level :name "quit")
 ("m g w" edebug-where :name "where am i")
 ("m g f c" edebug-Continue-fast-mode :name "fast continue")
 ("m g f g" edebug-Go-nonstop-mode :name "fast nonstop")
 ("m g f q" edebug-top-level-nonstop :name "fast quit")
 ("m g f t" edebug-Trace-fast-mode :name "fast trace"))

(ryo-modal-major-mode-keys
 'lisp-interaction-mode
 ;; macrostep
 ("m e" macrostep-expand :name "macro expand")
 ("m c" macrostep-collapse :name "macro collapse")
 ("m a" macrostep-collapse-all :name "macro collapse all")
 ("m n" macrostep-next-macro :name "macro next")
 ("m p" macrostep-prev-macro :name "macro previous")

 ;; semantic navigation
 ("m j" find-function :name "jump to function")
 ("m d" describe-function :name "doc of function")
 ("m v" find-variable :name "jump to variable")
 ("m w" describe-variable :name "doc of variable")
 ("m l" find-library :name "jump to library")
 ("m x" eval-last-sexp :name "eval last expression")
 ("m i" eval-print-last-sexp :name "eval and print last expression")
 ("m r" sk/ielm :name "run repl")
 
 ;; eval
 ("m s s" eval-region :name "eval region")
 
 ;; debugging
 ("m g g" sk/elisp-debug :name "start debugging")
 ("m g o" edebug-go-mode :name "go")
 ("m g s" edebug-set-breakpoint :name "set breakpoint")
 ("m g x" edebug-unset-breakpoint :name "delete breakpoint")
 ("m g b" edebug-next-breakpoint :name "next breakpoint")
 ("m g c" edebug-continue-mode :name "continue")
 ("m g n" edebug-step-mode :name "step next")
 ("m g l" edebug-display-freq-count :name "list frequency")
 ("m g t" edebug-trace-mode :name "trace")
 ("m g q" edebug-top-level :name "quit")
 ("m g w" edebug-where :name "where am i")
 ("m g f c" edebug-Continue-fast-mode :name "fast continue")
 ("m g f g" edebug-Go-nonstop-mode :name "fast nonstop")
 ("m g f q" edebug-top-level-nonstop :name "fast quit")
 ("m g f t" edebug-Trace-fast-mode :name "fast trace"))

;; which key hints
(which-key-add-major-mode-key-based-replacements 'emacs-lisp-mode
  "m g" "debug"
  "m g f" "fast"
  
  "m s" "eval"
  "m s i" "inside"
  "m s a" "around"
  "m s g" "global")

;; which key hints
(which-key-add-major-mode-key-based-replacements 'lisp-interaction-mode
  "m g" "debug"
  "m g f" "fast"
  
  "m s" "eval"
  "m s i" "inside"
  "m s a" "around")

;; provide elisp configuration
(provide 'sk-elisp)
