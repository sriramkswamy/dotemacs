(use-package macrostep
  :ensure t)

(defun sk/elisp-debug ()
  "calls edebug instead of using C-u C-M-x"
  (interactive)
  (let ((current-prefix-arg 4)) ;; emulate C-u
	(call-interactively 'eval-defun)))

;; edebug
(use-package edebug
  :commands (edebug-Trace-fast-mode
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

;; elisp bindings
(ryo-modal-key "m e" 'macrostep-expand :mode 'emacs-lisp-mode)
(ryo-modal-key "m c" 'macrostep-collapse :mode 'emacs-lisp-mode)
(ryo-modal-key "m a" 'macrostep-collapse-all :mode 'emacs-lisp-mode)
(ryo-modal-key "m n" 'macrostep-next-macro :mode 'emacs-lisp-mode)
(ryo-modal-key "m p" 'macrostep-prev-macro :mode 'emacs-lisp-mode)
(ryo-modal-key "m j" 'find-function :mode 'emacs-lisp-mode)
(ryo-modal-key "m d" 'describe-function :mode 'emacs-lisp-mode)
(ryo-modal-key "m v" 'find-variable :mode 'emacs-lisp-mode)
(ryo-modal-key "m w" 'describe-variable :mode 'emacs-lisp-mode)
(ryo-modal-key "m l" 'find-library :mode 'emacs-lisp-mode)
(ryo-modal-key "m x" 'eval-last-sexp :mode 'emacs-lisp-mode)
(ryo-modal-key "m i" 'eval-print-last-sexp :mode 'emacs-lisp-mode)
(ryo-modal-key "m r" 'sk/ielm :mode 'emacs-lisp-mode)

;; debugging
(ryo-modal-key "m g g" 'sk/elisp-debug :mode 'emacs-lisp-mode)
(ryo-modal-key "m g o" 'edebug-go-mode :mode 'emacs-lisp-mode)
(ryo-modal-key "m g s" 'edebug-set-breakpoint :mode 'emacs-lisp-mode)
(ryo-modal-key "m g u" 'edebug-unset-breakpoint :mode 'emacs-lisp-mode)
(ryo-modal-key "m g b" 'edebug-next-breakpoint :mode 'emacs-lisp-mode)
(ryo-modal-key "m g c" 'edebug-continue-mode :mode 'emacs-lisp-mode)
(ryo-modal-key "m g n" 'edebug-step-mode :mode 'emacs-lisp-mode)
(ryo-modal-key "m g l" 'edebug-display-freq-count :mode 'emacs-lisp-mode)
(ryo-modal-key "m g t" 'edebug-trace-mode :mode 'emacs-lisp-mode)
(ryo-modal-key "m g q" 'edebug-top-level :mode 'emacs-lisp-mode)
(ryo-modal-key "m g w" 'edebug-where :mode 'emacs-lisp-mode)
(ryo-modal-key "m g f c" 'edebug-Continue-fast-mode :mode 'emacs-lisp-mode)
(ryo-modal-key "m g f g" 'edebug-Go-nonstop-mode :mode 'emacs-lisp-mode)
(ryo-modal-key "m g f q" 'edebug-top-level-nonstop :mode 'emacs-lisp-mode)
(ryo-modal-key "m g f t" 'edebug-Trace-fast-mode :mode 'emacs-lisp-mode)

;; operator/textobject bindings
(add-hook 'emacs-lisp-mode-hook
		  (sk/ryo-operator-object eval-region "m s" "s" eval-region nil emacs-lisp-mode))

;; lisp interaction mode bindings
(ryo-modal-key "m e" 'macrostep-expand :mode 'lisp-interaction-mode)
(ryo-modal-key "m c" 'macrostep-collapse :mode 'lisp-interaction-mode)
(ryo-modal-key "m a" 'macrostep-collapse-all :mode 'lisp-interaction-mode)
(ryo-modal-key "m n" 'macrostep-next-macro :mode 'lisp-interaction-mode)
(ryo-modal-key "m p" 'macrostep-prev-macro :mode 'lisp-interaction-mode)
(ryo-modal-key "m j" 'find-function :mode 'lisp-interaction-mode)
(ryo-modal-key "m d" 'describe-function :mode 'lisp-interaction-mode)
(ryo-modal-key "m v" 'find-variable :mode 'lisp-interaction-mode)
(ryo-modal-key "m w" 'describe-variable :mode 'lisp-interaction-mode)
(ryo-modal-key "m l" 'find-library :mode 'lisp-interaction-mode)
(ryo-modal-key "m x" 'eval-last-sexp :mode 'lisp-interaction-mode)
(ryo-modal-key "m i" 'eval-print-last-sexp :mode 'lisp-interaction-mode)
(ryo-modal-key "m r" 'sk/ielm :mode 'lisp-interaction-mode)

;; debugging
(ryo-modal-key "m g g" 'sk/elisp-debug :mode 'lisp-interaction-mode)
(ryo-modal-key "m g o" 'edebug-go-mode :mode 'lisp-interaction-mode)
(ryo-modal-key "m g s" 'edebug-set-breakpoint :mode 'lisp-interaction-mode)
(ryo-modal-key "m g u" 'edebug-unset-breakpoint :mode 'lisp-interaction-mode)
(ryo-modal-key "m g b" 'edebug-next-breakpoint :mode 'lisp-interaction-mode)
(ryo-modal-key "m g c" 'edebug-continue-mode :mode 'lisp-interaction-mode)
(ryo-modal-key "m g n" 'edebug-step-mode :mode 'lisp-interaction-mode)
(ryo-modal-key "m g l" 'edebug-display-freq-count :mode 'lisp-interaction-mode)
(ryo-modal-key "m g t" 'edebug-trace-mode :mode 'lisp-interaction-mode)
(ryo-modal-key "m g q" 'edebug-top-level :mode 'lisp-interaction-mode)
(ryo-modal-key "m g w" 'edebug-where :mode 'lisp-interaction-mode)
(ryo-modal-key "m g f c" 'edebug-Continue-fast-mode :mode 'lisp-interaction-mode)
(ryo-modal-key "m g f g" 'edebug-Go-nonstop-mode :mode 'lisp-interaction-mode)
(ryo-modal-key "m g f q" 'edebug-top-level-nonstop :mode 'lisp-interaction-mode)
(ryo-modal-key "m g f t" 'edebug-Trace-fast-mode :mode 'lisp-interaction-mode)

;; operator/textobject bindings
(add-hook 'lisp-interaction-mode-hook
		  (sk/ryo-operator-object lisp-eval-region "m s" "s" eval-region nil lisp-interaction-mode))

;; ryo major mode hints
(which-key-add-major-mode-key-based-replacements 'emacs-lisp-mode

  "m e" "macro expand"
  "m c" "macro collapse"
  "m a" "macro collapse all"
  "m n" "next macro"
  "m p" "previous macro"
  "m j" "find function"
  "m d" "describe function"
  "m v" "find variable"
  "m w" "describe variable"
  "m k" "describe library"
  "m f" "eval defun"
  "m b" "eval buffer"
  "m l" "library"
  "m i" "eval print sexp"
  "m r" "run shell"
  "m x" "eval expression"

  ;; debug
  "m g" "debug"
  "m g g" "start"
  "m g o" "go mode"
  "m g s" "set breakpoint"
  "m g u" "unset breakpoint"
  "m g b" "next breakpoint"
  "m g c" "continue"
  "m g l" "frequency count"
  "m g n" "step"
  "m g t" "trace"
  "m g q" "top level"
  "m g w" "where"
  "m g f" "fast mode"
  "m g f c" "continue"
  "m g f g" "go mode"
  "m g f q" "top level"
  "m g f t" "trace"

  ;; send
  "m s" "eval"
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
  "m s G" "end of buffer")

;; ryo major mode hints
(which-key-add-major-mode-key-based-replacements 'lisp-interaction-mode

  "m e" "macro expand"
  "m c" "macro collapse"
  "m a" "macro collapse all"
  "m n" "next macro"
  "m p" "previous macro"
  "m j" "find function"
  "m d" "describe function"
  "m v" "find variable"
  "m w" "describe variable"
  "m k" "describe library"
  "m f" "eval defun"
  "m b" "eval buffer"
  "m l" "library"
  "m i" "eval print sexp"
  "m r" "run shell"
  "m x" "eval expression"

  ;; debug
  "m g" "debug"
  "m g g" "start"
  "m g o" "go mode"
  "m g s" "set breakpoint"
  "m g u" "unset breakpoint"
  "m g b" "next breakpoint"
  "m g c" "continue"
  "m g l" "frequency count"
  "m g n" "step"
  "m g t" "trace"
  "m g q" "top level"
  "m g w" "where"
  "m g f" "fast mode"
  "m g f c" "continue"
  "m g f g" "go mode"
  "m g f q" "top level"
  "m g f t" "trace"

  ;; send
  "m s" "eval"
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
  "m s G" "end of buffer")

;; provide elisp configuration
(provide 'sk-elisp)
