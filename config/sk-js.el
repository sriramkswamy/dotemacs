;; Make sure node's prompt displays correctly
;; http://stackoverflow.com/questions/9390770/node-js-prompt-can-not-show-in-eshell
(setenv "NODE_NO_READLINE" "1")

;; functions for nodejs repl
(defun sk/nodejs-execute (arg)
  "execute the given statement in the nodejs repl"
  (interactive
   (list
	(read-string "Execute: ")))
  (bookmark-set "nodejs")
  (let* ((sk/string-to-send (concat arg "\n")))
	(nodejs-repl-switch-to-repl)
	(delete-other-windows)
	(comint-send-string (get-buffer-process (current-buffer)) sk/string-to-send)
	(split-window-horizontally))
  (bookmark-jump "nodejs"))

;; better repl
(defun sk/nodejs-repl ()
  "open nodejs REPL in a split window"
  (interactive)
  (bookmark-set "nodejs")
  (nodejs-repl)
  (other-window 1)
  (bookmark-jump "nodejs"))

;; use js3-mode instead of the built-in js-mode
(use-package js3-mode
  :ensure t
  :mode ("\\.js\\'" . js3-mode)
  :config

  (use-package nodejs-repl
	:ensure t
	:commands (nodejs-repl
			   nodejs-repl-send-buffer
			   nodejs-repl-switch-to-repl
			   nodejs-repl-send-region
			   nodejs-repl-send-last-sexp
			   nodejs-repl-load-file))

  ;; bindings
  (ryo-modal-key "m r" 'sk/nodejs-repl :mode 'js3-mode)
  (ryo-modal-key "m z" 'nodejs-repl-switch-to-repl :mode 'js3-mode)
  (ryo-modal-key "m m" 'nodejs-repl-load-file :mode 'js3-mode)
  (ryo-modal-key "m c" 'sk/nodejs-execute :mode 'js3-mode)

  ;; operator/textobject sending
  (ryo-modal-key "m s s" 'nodejs-repl-send-region :mode 'js3-mode)
  (ryo-modal-key "m s i a" 'nodejs-repl-send-buffer :mode 'js3-mode)
  (ryo-modal-key "m s a a" 'nodejs-repl-send-buffer :mode 'js3-mode)
  (ryo-modal-key "m s i w" 'er/mark-word :then '(nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s a w" 'sk/mark-around-word :then '(nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s i k" 'sk/mark-inside-markdown-code-block :then '(nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s a k" 'sk/mark-around-markdown-code-block :then '(nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s i p" 'er/mark-text-paragraph :then '(nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s a p" 'mark-paragraph :then '(nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s i i" 'indent-tools-select :then '(nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s a i" 'sk/select-indent-tree :then '(nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s i s" 'er/mark-text-sentence :then '(nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s a s" 'er/mark-text-sentence :then '(nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s i l" 'sk/select-inside-line :then '(nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s a l" 'sk/select-around-line :then '(nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s i y" 'er/mark-symbol :then '(nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s a y" 'sk/mark-around-symbol :then '(nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s i c" 'er/mark-comment :then '(nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s a c" 'er/mark-comment :then '(nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s i f" 'er/mark-defun :then '(nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s a f" 'er/mark-defun :then '(nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s i h" 'diff-hl-mark-hunk :then '(nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s a h" 'diff-hl-mark-hunk :then '(nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s i q" 'er/mark-inside-quotes :then '(nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s a q" 'er/mark-outside-quotes :then '(nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s i b" 'er/mark-inside-pairs :then '(nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s a b" 'er/mark-outside-pairs :then '(nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s i o" 'sk/mark-inside-org-code :then '(nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s a o" 'er/mark-org-code-block :then '(nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s i u" 'sk/mark-inside-subtree :then '(nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s a u" 'org-mark-subtree :then '(nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s i e" 'er/mark-LaTeX-inside-environment :then '(nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s a e" 'LaTeX-mark-environment :then '(nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s i r" 'er/mark-method-call :then '(nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s a r" 'er/mark-method-call :then '(nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s i d" 'sk/mark-inside-ruby-block :then '(nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s a d" 'er/mark-method-call :then '(nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s i g" 'er/mark-inside-python-string :then '(nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s a g" 'er/mark-outside-python-string :then '(nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s i m" 'sk/mark-inside-python-block :then '(nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s a m" 'er/mark-outer-python-block :then '(nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s i n" 'er/mark-python-statement :then '(nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s a n" 'er/mark-python-block-and-decorator :then '(nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s i $" 'er/mark-LaTeX-math :then '(nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s a $" 'sk/mark-inside-LaTeX-math :then '(nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s i x" 'LaTeX-mark-section :then '(nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s a x" 'LaTeX-mark-section :then '(nodejs-repl-send-region) :mode 'js3-mode)

  (ryo-modal-key "m s ;" 'set-mark-command :then '(avy-resume
												   nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s SPC a" 'set-mark-command :then '(avy-goto-line
													 nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s f" 'set-mark-command :then '(avy-goto-char-in-line
												   nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s F" 'set-mark-command :then '(avy-goto-char-2
												   nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s t" 'set-mark-command :then '(avy-goto-char-2-below
												   nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s T" 'set-mark-command :then '(avy-goto-char-2-above
												   nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s h" 'set-mark-command :then '(backward-char
												   nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s j" 'set-mark-command :then '(next-line
												   move-end-of-line
												   exchange-point-and-mark
												   move-beginning-of-line
												   nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s k" 'set-mark-command :then '(move-end-of-line
												   exchange-point-and-mark
												   previous-line
												   move-beginning-of-line
												   nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s l" 'set-mark-command :then '(forward-char
												   nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s 0" 'set-mark-command :then '(sk/smarter-move-beginning-of-line
												   nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s $" 'set-mark-command :then '(move-end-of-line
												   nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s g g" 'set-mark-command :then '(beginning-of-buffer
													 nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s G" 'set-mark-command :then '(end-of-buffer
												   nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s {" 'set-mark-command :then '(backward-paragraph
												   nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s }" 'set-mark-command :then '(forward-paragraph
												   nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s (" 'set-mark-command :then '(backward-sentence
												   nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s )" 'set-mark-command :then '(forward-sentence
												   nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s e" 'set-mark-command :then '(forward-word
												   nodejs-repl-send-region) :mode 'js3-mode)
  (ryo-modal-key "m s b" 'set-mark-command :then '(backward-word
												   nodejs-repl-send-region) :mode 'js3-mode)

  ;; enable ryo-modal-mode
  (sk/enable-ryo-modal-mode))
(add-hook 'js3-mode-hook 'sk/enable-ryo-modal-mode)

;; ryo major mode
(which-key-add-major-mode-key-based-replacements 'js3-mode

  "m j" "find def"
  "m d" "find doc"
  "m b" "go back"
  "m r" "run nodejs"
  "m z" "back to nodejs"
  "m m" "send file"
  "m c" "last sexp"

  ;; functions
  "m u" "clear"
  "m a" "list"
  "m f" "plot"
  "m i" "shape"
  "m x" "size"
  "m o" "dimensions"
  "m l" "length"
  "m w" "type"
  "m =" "sum"
  "m +" "cumulative sum"
  "m e" "mean"
  "m a l" "locals"
  "m a g" "globals"
  "m a d" "dir"
  "m a a" "all"
  "m f p" "plot variable"

  ;; virtualenv
  "m v" "environment"
  "m v w" "workon"
  "m v d" "deactivate"
  "m v t" "target location"
  "m v l" "list envs"
  "m v o" "open env"
  "m v r" "remove env"
  "m v y" "copy env"
  "m v s" "set pyvenv"
  "m v u" "unset pyvenv"

  ;; debugging
  "m g" "debug"
  "m g g" "ipdb"

  ;; tests
  "m t" "test"
  "m t o" "one"
  "m t a" "all"
  "m t d" "dir"
  "m t f" "failed"
  "m t m" "module"

  ;; pdb
  "m p" "pdb"
  "m p o" "one"
  "m p a" "all"
  "m p d" "dir"
  "m p f" "failed"
  "m p m" "module"

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

  ;; format
  "m y" "format")

;; provide this configuration
(provide 'sk-js)
