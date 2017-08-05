;; install matlab from OCIO first
(use-package matlab-mode
  :ensure t
  :mode ("\\.m\\'" . matlab-mode)
  :diminish mlint-minor-mode
  :bind (:map matlab-shell-mode-map
			  ("C-c C-c" . term-interrupt-subjob))
  :init
  ;; workaround for emacs 26
  (if (version< emacs-version "26")
	  (message "Tracking stable Emacs")
	(defvar default-fill-column (default-value 'fill-column))
	(defalias 'string-to-int 'string-to-number))
  ;; settings
  (setq matlab-shell-command "/Applications/MATLAB_R2016a.app/bin/matlab"
		matlab-indent-function t)
  (setq matlab-mode-install-path '("~/Dropbox/PhD/codes/ttmat"
								   "/Applications/MATLAB_R2016a.app/toolbox"))
  (eval-after-load 'matlab
	'(add-to-list 'matlab-shell-command-switches "-nodesktop -nosplash"))
  :config
  (require 'matlab-load)
  (require 'mlint))

;; better repl
(defun sk/matlab-shell ()
  "open matlab REPL in a split window"
  (interactive)
  (bookmark-set "mshell")
  (split-window-horizontally)
  (other-window 1)
  (matlab-shell)
  (other-window 1)
  (bookmark-jump "mshell"))

;; setup matlab properly
(defun sk/matlab-setup ()
  "setup matlab mode properly"
  ;; mlint settings
  (setq-default matlab-show-mlint-warnings t)
  (setq-default mlint-verbose t)
  (setq-default mlint-programs '("/Applications/MATLAB_R2016a.app/bin/maci64/mlint"))
  (mlint-minor-mode 1)
  (rainbow-delimiters-mode)
  (if (fboundp 'indent-guide-mode)
	  (indent-guide-mode 1))
  ;; start ryo-modal-mode
  (sk/enable-ryo-modal-mode))
(add-hook 'matlab-mode-hook #'sk/matlab-setup)
(defun sk/diminish-mlint-minor ()
  (interactive)
  (diminish 'mlint-minor-mode ""))
(add-hook 'mlint-minor-mode-hook 'sk/diminish-mlint-minor)
(add-hook 'mlint-minor-mode-hook 'sk/enable-ryo-modal-mode)

;; hydra for mlint
(defhydra hydra-mlint (:color pink :hint nil)
  "
 _k_: previous   _f_: fix
 _j_: next       _m_: mlint
"
  ("k" mlint-prev-buffer)
  ("j" mlint-next-buffer)
  ("f" mlint-fix-warning)
  ("m" mlint-buffer)
  ("q" nil :color blue))

;;; functions based around matlab-mode

;; debugging
(sk/matlab-debug-query dbstop)
(sk/matlab-debug-query dbclear)
(sk/repl-query matlab comint dbstatus nil matlab-show-matlab-shell-buffer)
(sk/repl-query matlab comint dbquit nil matlab-show-matlab-shell-buffer)
(sk/repl-query matlab comint dbstep nil matlab-show-matlab-shell-buffer)
(sk/repl-query matlab comint dbcont nil matlab-show-matlab-shell-buffer)
(sk/repl-string-query matlab comint dbstep-in "dbstep in"
					  nil matlab-show-matlab-shell-buffer)
(sk/repl-string-query matlab comint dbstep-out "dbstep out"
					  nil matlab-show-matlab-shell-buffer)
(sk/repl-string-query matlab comint dbstop-error "dbstop if error"
					  nil matlab-show-matlab-shell-buffer)
(sk/repl-string-query matlab comint dbclear-all "dbclear all"
					  nil matlab-show-matlab-shell-buffer)
(sk/repl-string-query matlab comint dbquit-all "dbclear all; dbquit"
					  nil matlab-show-matlab-shell-buffer)

;; environment
(sk/repl-query matlab comint workspace nil matlab-show-matlab-shell-buffer)
(sk/repl-query matlab comint whos nil matlab-show-matlab-shell-buffer)
(sk/repl-paren-query matlab comint openvar nil matlab-show-matlab-shell-buffer)
(sk/repl-paren-query matlab comint size nil matlab-show-matlab-shell-buffer)
(sk/repl-paren-query matlab comint length nil matlab-show-matlab-shell-buffer)
(sk/repl-paren-query matlab comint numel nil matlab-show-matlab-shell-buffer)
(sk/repl-paren-query matlab comint fieldnames nil matlab-show-matlab-shell-buffer)
(sk/repl-paren-query matlab comint ndim nil matlab-show-matlab-shell-buffer)
(sk/repl-paren-query matlab comint mean nil matlab-show-matlab-shell-buffer)
(sk/repl-paren-query matlab comint sum nil matlab-show-matlab-shell-buffer)
(sk/repl-paren-query matlab comint cumsum nil matlab-show-matlab-shell-buffer)
(sk/repl-string-query matlab comint clear-all "clear all;"
					  nil matlab-show-matlab-shell-buffer)
(sk/repl-string-query matlab comint close-all "close all;"
					  nil matlab-show-matlab-shell-buffer)

;; plotting
(sk/repl-paren-query matlab comint plot nil matlab-show-matlab-shell-buffer)
(sk/repl-paren-query matlab comint mesh nil matlab-show-matlab-shell-buffer)

;; help
(sk/repl-space-query matlab comint doc nil matlab-show-matlab-shell-buffer)

;; addpath of the variable
(defun sk/matlab-shell-addpath ()
  "get the addpath of the variable"
  (interactive)
  (bookmark-set "mshell")
  (let* ((sk/buffer-name (buffer-name))
		 (sk/string-to-send (concat "addpath(" default-directory ")\n")))
	(matlab-show-matlab-shell-buffer)
	(delete-other-windows)
	(comint-send-string (get-buffer-process (current-buffer)) sk/string-to-send)
	(split-window-horizontally))
  (bookmark-jump "mshell"))

;; addpath of the variable
(defun sk/matlab-shell-genpath ()
  "get the addpath genpath of the variable"
  (interactive)
  (bookmark-set "mshell")
  (let* ((sk/buffer-name (buffer-name))
		 (sk/string-to-send (concat "addpath(genpath(" default-directory "))\n")))
	(matlab-show-matlab-shell-buffer)
	(delete-other-windows)
	(comint-send-string (get-buffer-process (current-buffer)) sk/string-to-send)
	(split-window-horizontally))
  (bookmark-jump "mshell"))

;; setup matlab bindings - finally!
(defun sk/matlab-bindings ()
  "becuase matlab is just weird"
  (interactive)
  ;; bindings
  (ryo-modal-key "m d" 'matlab-shell-describe-command :mode 'matlab-mode)
  (ryo-modal-key "m w" 'matlab-shell-describe-variable :mode 'matlab-mode)
  (ryo-modal-key "m i" 'matlab-show-line-info :mode 'matlab-mode)
  (ryo-modal-key "m t" 'matlab-shell-topic-browser :mode 'matlab-mode)
  (ryo-modal-key "m j" 'matlab-find-file-on-path :mode 'matlab-mode)
  (ryo-modal-key "m m" 'matlab-shell-save-and-go :mode 'matlab-mode)
  (ryo-modal-key "m c" 'matlab-shell-run-command :mode 'matlab-mode)
  (ryo-modal-key "m r" 'sk/matlab-shell :mode 'matlab-mode)
  (ryo-modal-key "m z" 'matlab-show-matlab-shell-buffer :mode 'matlab-mode)
  (ryo-modal-key "m \\" 'hydra-mlint/body :mode 'matlab-mode)
  (ryo-modal-key "m /" 'sk/matlab-shell-addpath :mode 'matlab-mode)
  (ryo-modal-key "m y" 'sk/matlab-shell-genpath :mode 'matlab-mode)

  ;; matlab functions bindings
  (ryo-modal-key "m g s" 'gud-break :then '(sk/breakpoint-icon-set)
				 :mode 'matlab-mode)
  (ryo-modal-key "m g x" 'gud-remove :then '(sk/breakpoint-icon-remove)
				 :mode 'matlab-mode)
  (ryo-modal-key "m g a" 'sk/matlab-dbclear-all-nil :mode 'matlab-mode)
  (ryo-modal-key "m g l" 'sk/matlab-dbstatus-nil :mode 'matlab-mode)
  (ryo-modal-key "m g q" 'gud-finish :mode 'matlab-mode)
  (ryo-modal-key "m g n" 'gud-next :mode 'matlab-mode)
  (ryo-modal-key "m g i" 'gud-step :mode 'matlab-mode)
  (ryo-modal-key "m g o" 'sk/matlab-dbstep-out-nil :mode 'matlab-mode)
  (ryo-modal-key "m g c" 'gud-cont :mode 'matlab-mode)
  (ryo-modal-key "m g e" 'sk/matlab-dbstop-error-nil :mode 'matlab-mode)
  (ryo-modal-key "m g u" 'gud-up :mode 'matlab-mode)
  (ryo-modal-key "m g p" 'gud-print :mode 'matlab-mode)
  (ryo-modal-key "m g r" 'gud-refresh :mode 'matlab-mode)
  (ryo-modal-key "m g f" 'gud-find-c-expr :mode 'matlab-mode)

  (ryo-modal-key "m b" 'sk/matlab-workspace-nil :mode 'matlab-mode)
  (ryo-modal-key "m a" 'sk/matlab-whos-nil :mode 'matlab-mode)
  (ryo-modal-key "m v" 'sk/matlab-openvar-nil :mode 'matlab-mode)
  (ryo-modal-key "m x" 'sk/matlab-size-nil :mode 'matlab-mode)
  (ryo-modal-key "m l" 'sk/matlab-length-nil :mode 'matlab-mode)
  (ryo-modal-key "m k" 'sk/matlab-numel-nil :mode 'matlab-mode)
  (ryo-modal-key "m n" 'sk/matlab-fieldnames-nil :mode 'matlab-mode)
  (ryo-modal-key "m o" 'sk/matlab-ndim-nil :mode 'matlab-mode)
  (ryo-modal-key "m e" 'sk/matlab-mean-nil :mode 'matlab-mode)
  (ryo-modal-key "m =" 'sk/matlab-sum-nil :mode 'matlab-mode)
  (ryo-modal-key "m +" 'sk/matlab-cumsum-nil :mode 'matlab-mode)
  (ryo-modal-key "m f p" 'sk/matlab-plot-nil :mode 'matlab-mode)
  (ryo-modal-key "m f m" 'sk/matlab-mesh-nil :mode 'matlab-mode)
  (ryo-modal-key "m h" 'sk/matlab-doc-nil :mode 'matlab-mode)
  (ryo-modal-key "m u" 'sk/matlab-clear-all-nil :mode 'matlab-mode)

  (sk/enable-ryo-modal-mode))
(add-hook 'matlab-mode-hook #'sk/matlab-bindings)
;; operator/textobject sending
(add-hook 'matlab-mode-hook
		  (sk/ryo-operator-object matlab-send "m s" "s" matlab-shell-run-region t matlab-mode))

;; ryo major mode
(which-key-add-major-mode-key-based-replacements 'matlab-mode

  "m d" "describe cmd"
  "m w" "describe var"
  "m i" "line info"
  "m t" "topic"
  "m j" "jump to def"
  "m m" "make/run"
  "m c" "run cmd"
  "m r" "run shell"
  "m z" "back to shell"

  ;; function bindings
  "m b" "workspace"
  "m a" "list variables"
  "m v" "open variable"
  "m x" "size of variable"
  "m l" "length of variable"
  "m k" "numel of variable"
  "m n" "names of variable"
  "m o" "dimensions of variable"
  "m e" "mean of variable"
  "m =" "sum of variable"
  "m +" "cumulative sum of variable"
  "m h" "documentation"
  "m u" "clear variables"
  "m /" "addpath dir"
  "m y" "genpath dir"
  "m g" "debug"
  "m f" "plot"
  "m g s" "set breakpoint"
  "m g x" "clear breakpoint"
  "m g a" "clear all breakpoints"
  "m g q" "quit"
  "m g n" "step over"
  "m g i" "step in"
  "m g o" "step out"
  "m g c" "continue"
  "m g e" "stop on error"
  "m g u" "debugger up"
  "m g p" "print"
  "m g r" "refresh"
  "m g f" "find expression"
  "m f p" "plot variable"
  "m f m" "mesh plot"

  ;; mlint
  "m \\" "mlint"

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
  "m s i h" "diff hunk"
  "m s a h" "diff hunk"
  "m s i x" "latex section"
  "m s a x" "latex section"
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

;;; Shell interaction

;; better repl
(defun sk/matlab-tmux ()
  "open matlab REPL in a split window"
  (interactive)
  (emamux:run-command "matlab"))

;; run the program
(defun sk/matlab-tmux-run ()
  "run matlab REPL in a split window"
  (interactive)
  (let* ((sk/buffer-name (buffer-name))
		 (sk/string-to-send (concat "run " sk/buffer-name)))
	(emamux:send-command sk/string-to-send)))

;; debugging
(sk/matlab-debug-query dbstop tmux)
(sk/matlab-debug-query dbclear tmux)
(sk/repl-query matlab comint dbstatus tmux matlab-show-matlab-shell-buffer)
(sk/repl-query matlab comint dbquit tmux matlab-show-matlab-shell-buffer)
(sk/repl-query matlab comint dbstep tmux matlab-show-matlab-shell-buffer)
(sk/repl-query matlab comint dbcont tmux matlab-show-matlab-shell-buffer)
(sk/repl-string-query matlab comint dbstep-in "dbstep in"
					  tmux matlab-show-matlab-shell-buffer)
(sk/repl-string-query matlab comint dbstep-out "dbstep out"
					  tmux matlab-show-matlab-shell-buffer)
(sk/repl-string-query matlab comint dbstop-error "dbstop if error"
					  tmux matlab-show-matlab-shell-buffer)
(sk/repl-string-query matlab comint dbclear-all "dbclear all"
					  tmux matlab-show-matlab-shell-buffer)
(sk/repl-string-query matlab comint dbquit-all "dbclear all; dbquit"
					  tmux matlab-show-matlab-shell-buffer)

;; environment
(sk/repl-query matlab comint workspace tmux matlab-show-matlab-shell-buffer)
(sk/repl-query matlab comint whos tmux matlab-show-matlab-shell-buffer)
(sk/repl-paren-query matlab comint openvar tmux matlab-show-matlab-shell-buffer)
(sk/repl-paren-query matlab comint size tmux matlab-show-matlab-shell-buffer)
(sk/repl-paren-query matlab comint length tmux matlab-show-matlab-shell-buffer)
(sk/repl-paren-query matlab comint numel tmux matlab-show-matlab-shell-buffer)
(sk/repl-paren-query matlab comint fieldnames tmux matlab-show-matlab-shell-buffer)
(sk/repl-paren-query matlab comint ndim tmux matlab-show-matlab-shell-buffer)
(sk/repl-paren-query matlab comint mean tmux matlab-show-matlab-shell-buffer)
(sk/repl-paren-query matlab comint sum tmux matlab-show-matlab-shell-buffer)
(sk/repl-paren-query matlab comint cumsum tmux matlab-show-matlab-shell-buffer)
(sk/repl-string-query matlab comint clear-all "clear all;"
					  tmux matlab-show-matlab-shell-buffer)
(sk/repl-string-query matlab comint close-all "close all;"
					  tmux matlab-show-matlab-shell-buffer)

;; plotting
(sk/repl-paren-query matlab comint plot tmux matlab-show-matlab-shell-buffer)
(sk/repl-paren-query matlab comint mesh tmux matlab-show-matlab-shell-buffer)

;; help
(sk/repl-space-query matlab comint doc tmux matlab-show-matlab-shell-buffer)
(sk/repl-space-query matlab comint help tmux matlab-show-matlab-shell-buffer)

;; describe variable
(defun sk/matlab-tmux-who ()
  "describe variable"
  (interactive)
  (let* ((sk/buffer-name (buffer-name))
		 (sk/string-to-send (concat "whos " (thing-at-point 'symbol))))
	(emamux:send-command sk/string-to-send)))

;; addpath of the variable
(defun sk/matlab-tmux-addpath ()
  "get the addpath of the variable"
  (interactive)
  (let* ((sk/buffer-name (buffer-name))
		 (sk/string-to-send (concat "addpath(" default-directory ")")))
	(emamux:send-command sk/string-to-send)))

;; addpath genpath of the variable
(defun sk/matlab-tmux-genpath ()
  "get the addpath of the variable"
  (interactive)
  (let* ((sk/buffer-name (buffer-name))
		 (sk/string-to-send (concat "addpath(genpath(" default-directory "))")))
	(emamux:send-command sk/string-to-send)))

;; setup matlab bindings - finally!
(defun sk/matlab-tmux-bindings ()
  "becuase matlab is just weird"
  (interactive)
  ;; bindings
  (ryo-modal-key "r w" 'sk/matlab-tmux-who :mode 'matlab-mode)
  (ryo-modal-key "r m" 'sk/matlab-tmux-run :mode 'matlab-mode)
  (ryo-modal-key "r c" 'emamux:send-command :mode 'matlab-mode)
  (ryo-modal-key "r r" 'sk/matlab-tmux :mode 'matlab-mode)
  (ryo-modal-key "r /" 'sk/matlab-tmux-addpath :mode 'matlab-mode)
  (ryo-modal-key "r y" 'sk/matlab-tmux-genpath :mode 'matlab-mode)
  (ryo-modal-key "r d" 'sk/matlab-help-tmux :mode 'matlab-mode)
  (ryo-modal-key "r j" 'find-file-at-point :mode 'matlab-mode)

  ;; matlab functions bindings
  (ryo-modal-key "r g s" 'sk/matlab-dbstop-tmux :then '(sk/breakpoint-icon-set)
				 :mode 'matlab-mode)
  (ryo-modal-key "r g x" 'sk/matlab-dbclear-tmux :then '(sk/breakpoint-icon-remove)
				 :mode 'matlab-mode)
  (ryo-modal-key "r g a" 'sk/matlab-dbclear-all-tmux :mode 'matlab-mode)
  (ryo-modal-key "r g l" 'sk/matlab-dbstatus-tmux :mode 'matlab-mode)
  (ryo-modal-key "r g q" 'sk/matlab-dbquit-tmux :mode 'matlab-mode)
  (ryo-modal-key "r g n" 'sk/matlab-dbstep-tmux :mode 'matlab-mode)
  (ryo-modal-key "r g i" 'sk/matlab-dbstep-in-tmux :mode 'matlab-mode)
  (ryo-modal-key "r g o" 'sk/matlab-dbstep-out-tmux :mode 'matlab-mode)
  (ryo-modal-key "r g c" 'sk/matlab-dbcont-tmux :mode 'matlab-mode)
  (ryo-modal-key "r g e" 'sk/matlab-dbstop-error-tmux :mode 'matlab-mode)

  (ryo-modal-key "r b" 'sk/matlab-workspace-tmux :mode 'matlab-mode)
  (ryo-modal-key "r a" 'sk/matlab-whos-tmux :mode 'matlab-mode)
  (ryo-modal-key "r v" 'sk/matlab-openvar-tmux :mode 'matlab-mode)
  (ryo-modal-key "r x" 'sk/matlab-size-tmux :mode 'matlab-mode)
  (ryo-modal-key "r l" 'sk/matlab-length-tmux :mode 'matlab-mode)
  (ryo-modal-key "r k" 'sk/matlab-numel-tmux :mode 'matlab-mode)
  (ryo-modal-key "r n" 'sk/matlab-fieldnames-tmux :mode 'matlab-mode)
  (ryo-modal-key "r o" 'sk/matlab-ndim-tmux :mode 'matlab-mode)
  (ryo-modal-key "r e" 'sk/matlab-mean-tmux :mode 'matlab-mode)
  (ryo-modal-key "r =" 'sk/matlab-sum-tmux :mode 'matlab-mode)
  (ryo-modal-key "r +" 'sk/matlab-cumsum-tmux :mode 'matlab-mode)
  (ryo-modal-key "r f p" 'sk/matlab-plot-tmux :mode 'matlab-mode)
  (ryo-modal-key "r f m" 'sk/matlab-mesh-tmux :mode 'matlab-mode)
  (ryo-modal-key "r h" 'sk/matlab-doc-tmux :mode 'matlab-mode)
  (ryo-modal-key "r u" 'sk/matlab-clear-all-tmux :mode 'matlab-mode)

  (sk/enable-ryo-modal-mode))
(add-hook 'matlab-mode-hook #'sk/matlab-tmux-bindings)

;; ryo major mode
(which-key-add-major-mode-key-based-replacements 'matlab-mode
  "r d" "help"
  "r w" "describe var"
  "r i" "line info"
  "r t" "topic"
  "r j" "jump to def"
  "r m" "make/run"
  "r c" "run cmd"
  "r r" "run shell"
  "r z" "back to shell"
  "r h" "documentation"

  ;; function bindings
  "r b" "workspace"
  "r a" "list variables"
  "r v" "open variable"
  "r x" "size of variable"
  "r l" "length of variable"
  "r k" "numel of variable"
  "r n" "names of variable"
  "r o" "dimensions of variable"
  "r e" "mean of variable"
  "r =" "sum of variable"
  "r +" "cumulative sum of variable"
  "r h" "documentation"
  "r u" "clear variables"
  "r y" "addpath dir"
  "r g" "debug"
  "r f" "plot"
  "r g s" "set breakpoint"
  "r g x" "clear breakpoint"
  "r g a" "clear all breakpoints"
  "r g q" "quit"
  "r g n" "step over"
  "r g i" "step in"
  "r g o" "step out"
  "r g c" "continue"
  "r g e" "stop on error"
  "r f p" "plot variable"
  "r f m" "mesh plot")

;; provide matlab configuration
(provide 'sk-matlab)
