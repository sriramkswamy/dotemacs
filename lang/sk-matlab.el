;; install matlab from OCIO first
;; better repl name
(defun sk/matlab-shell-buffer-name ()
  "set a better shell name for MATLAB"
  (interactive)
  (if (not (vc-root-dir))
	  (message "Using default MATLAB shell buffer name")
	;; set it to a more sensible value
	(setq matlab-shell-buffer-name (concat "MATLAB-"
										   (file-name-nondirectory
											(directory-file-name
											 (file-name-directory (vc-root-dir))))))))

;; better repl
(defun sk/matlab-shell ()
  "open matlab REPL in a split window"
  (interactive)
  (sk/matlab-shell-buffer-name)
  (bookmark-set "mshell")
  (split-window-sensibly)
  (other-window 1)
  (matlab-shell)
  (other-window 1)
  (bookmark-jump "mshell"))

;; diminish mlint mode line indicator
(defun sk/diminish-mlint-minor ()
  "diminish mlint minor mode"
  (interactive)
  (diminish 'mlint-minor-mode ""))

;; matlab operator bindings
(defun sk/matlab-operator ()
  "operator text object bindings for matlab"
  (interactive)
  (eval `(ryo-modal-major-mode-keys
		  'matlab-mode
		  ("m s" ,text-objects :then '(matlab-shell-run-region)))))

;; the matlab mode
(use-package matlab-mode
  ;; :load-path "lang/matlab-mode/"
  :ensure t
  :mode ("\\.m\\'" . matlab-mode)
  :diminish mlint-minor-mode
  :hook ((matlab-mode . mlint-minor-mode)
         (mlint-minor-mode . sk/diminish-mlint-minor)
         (matlab-mode . display-line-numbers-mode)
         (matlab-mode . sk/gud-mode)
         (matlab-mode . sk/enable-ryo-modal-mode)
         (matlab-mode . sk/matlab-operator)
		 (matlab-mode . sk/matlab-shell-buffer-name)
		 (matlab-mode . sk/company-matlab)
		 (matlab-shell-mode . sk/company-matlab-shell))
  :bind (:map matlab-shell-mode-map
			  ("C-c C-c" . term-interrupt-subjob)
			  ("C-d" . company-matlab-shell))

  :commands
  (matlab-shell-describe-command
   matlab-shell-describe-variable
   matlab-shell-topic-browser
   matlab-find-file-on-path
   matlab-shell-save-and-go
   matlab-shell-run-command
   matlab-show-matlab-shell-buffer
   matlab-show-line-info)

  :init
  ;; workaround for emacs 26
  (if (version< emacs-version "26")
	  (message "Tracking stable Emacs")
	(defvar default-fill-column (default-value 'fill-column))
	(defalias 'string-to-int 'string-to-number))
  ;; settings
  (cond ((eq system-type 'gnu/linux)                 ; if system is GNU/Linux
		 (setq matlab-shell-command "/home/sriramkrish92/MATLAB/bin/matlab"
			   matlab-indent-function t)
		 (setq matlab-mode-install-path '("/home/sriramkrish92/MATLAB/toolbox"))
		 (setq-default mlint-programs '("/home/sriramkrish92/MATLAB/bin/glnxa64/mlint")))
		((eq system-type 'darwin)                    ; if system is macOS
		 (setq matlab-shell-command "/Applications/MATLAB_R2016a.app/bin/matlab"
			   matlab-indent-function t)
		 (setq matlab-mode-install-path '("/Applications/MATLAB_R2016a.app/toolbox"))
		 (setq-default mlint-programs '("/Applications/MATLAB_R2016a.app/bin/glnxa64/mlint"))))
  (eval-after-load 'matlab
	'(add-to-list 'matlab-shell-command-switches "-softwareopengl -nodesktop -nosplash"))
  (setq-default matlab-show-mlint-warnings t)
  (setq-default mlint-verbose t)

  :config
  (require 'matlab-load)
  (require 'mlint)
  (mlint-minor-mode 1))

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
(sk/repl-string-query matlab comint dbstop-warning "dbstop if warning"
					  nil matlab-show-matlab-shell-buffer)
(sk/repl-string-query matlab comint dbclear-all "dbclear all"
					  nil matlab-show-matlab-shell-buffer)
(sk/repl-string-query matlab comint dbquit-all "dbclear all; dbquit"
					  nil matlab-show-matlab-shell-buffer)

;; environment
(sk/repl-query matlab comint workspace nil matlab-show-matlab-shell-buffer)
(sk/repl-query matlab comint whos nil matlab-show-matlab-shell-buffer)
(sk/repl-paren-string-query matlab comint openvar nil matlab-show-matlab-shell-buffer)
(sk/repl-paren-string-query matlab comint edit nil matlab-show-matlab-shell-buffer)
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
(ryo-modal-major-mode-keys
 'matlab-mode
 ;; bindings
 ("m d" matlab-shell-describe-command :name "doc command")
 ("m w" matlab-shell-describe-variable :name "describe variable")
 ("m i" matlab-show-line-info :name "line info")
 ("m t" matlab-shell-topic-browser :name "topic browser")
 ("m j" matlab-find-file-on-path :name "jump to file")
 ("m m" matlab-shell-save-and-go :name "run")
 ("m c" matlab-shell-run-command :name "command")
 ("m r" sk/matlab-shell :name "run matlab")
 ("m z" matlab-show-matlab-shell-buffer :name "goto shell")
 ("m q" hydra-mlint/body :name "mlint")
 ("m /" sk/matlab-shell-addpath :name "add path")
 ("m y" sk/matlab-shell-genpath :name "add general path")

 ;; matlab functions bindings
 ("m g s" sk/gud-break :name "set break")
 ("m g x" sk/gud-remove :name "delete break")
 ("m g a" sk/matlab-dbclear-all-nil :name "clear all")
 ("m g l" sk/matlab-dbstatus-nil :name "status")
 ("m g q" sk/gud-finish :name "quit")
 ("m g n" sk/gud-next :name "next")
 ("m g i" sk/gud-step :name "step in")
 ("m g o" sk/matlab-dbstep-out-nil :name "step out")
 ("m g c" sk/gud-cont :name "continue")
 ("m g e" sk/matlab-dbstop-error-nil :name "stop on error")
 ("m g w" sk/matlab-dbstop-warning-nil :name "stop on warning")
 ("m g u" sk/gud-up :name "up")
 ("m g p" sk/gud-print :name "print")
 ("m g r" sk/gud-refresh :name "refresh")
 ("m g f" sk/gud-find-c-expr :name "find c expression")

 ("m b" sk/matlab-workspace-nil :name "workspace")
 ("m a" sk/matlab-whos-nil :name "list all variables")
 ("m v" sk/matlab-openvar-nil :name "open variable")
 ("m p" sk/matlab-edit-nil :name "preview in matlab")
 ("m x" sk/matlab-size-nil :name "size of variable")
 ("m l" sk/matlab-length-nil :name "length of variable")
 ("m e" sk/matlab-numel-nil :name "number of elements in variable")
 ("m n" sk/matlab-fieldnames-nil :name "fieldnames in struct")
 ("m o" sk/matlab-ndim-nil :name "number of dimensions in variable")
 ("m -" sk/matlab-mean-nil :name "mean of variable")
 ("m =" sk/matlab-sum-nil :name "sum of variable")
 ("m +" sk/matlab-cumsum-nil :name "cumulative sum of variable")
 ("m f p" sk/matlab-plot-nil :name "plot")
 ("m f m" sk/matlab-mesh-nil :name "mesh plot")
 ("m h" sk/matlab-doc-nil :name "help doc")
 ("m u" sk/matlab-clear-all-nil :name "clear all"))

;; ryo major mode
(which-key-add-major-mode-key-based-replacements
  'matlab-mode
  "m g" "debug"
  "m q" "error"
  "m f" "plot"
  "m s" "send")

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
(sk/repl-string-query matlab comint dbstop-warning "dbstop if warning"
					  tmux matlab-show-matlab-shell-buffer)
(sk/repl-string-query matlab comint dbclear-all "dbclear all"
					  tmux matlab-show-matlab-shell-buffer)
(sk/repl-string-query matlab comint dbquit-all "dbclear all; dbquit"
					  tmux matlab-show-matlab-shell-buffer)

;; environment
(sk/repl-query matlab comint workspace tmux matlab-show-matlab-shell-buffer)
(sk/repl-query matlab comint whos tmux matlab-show-matlab-shell-buffer)
(sk/repl-paren-string-query matlab comint openvar tmux matlab-show-matlab-shell-buffer)
(sk/repl-paren-string-query matlab comint edit tmux matlab-show-matlab-shell-buffer)
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

;; setup matlab bindings
(ryo-modal-major-mode-keys
 'matlab-mode
 ("r w" sk/matlab-tmux-who :name "describe variable")
 ("r m" sk/matlab-tmux-run :name "run")
 ("r c" emamux:send-command :name "command")
 ("r r" sk/matlab-tmux :name "run matlab")
 ("r /" sk/matlab-tmux-addpath :name "add path")
 ("r y" sk/matlab-tmux-genpath :name "add gen path")
 ("r d" sk/matlab-help-tmux :name "doc")
 ("r j" find-file-at-point :name "jump to file")

 ;; matlab functions bindings
 ("r g s" sk/matlab-dbstop-tmux :then '(sk/breakpoint-icon-set) :name "set breakpoint")
 ("r g x" sk/matlab-dbclear-tmux :then '(sk/breakpoint-icon-remove) :name "delete breakpoint")
 ("r g a" sk/matlab-dbclear-all-tmux :name "clear all")
 ("r g l" sk/matlab-dbstatus-tmux :name "status")
 ("r g q" sk/matlab-dbquit-tmux :name "quit")
 ("r g n" sk/matlab-dbstep-tmux :name "next")
 ("r g i" sk/matlab-dbstep-in-tmux :name "step in")
 ("r g o" sk/matlab-dbstep-out-tmux :name "step out")
 ("r g c" sk/matlab-dbcont-tmux :name "continue")
 ("r g e" sk/matlab-dbstop-error-tmux :name "stop on error")
 ("r g w" sk/matlab-dbstop-warning-tmux :name "stop on warning")

 ("r b" sk/matlab-workspace-tmux :name "workspace")
 ("r a" sk/matlab-whos-tmux :name "list all variables")
 ("r v" sk/matlab-openvar-tmux :name "open variable")
 ("r p" sk/matlab-edit-tmux :name "preview in matlab")
 ("r x" sk/matlab-size-tmux :name "size")
 ("r l" sk/matlab-length-tmux :name "length")
 ("r e" sk/matlab-numel-tmux :name "number of elements")
 ("r n" sk/matlab-fieldnames-tmux :name "fieldnames")
 ("r o" sk/matlab-ndim-tmux :name "number of dimensions")
 ("r -" sk/matlab-mean-tmux :name "mean")
 ("r =" sk/matlab-sum-tmux :name "sum")
 ("r +" sk/matlab-cumsum-tmux :name "cumulative sum")
 ("r f p" sk/matlab-plot-tmux :name "plot")
 ("r f m" sk/matlab-mesh-tmux :name "mesh plot")
 ("r h" sk/matlab-doc-tmux :name "help")
 ("r u" sk/matlab-clear-all-tmux :name "clear all"))

;; which key hints
(which-key-add-major-mode-key-based-replacements
  'matlab-mode
  "r g" "debug"
  "r f" "figure"

  "m g" "debug"
  "m f" "figure"
  
  "m s" "eval"
  "m s i" "inside"
  "m s a" "around"
  "m s g" "global")

;; provide matlab configuration
(provide 'sk-matlab)
