;; ansi-term behave like shell-mode by default
(defun sk/term ()
  "calls ansi term that behaves like shell mode"
  (interactive)
  (sk/split-right-and-move)
  (cond ((eq system-type 'gnu/linux)
		 (ansi-term "/bin/bash"))
		((eq system-type 'darwin)
		 (ansi-term "/usr/local/bin/bash")))
  (sk/term-toggle-mode)
  (other-window 1))

;; shell based on OS
(defun sk/shell (arg)
  "choose shell based on operating system and calling it with universal argument just invokes shell"
  (interactive "P")
  (if arg
	  (shell)
	(cond ((eq system-type 'gnu/linux)
		   (setq explicit-shell-file-name "/bin/bash"))
		  ((eq system-type 'darwin)
		   (setq explicit-shell-file-name "/usr/local/bin/bash")))
	(shell))
  (other-window 1))

;; Call the terminal
(defun sk/call-terminal ()
  (interactive)
  (cond ((eq system-type 'gnu/linux)
		 (shell-command "xterm"))
		((eq system-type 'darwin)
		 (shell-command "open -a /Applications/Utilities/Terminal.app"))))
(bind-key* "C-c :" 'sk/call-terminal)

;; Shell auto completion
(use-package company-shell
  :ensure t
  :bind* (("C-t l"	. company-shell)
		  ("C-t C-l"	. company-shell)))

;; zoom into the tmux pane (tmux > 1.8)
;; tmux resize-pane -Z
(defun sk/zoom-tmux ()
  (interactive)
  (shell-command "tmux resize-pane -Z"))
(bind-key* "C-c Z" 'sk/zoom-tmux)

;;;;;;;;;;;
;; Tmux  ;;
;;;;;;;;;;;

;; interact with tmux
(use-package emamux
  :ensure t
  ;; :load-path "site-lisp/emamux/"
  :commands (emamux:send-command
             emamux:send-region
			 emamux:run-command
			 emamux:run-region
			 emamux:new-window
			 emamux:clone-current-frame
			 emamux:split-window
			 emamux:split-window-horizontally
			 emamux:run-last-command
			 emamux:zoom-runner
			 emamux:inspect-runner
			 emamux:close-runner-pane
			 emamux:close-panes
			 emamux:clear-runner-history
			 emamux:interrupt-runner
			 emamux:copy-kill-ring
			 emamux:yank-from-list-buffers))

;; mapping for interacting with tmux
(ryo-modal-keys
 ("r TAB"
  (("y" emamux:copy-kill-ring)
   ("p" emamux:yank-from-list-buffers)
   ("o" emamux:close-panes))))
(sk/ryo-operator-object emamux-run "r TAB" "TAB" emamux:run-region t)
(sk/ryo-emamux-run-bindings)

(ryo-modal-keys
 ("r s"
  (("q" emamux:close-panes)
   ("y" emamux:copy-kill-ring)
   ("p" emamux:yank-from-list-buffers))))
(sk/ryo-operator-object emamux-send "r s" "s" emamux:send-region t)
(sk/ryo-emamux-send-bindings)

;; global bindings
(ryo-modal-key "r -" 'emamux:split-window)
(ryo-modal-key "r \\" 'emamux:split-window-horizontally)
(ryo-modal-key "r |" 'emamux:new-window)
(ryo-modal-key "r _" 'emamux:clone-current-frame)
(ryo-modal-key "r DEL" 'emamux:interrupt-runner)
(ryo-modal-key "r >" 'emamux:inspect-runner)
(ryo-modal-key "r <" 'emamux:close-runner-pane)
(ryo-modal-key "r :" 'emamux:run-command)
(ryo-modal-key "r z" 'emamux:zoom-runner)
(ryo-modal-key "r SPC" 'emamux:clear-runner-history)
(ryo-modal-key "r RET" 'emamux:send-command)

;; general
(which-key-add-key-based-replacements
  "r -" "split horizontally"
  "r \\" "split vertically"
  "r |" "new window"
  "r DEL" "interrupt runner"
  "r _" "clone frame"
  "r >" "inspect runner"
  "r <" "close runner"
  "r :" "start runner"
  "r z" "zoom runner"
  "r RET" "send command"
  "r SPC" "clear history")

;; runner pane
(which-key-add-key-based-replacements
  "r TAB" "tmux run"
  "r TAB y" "copy"
  "r TAB p" "paste from"
  "r TAB o" "close panes"
  "r TAB s" "run command"
  "r TAB -" "split below"
  "r TAB v" "split right"
  "r TAB n" "new window"
  "r TAB x" "interrupt runner"
  "r TAB z" "zoom runner"
  "r TAB r" "region"
  "r TAB i" "inside"
  "r TAB a" "around"
  "r TAB g" "global"
  "r TAB i a" "all"
  "r TAB a a" "all"
  "r TAB i w" "word"
  "r TAB a w" "word"
  "r TAB i p" "para"
  "r TAB a p" "para"
  "r TAB i s" "sentence"
  "r TAB a s" "sentence"
  "r TAB i l" "line"
  "r TAB a l" "line"
  "r TAB i y" "symbol"
  "r TAB a y" "symbol"
  "r TAB i c" "comment"
  "r TAB a c" "comment"
  "r TAB i f" "function"
  "r TAB a f" "function"
  "r TAB i q" "quote"
  "r TAB a q" "quote"
  "r TAB i b" "block/pairs"
  "r TAB a b" "block/pairs"
  "r TAB i o" "org code"
  "r TAB a o" "org code"
  "r TAB i u" "org subtree"
  "r TAB a u" "org subtree"
  "r TAB i e" "latex env"
  "r TAB a e" "latex env"
  "r TAB i r" "method call"
  "r TAB a r" "method call"
  "r TAB i d" "ruby block"
  "r TAB a d" "ruby block"
  "r TAB i h" "diff hunk"
  "r TAB a h" "diff hunk"
  "r TAB i x" "latex section"
  "r TAB a x" "latex section"
  "r TAB i g" "python string"
  "r TAB a g" "python string"
  "r TAB i m" "python block"
  "r TAB a m" "python block"
  "r TAB i n" "python statement"
  "r TAB a n" "python block and dec"
  "r TAB i $" "latex math"
  "r TAB a $" "latex math"
  "r TAB f" "to char"
  "r TAB F" "to char back"
  "r TAB t" "till char"
  "r TAB T" "till char back"
  "r TAB ;" "find on screen"
  "r TAB g ;" "till line"
  "r TAB h" "prev char"
  "r TAB j" "next line"
  "r TAB k" "prev line"
  "r TAB l" "next char"
  "r TAB 0" "till start of line"
  "r TAB $" "till end of line"
  "r TAB {" "till start of para"
  "r TAB }" "till end of para"
  "r TAB (" "till start of sentence"
  "r TAB )" "till end of sentence"
  "r TAB e" "end of word"
  "r TAB b" "start of word"
  "r TAB g g" "start of buffer"
  "r TAB G" "end of buffer")

;; send region
(which-key-add-key-based-replacements
  "r s" "tmux send"
  "r s u" "tmux clear history"
  "r s c" "clone panes"
  "r s m" "run command"
  "r s q" "close runner"
  "r s y" "copy"
  "r s p" "paste from"
  "r s o" "close panes"
  "r s d" "inspect"
  "r s s" "run command"
  "r s -" "split below"
  "r s v" "split right"
  "r s n" "new window"
  "r s x" "interrupt runner"
  "r s z" "zoom runner"
  "r s r" "region"
  "r s i" "inside"
  "r s a" "around"
  "r s g" "global"
  "r s i a" "all"
  "r s a a" "all"
  "r s i w" "word"
  "r s a w" "word"
  "r s i p" "para"
  "r s a p" "para"
  "r s i s" "sentence"
  "r s a s" "sentence"
  "r s i l" "line"
  "r s a l" "line"
  "r s i y" "symbol"
  "r s a y" "symbol"
  "r s i c" "comment"
  "r s a c" "comment"
  "r s i f" "function"
  "r s a f" "function"
  "r s i q" "quote"
  "r s a q" "quote"
  "r s i b" "block/pairs"
  "r s a b" "block/pairs"
  "r s i o" "org code"
  "r s a o" "org code"
  "r s i u" "org subtree"
  "r s a u" "org subtree"
  "r s i e" "latex env"
  "r s a e" "latex env"
  "r s i r" "method call"
  "r s a r" "method call"
  "r s i d" "ruby block"
  "r s a d" "ruby block"
  "r s i h" "diff hunk"
  "r s a h" "diff hunk"
  "r s i x" "latex section"
  "r s a x" "latex section"
  "r s i g" "python string"
  "r s a g" "python string"
  "r s i m" "python block"
  "r s a m" "python block"
  "r s i n" "python statement"
  "r s a n" "python block and dec"
  "r s i $" "latex math"
  "r s a $" "latex math"
  "r s f" "to char"
  "r s F" "to char back"
  "r s t" "till char"
  "r s T" "till char back"
  "r s ;" "find on screen"
  "r s g ;" "till line"
  "r s h" "prev char"
  "r s j" "next line"
  "r s k" "prev line"
  "r s l" "next char"
  "r s 0" "till start of line"
  "r s $" "till end of line"
  "r s {" "till start of para"
  "r s }" "till end of para"
  "r s (" "till start of sentence"
  "r s )" "till end of sentence"
  "r s e" "end of word"
  "r s b" "start of word"
  "r s g g" "start of buffer"
  "r s G" "end of buffer")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; quickly launch and run stuff ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package quickrun
  :ensure t
  :commands (quickrun
			 quickrun-region
			 quickrun-with-arg
			 quickrun-shell
			 quickrun-compile-only
			 quickrun-replace-region))

;; mapping for quickly running some commands
(ryo-modal-keys
 ("c m"
  (("r" quickrun-with-arg)
   ("y" quickrun-replace-region)
   ("o" quickrun-compile-only))))
(sk/ryo-operator-object quickrun "c m" "m" quickrun-region)
(sk/ryo-quickrun-bindings)

;; quickrun
(which-key-add-key-based-replacements
  "c m" "quickrun"
  "c m m" "with arg"
  "c m r" "region"
  "c m y" "replace region"
  "c m o" "compile only"
  "c m i" "inside"
  "c m a" "around"
  "c m g" "global"
  "c m i a" "all"
  "c m a a" "all"
  "c m i w" "word"
  "c m a w" "word"
  "c m i p" "para"
  "c m a p" "para"
  "c m i s" "sentence"
  "c m a s" "sentence"
  "c m i l" "line"
  "c m a l" "line"
  "c m i y" "symbol"
  "c m a y" "symbol"
  "c m i c" "comment"
  "c m a c" "comment"
  "c m i f" "function"
  "c m a f" "function"
  "c m i h" "diff hunk"
  "c m a h" "diff hunk"
  "c m i x" "latex section"
  "c m a x" "latex section"
  "c m i q" "quote"
  "c m a q" "quote"
  "c m i b" "block/pairs"
  "c m a b" "block/pairs"
  "c m i o" "org code"
  "c m a o" "org code"
  "c m i u" "org subtree"
  "c m a u" "org subtree"
  "c m i e" "latex env"
  "c m a e" "latex env"
  "c m i r" "method call"
  "c m a r" "method call"
  "c m i d" "ruby block"
  "c m a d" "ruby block"
  "c m i g" "python string"
  "c m a g" "python string"
  "c m i m" "python block"
  "c m a m" "python block"
  "c m i n" "python statement"
  "c m a n" "python block and dec"
  "c m i $" "latex math"
  "c m a $" "latex math"
  "c m f" "to char"
  "c m F" "to char back"
  "c m t" "till char"
  "c m T" "till char back"
  "c m ;" "find on screen"
  "c m g ;" "till line"
  "c m h" "prev char"
  "c m j" "next line"
  "c m k" "prev line"
  "c m l" "next char"
  "c m 0" "till start of line"
  "c m $" "till end of line"
  "c m {" "till start of para"
  "c m }" "till end of para"
  "c m (" "till start of sentence"
  "c m )" "till end of sentence"
  "c m e" "end of word"
  "c m b" "start of word"
  "c m g g" "start of buffer"
  "c m G" "end of buffer")

;;;;;;;;;;;;;;;
;; pop shell ;;
;;;;;;;;;;;;;;;

(use-package shell-pop
  :ensure t
  :init
  (setq shell-pop-shell-type '("eshell"
							   "*eshell*"
							   (lambda nil (eshell)))))
(ryo-modal-key "g \\" 'shell-pop)
(which-key-add-key-based-replacements
  "g \\" "shell pop")

;;;;;;;;;;;;
;; eshell ;;
;;;;;;;;;;;;

;; eshell in a vertical split
(defun sk/eshell ()
  "eshell vertical split"
  (interactive)
  (split-window-horizontally)
  (other-window 1)
  (eshell))
(bind-key* "C-c u" 'sk/eshell)

(use-package eshell
  :init
  :config
  ;; eshell
  (setq eshell-glob-case-insensitive t
		eshell-scroll-to-bottom-on-input 'this
		eshell-buffer-shorthand t
		eshell-history-size 1024
		eshell-cmpl-ignore-case t
		eshell-prompt-function (lambda () (concat " $ "))
		eshell-aliases-file (concat user-emacs-directory ".eshell-aliases")
		eshell-last-dir-ring-size 512)
  (add-hook 'shell-mode-hook 'goto-address-mode))
(use-package eshell-git-prompt
  :ensure t
  :init
  (eshell-git-prompt-use-theme 'powerline))
(add-hook 'eshell-mode-hook (lambda () (eshell-git-prompt-use-theme 'powerline)))

;;;;;;;;;;;;
;; Shell  ;;
;;;;;;;;;;;;

;; thanks to http://stackoverflow.com/questions/6286579/emacs-shell-mode-how-to-send-region-to-shell
;; send line or region to shell
(defun sk/shell-send-region-or-line (&optional step)
  (interactive)
  (let ((proc (get-process "shell"))
        pbuf min max command)
    (unless proc
      (let ((currbuff (current-buffer)))
        (sk/shell)
        (switch-to-buffer currbuff)
        (setq proc (get-process "shell"))))
    (setq pbuff (process-buffer proc))
    (if (use-region-p)
        (setq min (region-beginning)
              max (region-end))
      (setq min (point-at-bol)
            max (point-at-eol)))
    (setq command (concat (buffer-substring min max) "\n"))
    (with-current-buffer pbuff
      (goto-char (process-mark proc))
      (insert command)
      (move-marker (process-mark proc) (point))) ;;pop-to-buffer does not work with save-current-buffer -- bug?
    (process-send-string  proc command)
    (display-buffer (process-buffer proc) t)
    (when step
      (goto-char max)
      (next-line))))

;; send command to shell
(defun sk/shell-command (arg &optional step)
  (interactive
   (list
	(read-string "Enter the command to send: ")))
  (let ((proc (get-process "shell"))
        pbuf min max command)
    (unless proc
      (let ((currbuff (current-buffer)))
        (sk/shell)
        (switch-to-buffer currbuff)
        (setq proc (get-process "shell"))))
    (setq pbuff (process-buffer proc))
	(setq command (concat arg "\n"))
    (with-current-buffer pbuff
      (goto-char (process-mark proc))
      (insert command)
      (move-marker (process-mark proc) (point))) ;;pop-to-buffer does not work with save-current-buffer -- bug?
    (process-send-string  proc command)
    (display-buffer (process-buffer proc) t)
    (when step
      (goto-char max)
      (next-line))))

(defun sk/shell-send-line-or-region-and-step ()
  (interactive)
  (sk/shell-send-region-or-line t))
(defun sk/shell-switch-to-process-buffer ()
  (interactive)
  (pop-to-buffer (process-buffer (get-process "shell")) t))

(ryo-modal-keys
 ("w r"
  (("z" sk/shell-switch-to-process-buffer)
   ("w" sk/shell-command))))
(sk/ryo-operator-object shell-send "w r" "r" sk/shell-send-line-or-region-and-step t)
(sk/ryo-shell-send-bindings)

;; send region
(which-key-add-key-based-replacements
  "w r" "shell send"
  "w r z" "switch to shell"
  "w r r" "region or line"
  "w r w" "command"
  "w r i" "inside"
  "w r a" "around"
  "w r g" "global"
  "w r i a" "all"
  "w r a a" "all"
  "w r i w" "word"
  "w r a w" "word"
  "w r i p" "para"
  "w r a p" "para"
  "w r i s" "sentence"
  "w r a s" "sentence"
  "w r i l" "line"
  "w r a l" "line"
  "w r i y" "symbol"
  "w r a y" "symbol"
  "w r i c" "comment"
  "w r a c" "comment"
  "w r i f" "function"
  "w r a f" "function"
  "w r i q" "quote"
  "w r a q" "quote"
  "w r i b" "block/pairs"
  "w r a b" "block/pairs"
  "w r i o" "org code"
  "w r a o" "org code"
  "w r i u" "org subtree"
  "w r a u" "org subtree"
  "w r i e" "latex env"
  "w r a e" "latex env"
  "w r i r" "method call"
  "w r a r" "method call"
  "w r i d" "ruby block"
  "w r a d" "ruby block"
  "w r i h" "diff hunk"
  "w r a h" "diff hunk"
  "w r i x" "latex section"
  "w r a x" "latex section"
  "w r i g" "python string"
  "w r a g" "python string"
  "w r i m" "python block"
  "w r a m" "python block"
  "w r i n" "python statement"
  "w r a n" "python block and dec"
  "w r i $" "latex math"
  "w r a $" "latex math"
  "w r f" "to char"
  "w r F" "to char back"
  "w r t" "till char"
  "w r T" "till char back"
  "w r ;" "find on screen"
  "w r g ;" "till line"
  "w r h" "prev char"
  "w r j" "next line"
  "w r k" "prev line"
  "w r l" "next char"
  "w r 0" "till start of line"
  "w r $" "till end of line"
  "w r {" "till start of para"
  "w r }" "till end of para"
  "w r (" "till start of sentence"
  "w r )" "till end of sentence"
  "w r e" "end of word"
  "w r b" "start of word"
  "w r g g" "start of buffer"
  "w r G" "end of buffer")

;;;;;;;;;;
;; Term ;;
;;;;;;;;;;

(require 'term)

;; term mode improve
(defun sk/term-toggle-mode ()
  "Toggles term between line mode and char mode"
  (interactive)
  (if (term-in-line-mode)
      (term-char-mode)
    (term-line-mode)))
(ryo-modal-key "g I" 'sk/term-toggle-mode)
(which-key-add-key-based-replacements
  "g I" "char or line mode")

;; thanks to http://stackoverflow.com/questions/6286579/emacs-shell-mode-how-to-send-region-to-shell
;; send line or region to eshell
(defun sk/term-send-line-or-region (&optional step)
  (interactive)
  (let ((proc (get-process "*ansi-term*"))
        pbuf min max command)
    (unless proc
      (let ((currbuff (current-buffer)))
        (sk/term)
        (switch-to-buffer currbuff)
        (setq proc (get-process "*ansi-term*"))))
    (setq pbuff (process-buffer proc))
    (if (use-region-p)
        (setq min (region-beginning)
              max (region-end))
      (setq min (point-at-bol)
            max (point-at-eol)))
    (setq command (concat (buffer-substring min max) "\n"))
    (with-current-buffer pbuff
      (goto-char (process-mark proc))
      (insert command)
      (move-marker (process-mark proc) (point))) ;;pop-to-buffer does not work with save-current-buffer -- bug?
    (process-send-string  proc command)
    (display-buffer (process-buffer proc) t)
    (when step
      (goto-char max)
      (next-line))))

;; send command to shell
(defun sk/term-command (arg &optional step)
  (interactive
   (list
	(read-string "Enter the command to send: ")))
  (let ((proc (get-process "*ansi-term*"))
        pbuf min max command)
    (unless proc
      (let ((currbuff (current-buffer)))
        (sk/term)
        (switch-to-buffer currbuff)
        (setq proc (get-process "*ansi-term*"))))
    (setq pbuff (process-buffer proc))
	(setq command (concat arg "\n"))
    (with-current-buffer pbuff
      (goto-char (process-mark proc))
      (insert command)
      (move-marker (process-mark proc) (point))) ;;pop-to-buffer does not work with save-current-buffer -- bug?
    (process-send-string  proc command)
    (display-buffer (process-buffer proc) t)
    (when step
      (goto-char max)
      (next-line))))

(defun sk/term-send-line-or-region-and-step ()
  (interactive)
  (sk/term-send-line-or-region t))
(defun sk/term-switch-to-process-buffer ()
  (interactive)
  (pop-to-buffer (process-buffer (get-process "*ansi-term*")) t))

(ryo-modal-keys
 ("w e"
  (("z" sk/term-switch-to-process-buffer)
   ("v" sk/term-command))))
(sk/ryo-operator-object term-send "w e" "r" sk/term-send-line-or-region-and-step t)
(sk/ryo-term-send-bindings)

;; send region
(which-key-add-key-based-replacements
  "w e" "term send"
  "w e z" "switch to shell"
  "w e r" "region or line"
  "w e v" "command"
  "w e i" "inside"
  "w e a" "around"
  "w e g" "global"
  "w e i a" "all"
  "w e a a" "all"
  "w e i w" "word"
  "w e a w" "word"
  "w e i p" "para"
  "w e a p" "para"
  "w e i s" "sentence"
  "w e a s" "sentence"
  "w e i l" "line"
  "w e a l" "line"
  "w e i y" "symbol"
  "w e a y" "symbol"
  "w e i c" "comment"
  "w e a c" "comment"
  "w e i f" "function"
  "w e a f" "function"
  "w e i q" "quote"
  "w e a q" "quote"
  "w e i b" "block/pairs"
  "w e a b" "block/pairs"
  "w e i o" "org code"
  "w e a o" "org code"
  "w e i u" "org subtree"
  "w e a u" "org subtree"
  "w e i e" "latex env"
  "w e a e" "latex env"
  "w e i r" "method call"
  "w e a r" "method call"
  "w e i d" "ruby block"
  "w e a d" "ruby block"
  "w e i h" "diff hunk"
  "w e a h" "diff hunk"
  "w e i x" "latex section"
  "w e a x" "latex section"
  "w e i g" "python string"
  "w e a g" "python string"
  "w e i m" "python block"
  "w e a m" "python block"
  "w e i n" "python statement"
  "w e a n" "python block and dec"
  "w e i $" "latex math"
  "w e a $" "latex math"
  "w e f" "to char"
  "w e F" "to char back"
  "w e t" "till char"
  "w e T" "till char back"
  "w e ;" "find on screen"
  "w e g ;" "till line"
  "w e h" "prev char"
  "w e j" "next line"
  "w e k" "prev line"
  "w e l" "next char"
  "w e 0" "till start of line"
  "w e $" "till end of line"
  "w e {" "till start of para"
  "w e }" "till end of para"
  "w e (" "till start of sentence"
  "w e )" "till end of sentence"
  "w e e" "end of word"
  "w e b" "start of word"
  "w e g g" "start of buffer"
  "w e G" "end of buffer")

;; provide the shell settings
(provide 'sk-shell)
