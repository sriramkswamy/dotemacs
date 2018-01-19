;; ansi-term behave like shell-mode by default
(defun sk/term ()
  "calls ansi term that behaves like shell mode"
  (interactive)
  (split-window-sensibly)
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

;; Shell auto completion
(use-package company-shell
  :ensure t
  :hook ((shell-mode-hook . sk/company-shell))
  :bind* (("C-j s" . company-shell))
  :bind (:map shell-mode-map
			  ("C-d" . company-shell))
  :bind (:map eshell-mode-map
			  ("C-d" . company-shell)))

;; zoom into the tmux pane (tmux > 1.8)
;; tmux resize-pane -Z
(defun sk/zoom-tmux ()
  (interactive)
  (shell-command "tmux resize-pane -Z"))

;;;;;;;;;;;
;; Tmux  ;;
;;;;;;;;;;;

;; interact with tmux
(use-package emamux
  :ensure t
  ;; :load-path "site-lisp/emamux/"
  :commands
  (emamux:send-command
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; quickly launch and run stuff ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package quickrun
  :ensure t
  :commands
  (quickrun
   quickrun-region
   quickrun-with-arg
   quickrun-shell
   quickrun-compile-only
   quickrun-replace-region))

;;;;;;;;;;;;
;; eshell ;;
;;;;;;;;;;;;

;; eshell in a vertical split
(defun sk/eshell ()
  "eshell vertical split"
  (interactive)
  (split-window-sensibly)
  (other-window 1)
  (eshell))

(use-package eshell
  :hook ((shell-mode-hook . sk/company-shell)
		 (shell-mode . goto-address-mode))
  :init
  ;; eshell
  (setq eshell-glob-case-insensitive t
		eshell-scroll-to-bottom-on-input 'this
		eshell-buffer-shorthand t
		eshell-history-size 1024
		eshell-cmpl-ignore-case t
		eshell-prompt-function (lambda () (concat " $ "))
		eshell-aliases-file (concat user-emacs-directory ".eshell-aliases")
		eshell-last-dir-ring-size 512))

;; https://emacs.stackexchange.com/questions/7617/how-to-programmatically-execute-a-command-in-eshell
(defun sk/eshell-send ()
  "send region to eshell"
  (interactive)
  (setq command "")
  (setq command (buffer-substring (region-beginning) (region-end)))
  (with-current-buffer "*eshell*"
	  (eshell-return-to-prompt)
	  (insert command)
	  (kill-line)
	  (insert "\n")
	  (eshell-send-input)))

(defun sk/eshell-send-region-or-line ()
  "send current line or region to eshell buffer"
  (interactive)
  (if (region-active-p)
	  (sk/eshell-send)
	(sk/select-inside-line)
	(sk/eshell-send)))

(defun sk/eshell-command (arg)
  "send a command to eshell buffer"
  (interactive
   (list
	(read-string "Enter the command to send: ")))
  (with-current-buffer "*eshell*"
	(eshell-return-to-prompt)
	(insert (concat arg "\n"))
	(eshell-send-input)))

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

;; thanks to http://stackoverflow.com/questions/6286579/emacs-shell-mode-how-to-send-region-to-shell
;; send line or region to ansi-term
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

;; provide the shell settings
(provide 'sk-shell)
