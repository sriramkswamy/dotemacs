;; Call the terminal
(defun sk/call-terminal ()
  (interactive)
  (shell-command "open -a /Applications/Utilities/Terminal.app"))
(bind-key* "C-c :" 'sk/call-terminal)

;; interact with tmux
(use-package emamux
  :ensure t
  :commands (emamux:send-command
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
;; hydra for interacting with this
(defhydra hydra-emamux (:color red :hint nil)
  "
 ^Command^       ^Runner^                          ^Clipboard^       ^tmux^
^^^^^^^^^^----------------------------------------------------------------------------------------------
 _s_: region     _r_: run        _c_: close          _y_: copy kill    _w_: new window        _q_: quit
 _S_: command    _l_: last cmd   _C_: close other    _p_: paste tmux   _f_: clone frame
			   _z_: zoom       _h_: clear hist                     _v_: split vertically
			   _i_: inspect    _I_: interrupt                      _V_: split horizontally
"
  ("s" emamux:run-region)
  ("S" emamux:send-command)
  ("r" emamux:run-command)
  ("l" emamux:run-last-command)
  ("z" emamux:zoom-runner)
  ("i" emamux:inspect-runner)
  ("c" emamux:close-runner-pane)
  ("C" emamux:close-panes)
  ("h" emamux:clear-runner-history)
  ("I" emamux:interrupt-runner)
  ("y" emamux:copy-kill-ring)
  ("p" emamux:yank-from-list-buffers)
  ("w" emamux:new-window)
  ("f" emamux:clone-current-frame)
  ("v" emamux:split-window-horizontally)
  ("V" emamux:split-window)
  ("q" nil :color blue))
(bind-key* "C-c ;" 'hydra-emamux/body)
;; zoom into the tmux pane (tmux > 1.8)
;; tmux resize-pane -Z
(defun sk/zoom-tmux ()
  (interactive)
  (shell-command "tmux resize-pane -Z"))
(bind-key* "C-c z" 'sk/zoom-tmux)

;; quickly launch and run stuff
(use-package quickrun
  :ensure t
  :commands (quickrun
             quickrun-region
             quickrun-with-arg
             quickrun-shell
             quickrun-compile-only
             quickrun-replace-region))
;; hydra for quickrun
(defhydra hydra-quickrun (:color blue
                          :hint nil)
  "
 _s_: quickrun   _r_: run region         _a_: with arg      _q_: quit
 _S_: shell      _R_: replace region     _c_: compile only
"
  ("s" quickrun)
  ("S" quickrun-shell)
  ("r" quickrun-region)
  ("R" quickrun-replace-region)
  ("a" quickrun-with-arg)
  ("c" quickrun-compile-only)
  ("q" nil :color blue))
(bind-key* "C-c q" 'hydra-quickrun/body)

;; provide the shell settings
(provide 'sk-shell)
