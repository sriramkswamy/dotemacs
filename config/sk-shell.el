;; send stuff to any buffer
(add-to-list 'load-path (concat user-emacs-directory "lisp/isend-mode"))
(require 'isend)
(general-nvmap :prefix sk--evil-global-leader
	       "[" 'isend-associate
	       "]" 'isend-send)

;; interact with tmux
(use-package emamux
  :ensure t
  :commands (emamux:send-command
	     emamux:run-command
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
(defhydra hydra-emamux (:color red
			:hint nil)
  "
 ^Command^    ^Runner^                          ^Clipboard^
^^^^^^^^^^-----------------------------------------------------------------
 _s_: send    _r_: run        _c_: close          _y_: copy kill    _q_: quit
	    _l_: last cmd   _C_: close other    _p_: paste tmux
	    _z_: zoom       _h_: clear hist
	    _i_: inspect    _I_: interrupt
"
  ("s" emamux:send-command)
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
  ("q" nil :color blue))
(general-nvmap :prefix sk--evil-global-leader
	       "x" 'hydra-emamux/body)

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
(general-nvmap :prefix sk--evil-global-leader
	       "m" 'hydra-quickrun/body)

;; provide the shell settings
(provide 'sk-shell)
