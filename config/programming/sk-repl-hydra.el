;;; sk-repl-hydra.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; For REPL hydras

;;; Code:

;; Hydra for emamux
(defhydra sk/hydra-for-emamux (:color red
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

;; Binding
(global-set-key (kbd "C-c h t") 'sk/hydra-for-emamux/body)

;; modal binding
(modalka-define-kbd "c t" "C-c h t")

(which-key-add-key-based-replacements
  "c t" "code tmux")


(provide 'sk-repl-hydra)
;;; sk-repl-hydra.el ends here
