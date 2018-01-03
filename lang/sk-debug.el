;; realgud - improved debugger
(use-package realgud
  :ensure t
  :commands
  (realgud:backtrace-describe
   realgud:backtrace-init
   realgud:bashdb
   realgud:bashdb-customize
   realgud:cmd-backtrace
   realgud:cmd-break
   realgud:cmd-clear
   realgud:cmd-continue
   realgud:cmd-delete
   realgud:cmd-disable
   realgud:cmd-enable
   realgud:cmd-eval
   realgud:cmd-eval-at-point
   realgud:cmd-eval-dwim
   realgud:cmd-eval-region
   realgud:cmd-finish
   realgud:cmd-frame
   realgud:cmd-jump
   realgud:cmd-kill
   realgud:cmd-newer-frame
   realgud:cmd-next
   realgud:cmd-next-no-arg
   realgud:cmd-older-frame
   realgud:cmd-quit
   realgud:cmd-repeat-last
   realgud:cmd-restart
   realgud:cmd-shell
   realgud:cmd-step
   realgud:cmd-step-no-arg
   realgud:cmd-terminate
   realgud:cmd-until
   realgud:cmdbuf-follow-buffer
   realgud:cmdbuf-info-describe
   realgud:flake8-goto-msg-line
   realgud:follow
   realgud:follow-event
   realgud:follow-point
   realgud:gdb
   realgud:gdb-customize
   realgud:gdb-pid
   realgud:gdb-pid-associate
   realgud:gdb-reset
   realgud:gdb-track-mode
   realgud:goto-debugger-backtrace-line
   realgud:goto-debugger-loc-line
   realgud:goto-lang-backtrace-line
   realgud:goto-loc-hist-4
   realgud:goto-loc-hist-5
   realgud:goto-loc-hist-6
   realgud:goto-loc-hist-7
   realgud:goto-loc-hist-8
   realgud:goto-loc-hist-9
   realgud:goto-maven-errmsg-line
   realgud:gub-customize
   realgud:gub-goto-location
   realgud:gub-goto-panic-location
   realgud:info-mode
   realgud:ipdb
   realgud:ipdb-backend-complete
   realgud:ipdb-customize
   realgud:ipdb-remote
   realgud:jdb
   realgud:jdb-customize
   realgud:jdb-goto-control-frame-line
   realgud:jdb-goto-syntax-error-line
   realgud:jdb-track-mode
   realgud:kshdb
   realgud:kshdb-customize
   realgud:loc-describe
   realgud:nodejs
   realgud:nodejs-customize
   realgud:nodejs-reset
   realgud:pdb-customize
   realgud:pdb-delayed
   realgud:pdb-remote
   realgud:perl-goto-errmsg-line
   realgud:perldb
   realgud:perldb-customize
   realgud:perldb-reset
   realgud:pytest-goto-errmsg-line
   realgud:rails-goto-backtrace-line
   realgud:rdebug
   realgud:rdebug-customize
   realgud:reload-features
   realgud:remake
   realgud:remake-customize
   realgud:remove-ansi-schmutz
   realgud:reset
   realgud:rspec-goto-backtrace-line
   realgud:rubinius-goto-Xagent-backtrace-line
   realgud:ruby-goto-backtrace-line
   realgud:ruby-goto-dollar-bang-line
   realgud:send-input
   realgud:srcbuf-info-describe
   realgud:terminate
   realgud:terminate-srcbuf
   realgud:tooltip-eval
   realgud:track-from-region
   realgud:track-mode-disable
   realgud:track-mode-enable
   realgud:track-set-debugger
   realgud:trepan
   realgud:trepan-customize
   realgud:trepan-goto-control-frame-line
   realgud:trepan-goto-syntax-error-line
   realgud:trepan.pl
   realgud:trepan2
   realgud:trepan2-customize
   realgud:trepan2-delayed
   realgud:trepan3k
   realgud:trepan3k-customize
   realgud:trepan3k-delayed
   realgud:trepanjs
   realgud:trepanjs-customize
   realgud:trepanjs-goto-syntax-error-line
   realgud:trepanjs-reset
   realgud:trepanpl-customize
   realgud:trepanpl-goto-syntax-error-line
   realgud:trepanpl-reset
   realgud:trepanpl-track-mode
   realgud:window-bt
   realgud:window-bt-undisturb-src
   realgud:zshdb
   realgud:zshdb-customize
realgud:pdb))

(defun sk/realgud-backtrace ()
  "realgud backtrace"
  (interactive)
  (require 'realgud)
  (realgud:cmd-backtrace))



;; provide debugging configuration
(provide 'sk-debug)