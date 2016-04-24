;;; sk-fun.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; For absolutely random things

;;; Code:

;; Jabber
(sk/require-package 'jabber)
(setq jabber-history-enabled t
      jabber-activity-mode nil
      jabber-use-global-history nil
      jabber-backlog-number 40
      jabber-backlog-days 30)
(setq jabber-alert-presence-message-function
      (lambda (who oldstatus newstatus statustext) nil))
;; Account settings
(setq jabber-account-list
      '(("sriram.krish.92@gmail.com"
         (:network-server . "talk.google.com")
         (:connection-type . ssl))))

;; XKCD
(sk/require-package 'xkcd)
(global-set-key (kbd "C-c v g x") 'xkcd)
(modalka-define-kbd "g x" "C-c v g x")
(which-key-add-key-based-replacements "g x" "xkcd")

(provide 'sk-fun)
;;; sk-fun.el ends here
