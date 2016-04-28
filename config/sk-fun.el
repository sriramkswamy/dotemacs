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

;; Rainbow delimiters
(sk/require-package 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; IRC chat
(sk/require-package 'circe)

;; Ledger for managing accounts
(sk/require-package 'ledger-mode)

;; Stackexchange from org
(sk/require-package 'sx)

;; Fireplace
(sk/require-package 'fireplace)

;; RSS feed
(sk/require-package 'elfeed)

;; Typing exercies
(sk/require-package 'typit)

;; Google stuff
(sk/require-package 'google-this)
;; Hydra google
(defhydra sk/hydra-google (:color blue
                           :hint nil)
  "
 _w_: word   _r_: region    _v_: symbol   _l_: line
 _g_: google _c_: cpp       _s_: string   _q_: quit
 "
  ("w" google-this-word)
  ("r" google-this-region)
  ("v" google-this-symbol)
  ("s" google-this-clean-error-string)
  ("l" google-this-line)
  ("g" google-this-search)
  ("c" google-this-cpp-reference)
  ("q" nil :color blue))
(global-set-key (kbd "C-c v g G") 'sk/hydra-google/body)
(modalka-define-kbd "g n" "C-c v g G")
(which-key-add-key-based-replacements "g n" "google now")

(provide 'sk-fun)
;;; sk-fun.el ends here
