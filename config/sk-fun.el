;;; sk-fun.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; For absolutely random things

;;; Code:

;; Jabber
(use-package jabber
  :ensure t
  :commands (jabber-connect)
  :init
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
	   (:connection-type . ssl)))))

;; XKCD
(use-package xkcd
  :ensure t
  :commands (xkcd)
  :bind (
	 ("C-c v g x" . xkcd)
	 ))
(modalka-define-kbd "g x" "C-c v g x")
(which-key-add-key-based-replacements "g x" "xkcd")

;; Rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :defer 2)

;; IRC chat
(use-package circe
  :ensure t
  :commands (circe))

;; Ledger for managing accounts
(use-package ledger-mode
  :ensure t
  :mode "\\.dat$")

;; Stackexchange from org
(use-package sx
  :ensure t
  :commands (sx-tab-all-questions)
  :bind (
	 ("C-c v g X" . sx-tab-all-questions)
	 ))
(modalka-define-kbd "g X" "C-c v g X")
(which-key-add-key-based-replacements "g X" "stack exchange")

;; Typing exercies
(use-package typit
  :ensure t
  :commands (typit))

;; My key frquency
(use-package keyfreq
  :ensure t
  :init
  (setq keyfreq-excluded-commands
	'(self-insert-command
	  org-self-insert-command
	  company-ignore
	  abort-recursive-edit
	  forward-char
	  modalka-mode
	  backward-char
	  previous-line
	  next-line))
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;; Google stuff
(use-package google-this
  :ensure t
  :commands (google-this-word
	     google-this-region
	     google-this-symbol
	     google-this-clean-error-string
	     google-this-line
	     google-this-search
	     google-this-cpp-reference))
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
