;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Regular bindings    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ryo modal digit mappings
(ryo-modal-keys
   (:norepeat t)
   ("0" "M-0" :name "0")
   ("1" "M-1" :name "1")
   ("2" "M-2" :name "2")
   ("3" "M-3" :name "3")
   ("4" "M-4" :name "4")
   ("5" "M-5" :name "5")
   ("6" "M-6" :name "6")
   ("7" "M-7" :name "7")
   ("8" "M-8" :name "8")
   ("9" "M-9" :name "9"))

;; ryo modal emacs emulation
(ryo-modal-keys
  (:norepeat t)
  (":" "C-x" :name "C-x maps")
  ("\"" "C-c" :name "C-c maps")
  ("\\" "C-u" :name "C-u maps"))

;; ryo modal general mappings
(ryo-modal-keys
  (:norepeat t)
  ("z z" recenter-top-bottom :name "recenter")
  ("+" bookmark-set :name "add bookmark")
  ("Z" sk/toggle-frame-fullscreen-non-native :name "fullscreen")
  ("?" which-key-show-top-level :name "which key"))

;; ryo modal navigation mappings
(ryo-modal-keys
  (:norepeat t)
  ("DEL" mode-line-other-buffer :name "last buffer")
  ("W" dired-jump :name "open dir"))

;; ryo modal insert editing maps
(ryo-modal-keys
  ("i" sk/disable-ryo-modal-mode :name "insert")
  ("a" forward-char :name "append" :exit t)
  ("I" sk/smarter-move-beginning-of-line :name "insert start of line" :exit t)
  ("A" move-end-of-line :name "append end of line" :exit t))

;; ryo modal selection maps
(ryo-modal-keys
  (:norepeat t)
  ("X" "C-x C-x" :name "prev selection/exchange selection")
  ("V" "C-SPC" :name "start select"))

;; ryo modal editing maps
(ryo-modal-keys
  ("x" "C-d" :name "delete char")
  ("u" "C-/" :name "undo" :norepeat t)
  ("U" "M-/" :name "redo" :norepeat t))

;; ryo modal macro maps to not repeat
(ryo-modal-keys
  (:norepeat t)
  ("q" "C-x (" :name "start macro")
  ("Q" "C-x )" :name "stop macro")
  ("@" kmacro-end-or-call-macro-repeat :name "call macro")
  ("g @" kmacro-end-or-call-macro-repeat :name "name macro")
  ("g ~" kmacro-bind-to-key :name "map macro")
  ("g q" "C-x q" :name "query macro"))

;; ryo modal movement maps
(ryo-modal-keys
  (:norepeat t)
  ("G" "M->" :name "end of buffer")
  ("0" "C-a" :name "start of line")
  ("$" "C-e" :name "end of line")
  ("H" "C-x >" :name "scroll left")
  ("J" "C-v" :name "scroll down")
  ("K" "M-v" :name "scroll up")
  ("L" "C-x <" :name "scroll right")
  ("h" "C-b" :name "prev char")
  ("j" "C-n" :name "next line")
  ("k" "C-p" :name "prev line")
  ("l" "C-f" :name "next char"))

;; ryo modal search mappings
(ryo-modal-keys
  (:norepeat t)
  ("/" "C-s" :name "search in buffer"))

;; ryo modal window navigation maps
(ryo-modal-keys
  (:norepeat t)
  ("w a" switch-to-buffer-other-window :name "switch other window")
  ("w d" clone-indirect-buffer-other-window :name "clone window")
  ("w z" sk/recenter-other-window :name "recenter other window")
  ("w h" windmove-left :name "focus left")
  ("w j" windmove-down :name "focus down")
  ("w k" windmove-up :name "focus up")
  ("w l" windmove-right :name "focus right")
  ("w [" shrink-window-horizontally :name "reduce width")
  ("w {" shrink-window :name "reduce height")
  ("w }" enlarge-window :name "increase height")
  ("w ]" enlarge-window-horizontally :name "increase width")
  ("w x" sk/rotate-windows :name "exchange")
  ("w q" delete-window :name "close")
  ("w o" delete-other-windows :name "only window")
  ("w f" find-file-other-window :name "find file other window")
  ("w v" sk/split-right-and-move :name "vertical split")
  ("w s" sk/split-below-and-move :name "split horizontal")
  ("w =" balance-windows :name "balance windows")
  ("w _" widen :name "widen window")
  ("w w" "C-x o" :name "other window"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Leader bindings    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; mapping with leader
(ryo-modal-key "SPC"
               '(("w" "C-x C-s" :name "save buffer" :norepeat t)
                 ("e" magit-status :name "git status" :norepeat t)
                 ("f" "C-x C-f" :name "open file" :norepeat t)
                 ("g" "C-g" :name "interrupt" :norepeat t)
                 ("h" "C-h" :name "help" :norepeat t)
                 ("y" "M-y" :name "copy history" :norepeat t)
                 ("k" "C-x b" :name "switch buffer" :norepeat t)
				 ("m" sk/ediff-dwim :name "diff" :norepeat t)
				 ("x" ibuffer :name "interactive buffer" :norepeat t)
                 ("j" "M-x" :name "commands" :norepeat t)))

;; mapping with global prefix
(ryo-modal-key "g"
               '(("A" describe-char :name "describe char")
				 ("b" magit-blame :name "git blame")
				 ("f" ffap :name "find file at point")
				 ("W" wdired-change-to-wdired-mode :name "writeable dir")
				 ("O" package-install :name "install package")
				 ("`" async-shell-command :name "run shell command")
				 ("M" list-packages :name "list packages")
				 ("V" sk/browse-current-file :name "view file in browser")
				 ("N" sk/rename-current-buffer-file :name "rename file")
				 ("K" sk/delete-current-buffer-file :name "remove file")
				 ("Y" sk/copy-current-file-path :name "copy file path")
                 ("." sk/duplicate-current-line-or-region :name "duplicate line/region")
                 ("g" "M-<" :name "start of buffer")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Operator/Text-object bindings    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; text object/operator relationship
(let ((text-objects
        '(;; single-char style text object
          ("f" sk/mark-to-char :name "to char")
          ("F" sk/mark-to-char-backward :name "to char back")
          ("t" sk/mark-up-to-char :name "till char")
          ("T" sk/mark-up-to-char-backward :name "till char back")
          ;; inner-around style text object
          ("i w" er/mark-word :name "word")
          ("a w" sk/mark-around-word :name "word")
          ("a a" mark-whole-buffer :name "all")
          ("i a" mark-whole-buffer :name "all"))))
(eval `(ryo-modal-keys
        ;; complex operators
        ("c y" ,text-objects :then '(sk/cut-region-or-line-to-clipboard) :exit t)
        ("d y" ,text-objects :then '(sk/cut-region-or-line-to-clipboard))
        ;; basic operators
        ("v" ,text-objects)
        ("c" ,text-objects :then '(kill-region) :exit t)
        ("d" ,text-objects :then '(kill-region))
        ("y" ,text-objects :then '(copy-region-as-kill)))))

;; capital versions of operators
(if (fboundp 'sp-kill-hybrid-sexp)
    (ryo-modal-keys
        ("C" sp-kill-hybrid-sexp :name "change rest of line" :exit t)
        ("D" sp-kill-hybrid-sexp :name "delete rest of line"))
  (ryo-modal-keys
    ("C" kill-line :name "change rest of line" :exit t)
    ("D" kill-line :name "change rest of line")))
(ryo-modal-key "Y" 'sk/copy-to-end-of-line :name "copy rest of line")

;; repeating style operator mappings
(ryo-modal-keys
  ;; complex operator repeats
  ("c y y" sk/cut-region-or-line-to-clipboard :name "line/region" :exit t)
  ("d y y" sk/cut-region-or-line-to-clipboard :name "line/region")
  ;; basic operator repeats
  ("c c" sk/kill-region-or-backward-word :name "line/region" :exit t)
  ("d d" sk/kill-region-or-backward-word :name "line/region")
  ("y y" sk/copy-region-or-line :name "line/region"))

;; provide ryo bindings
(provide 'sk-ryo-bindings)
