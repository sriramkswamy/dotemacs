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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Operator/Text-object bindings    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; text object/operator relationship
(let ((text-objects
        '(
          ("a" mark-whole-buffer :name "all")
        )))
(eval `(ryo-modal-keys
        ("c" ,text-objects :then '(kill-region) :exit t)
        ("d" ,text-objects :then '(kill-region))
        ("y" ,text-objects :then '(copy-region-as-kill))
        ("v" ,text-objects))))

;; provide ryo bindings
(provide 'sk-ryo-bindings)
