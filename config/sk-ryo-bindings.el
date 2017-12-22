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
