;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Regular bindings    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ryo modal emacs emulation
(ryo-modal-keys
  (:norepeat t)
  (":" "C-x" :name "C-x maps")
  ("\"" "C-c" :name "C-c maps"))

;; ryo modal general mappings
(ryo-modal-keys
  (:norepeat t)
  ("?" which-key-show-top-level :name "which key"))

;; ryo modal editing maps to not repeat
(ryo-modal-keys
  (:norepeat t)
  ("i" sk/disable-ryo-modal-mode :name "insert")
  ("u" undo :name "undo"))

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
