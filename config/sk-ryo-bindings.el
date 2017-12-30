;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Regular bindings    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ryo modal digit mappings
(ryo-modal-keys
 (:norepeat t)
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
 ("`" "C-u" :name "C-u maps"))

;; ryo modal general mappings
(ryo-modal-keys
 ("SPC SPC" recenter-top-bottom :name "recenter")
 ("+" sk/counsel-bookmarks :name "add/see bookmarks" :norepeat t)
 ("H" "C-h" :name "help" :norepeat t)
 ("Z" sk/toggle-frame-fullscreen-non-native :name "fullscreen" :norepeat t)
 ("?" which-key-show-top-level :name "which key" :norepeat t))

;; ryo modal navigation mappings
(ryo-modal-keys
 (:norepeat t)
 ("n" "M-n" :name "next history")
 ("N" "M-p" :name "previous history")
 ("[" flycheck-previous-error :name "previous error")
 ("]" flycheck-next-error :name "next error")
 ("g z" flyspell-correct-previous-word-generic :name "correct spelling")
 ("g [" xref-find-references :name "xref references")
 ("g ]" xref-find-definitions :name "xref definitions")
 ("g \\" xref-pop-marker-stack :name "xref back")
 ("g (" ggtags-create-tags :name "create tags")
 ("g )" ggtags-update-tags :name "update tags")
 ("s s" hydra-smartparens/body :name "smartparens")
 ("DEL" mode-line-other-buffer :name "last buffer")
 ("TAB" vimish-fold-toggle :name "toggle folds")
 ("W" dired-jump :name "open dir"))

;; ryo modal insert editing maps
(ryo-modal-keys
 ("i" sk/disable-ryo-modal-mode :name "insert")
 ("a" forward-char :name "append" :exit t)
 ("o" sk/open-line-below :name "open" :exit t)
 ("O" sk/open-line-above :name "open up" :exit t)
 ("I" sk/smarter-move-beginning-of-line :name "insert start of line" :exit t)
 ("A" move-end-of-line :name "append end of line" :exit t))

;; ryo modal selection maps
(ryo-modal-keys
 (:norepeat t)
 ("X" "C-x C-x" :name "prev selection/exchange selection")
 ("V" "C-SPC" :name "start select"))

;; ryo modal editing maps
(ryo-modal-keys
 ("p" "C-y" :name "paste")
 ("P" yank-rectangle :name "paste rectangle")
 ("S" embrace-commander :name "surround")
 ("x" delete-char :name "delete char")
 ("~" sk/toggle-letter-case :name "toggle case")
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
 ("e" "M-f" :name "next word")
 ("b" "M-b" :name "prev word")
 ("{" "M-{" :name "next para")
 ("}" "M-}" :name "prev para")
 ("h" "C-b" :name "prev char")
 ("j" "C-n" :name "next line")
 ("k" "C-p" :name "prev line")
 ("l" "C-f" :name "next char"))

;; ryo modal motion maps
(ryo-modal-keys
 (:norepeat t)
 ("f" iy-go-to-char :name "to char")
 ("F" iy-go-to-char-backward :name "to char back")
 (";" iy-go-to-or-up-to-continue :name "continue char")
 ("," iy-go-to-or-up-to-continue-backward :name "continue char back")
 ("'" avy-goto-line :name "goto line"))

;; ryo modal narrowing maps
(ryo-modal-keys
 (:norepeat t)
 ("t" sk/counsel-imenu-org-goto :name "tags")
 ("T" ggtags-find-tag-regexp :name "find tags")
 ("L" ivy-resume :name "last narrowing")
 ("-" ivy-push-view :name "create view")
 ("_" ivy-pop-view :name "delete view"))

;; ryo modal bibliography mappings
(ryo-modal-keys
 (:norepeat t)
 ("B" sk/default-bib :name "default bib")
 ("g B" sk/current-bib :name "bib in project"))

;; ryo modal search mappings
(ryo-modal-keys
 (:norepeat t)
 ("*" sk/swiper-at-point :name "search word under cursor")
 ("#" sk/counsel-ag-project-at-point :name "grep word under cursor")
 ("E" ivy-occur :name "edit search results")
 ("g *" counsel-colors-emacs :name "search emacs colors")
 ("g #" counsel-colors-web :name "search web colors")
 ("g x" wgrep-save-all-buffers :name "save edited grep results")
 ("g X" wgrep-abort-changes :name "abort edited grep results")
 ("g E" ivy-wgrep-change-to-wgrep-mode :name "edit grep results")
 ("g s" sk/counsel-ag-directory :name "search dir")
 ("g G" counsel-git-log :name "git log")
 ("SPC ," counsel-descbinds :name "bindings" :norepeat t)
 ("SPC ." counsel-load-theme :name "themes" :norepeat t)
 ("J" dumb-jump-go :name "dumb jump")
 ("K" dash-at-point-with-docset :name "dash doc")
 ("/" "C-s" :name "search in buffer"))

;; ryo scroll maps
(ryo-modal-keys
 ("g j" diff-hl-next-hunk :name "next diff")
 ("g k" diff-hl-previous-hunk :name "previous diff")
 ("s p" sk/other-window-up :name "scroll up other window")
 ("s n" scroll-other-window :name "scroll down other window")
 ("s f" sk/other-doc-down :name "next page other window")
 ("s b" sk/other-doc-up :name "previous page other window")
 ("s a" sk/doc-revert-other-window :name "revert pdf other window")
 ("s o" sk/other-doc-fit :name "fit pdf other window")
 ("s u" sk/interleave-other-window-previous :name "interleave up other window")
 ("s d" sk/interleave-other-window-next :name "interleave down other window")
 ("s i" sk/interleave-other-window-note :name "interleave note")
 ("s h" "C-x >" :name "scroll left")
 ("s j" "C-v" :name "scroll down")
 ("s k" "M-v" :name "scroll up")
 ("s l" "C-x <" :name "scroll right"))

;; ryo modal repl maps
(ryo-modal-keys
 (:norepeat t)
 ("r m" sk/call-terminal :name "terminal")
 ("r n" sk/shell :name "shell")
 ("r v" sk/term :name "term")
 ("r y" sk/eshell :name "eshell"))

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

;; ryo modal mark navigation
(ryo-modal-keys
 (:norepeat t)
 ("g m" counsel-mark-ring :name "search marks")
 ("<down>" back-button-local-forward :name "local mark next")
 ("<up>" back-button-local-backward :name "local mark prev")
 ("<right>" back-button-global-forward :name "global mark next")
 ("<left>" back-button-global-backward :name "global mark prev"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Leader bindings    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; mapping with leader
(ryo-modal-key "SPC"
               '(("w" "C-x C-s" :name "save buffer" :norepeat t)
                 ("e" magit-status :name "git status" :norepeat t)
                 ("f" "C-x C-f" :name "open file" :norepeat t)
                 ("g" "C-g" :name "interrupt" :norepeat t)
                 ("r" counsel-recentf :name "recent files" :norepeat t)
                 ("d" sk/counsel-fzf-project :name "project files" :norepeat t)
                 ("s" spotlight :name "desktop files" :norepeat t)
                 ("p" sk/counsel-ag-project :name "grep project" :norepeat t)
                 ("a" ivy-switch-view :name "switch views" :norepeat t)
                 ("l" flycheck-list-errors :name "list errors" :norepeat t)
                 ("y" "M-y" :name "copy history" :norepeat t)
                 ("k" "C-x b" :name "switch buffer" :norepeat t)
				 ("x" ibuffer :name "interactive buffer" :norepeat t)
				 ("n" treemacs-toggle :name "file tree" :norepeat t)
				 ("z" prodigy :name "background services" :norepeat t)
                 ("j" "M-x" :name "commands" :norepeat t)))

;; mapping with global prefix
(ryo-modal-key "g"
               '(("A" describe-char :name "describe char" :norepeat t)
                 ("D" dash-at-point-with-docset :name "dash doc" :norepeat t)
                 ("S" electric-newline-and-maybe-indent :name "split line")
                 ("J" join-line :name "join line")
				 ("b" magit-blame :name "git blame" :norepeat t)
				 ("T" git-timemachine-toggle :name "git time machine" :norepeat t)
				 ("P" sk/post-gist :name "gist post" :norepeat t)
				 ("R" browse-at-remote :name "git remote" :norepeat t)
				 ("f" ffap :name "find file at point" :norepeat t)
				 ("W" wdired-change-to-wdired-mode :name "writeable dir" :norepeat t)
				 ("O" package-install :name "install package" :norepeat t)
				 ("`" async-shell-command :name "run shell command" :norepeat t)
				 ("M" list-packages :name "list packages" :norepeat t)
				 ("V" sk/browse-current-file :name "view file in browser" :norepeat t)
				 ("N" sk/rename-current-buffer-file :name "rename file" :norepeat t)
				 ("K" sk/delete-current-buffer-file :name "remove file" :norepeat t)
				 ("Y" sk/copy-current-file-path :name "copy file path")
                 ("." sk/duplicate-current-line-or-region :name "duplicate line/region")
                 ("C" string-inflection-all-cycle :name "change case")
                 (";" goto-last-change :name "last change")
                 ("," goto-last-change-reverse :name "last change reverse")
                 ("F" "C-c C-f" :name "follow mode")
                 ("SPC" "C-c C-c" :name "dwim")
                 ("RET" flycheck-select-checker :name "select linters")
                 ("TAB" flycheck-verify-setup :name "verify linting")
                 ("g" "M-<" :name "start of buffer" :norepeat t)))

;; mapping with mode leader
(ryo-modal-key "m"
               '(("SPC" org-store-link :name "org store link" :norepeat t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Operator/Text-object bindings    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; text object/operator relationship
(let ((text-objects
       '(;; single-char style text object
         ("h" sk/mark-previous-char :name "previous char(s)")
         ("j" sk/mark-next-line :name "next line(s)")
         ("k" sk/mark-previous-line :name "previous line(s)")
         ("l" sk/mark-next-char :name "next char(s)")
         ("b" sk/mark-backward-word :name "to start of word")
         ("e" mark-word :name "to end of word")
         ("{" sk/mark-backward-para :name "to start of para")
         ("}" sk/mark-forward-para :name "to end of para")
         ("g g" sk/mark-beginning-buffer :name "to beginning of buffer")
         ("G" sk/mark-end-buffer :name "to end of buffer")
         ("f" sk/mark-to-char :name "to char")
         ("F" sk/mark-to-char-backward :name "to char back")
         ("t" sk/mark-up-to-char :name "till char")
         ("T" sk/mark-up-to-char-backward :name "till char back")
         (";" sk/mark-continue :name "continue")
         ("," sk/mark-continue-backward :name "continue back")
         ("'" sk/mark-avy-goto-line :name "till line")
         ;; inner-around style text object
         ("i w" er/mark-word :name "word")
         ("a w" sk/mark-around-word :name "word")
         ("i h" diff-hl-mark-hunk :name "diff")
         ("a h" diff-hl-mark-hunk :name "diff")
         ("i p" er/mark-text-paragraph :name "para")
         ("a p" mark-paragraph :name "para")
         ("i l" sk/select-inside-line :name "line")
         ("a l" sk/select-around-line :name "line")
         ("i s" er/mark-text-sentence :name "sentence")
         ("a s" er/mark-text-sentence :name "sentence")
         ("i y" er/mark-symbol :name "symbol")
         ("a y" sk/mark-around-symbol :name "symbol")
         ("i c" er/mark-comment :name "comment")
         ("a c" er/mark-comment :name "comment")
         ("i f" er/mark-defun :name "function")
         ("a f" er/mark-defun :name "function")
         ("i q" er/mark-inside-quotes :name "quotes")
         ("a q" er/mark-outside-quotes :name "quotes")
         ("i o" sk/mark-inside-org-code :name "org code")
         ("a o" er/mark-org-code-block :name "org code")
         ("i u" sk/mark-inside-subtree :name "subtree")
         ("a u" org-mark-subtree :name "subtree")
         ("i e" er/mark-LaTeX-inside-environment :name "latex env")
         ("a e" LaTeX-mark-environment :name "latex env")
         ("i m" er/mark-method-call :name "method call")
         ("a m" er/mark-method-call :name "method call")
         ("i r" sk/mark-inside-ruby-block :name "ruby style block")
         ("a r" er/ruby-block-up :name "ruby style block")
         ("i g" er/mark-inside-python-string :name "python string")
         ("a g" er/mark-outside-python-string :name "python string")
         ("i d" sk/mark-inside-python-block :name "python block")
         ("a d" er/mark-outer-python-block :name "python block")
         ("i x" sk/mark-inside-LaTeX-math :name "latex math")
         ("a x" er/mark-LaTeX-math :name "latex math")
         ("i b" er/mark-inside-pairs :name "pairs")
         ("a b" er/mark-outside-pairs :name "pairs")
         ("i i" sk/select-indent-tree :name "indent")
         ("a i" sk/select-indent-tree :name "indent")
         ("a a" mark-whole-buffer :name "all")
         ("i a" mark-whole-buffer :name "all"))))
  (eval `(ryo-modal-keys
          ;; complex operators
          ("r" ,text-objects :then '(sk/eshell-send-region-or-line))
          ("v r" ,text-objects :then '(rectangle-mark-mode))
          ("c y" ,text-objects :then '(sk/cut-region-or-line-to-clipboard) :exit t)
          ("d y" ,text-objects :then '(sk/cut-region-or-line-to-clipboard))
          ("g c" ,text-objects :then '(comment-dwim-2))
          ("r s" ,text-objects :then '(emamux:send-region))
          ("r u" ,text-objects :then '(emamux:run-region))
          ("r w" ,text-objects :then '(sk/shell-send-region-or-line))
          ("r q" ,text-objects :then '(quickrun-region))
          ("r o" ,text-objects :then '(sk/term-send-line-or-region))
		  ("g u" ,text-objects :then '(downcase-region))
          ("g U" ,text-objects :then '(upcase-region))
          ;; basic operators
          ("=" ,text-objects :then '(indent-region))
          ("z" ,text-objects :then '(vimish-fold))
          ("v" ,text-objects)
          ("c" ,text-objects :then '(kill-region) :exit t)
          ("d" ,text-objects :then '(kill-region))
          ("y" ,text-objects :then '(copy-region-as-kill))))
  ;; (eval `(ryo-modal-major-mode-keys
  ;; 		  'matlab-mode
  ;; 		  ("m s" ,text-objects :then '(matlab-shell-run-region))))
  (eval `(ryo-modal-major-mode-keys
		  'emacs-lisp-mode
		  ("m s" ,text-objects :then '(eval-region)))))

;; capital versions of operators
(ryo-modal-keys
 ("C" sp-kill-hybrid-sexp :name "change rest of line" :exit t)
 ("D" sp-kill-hybrid-sexp :name "change rest of line")
 ("Y" sk/copy-to-end-of-line :name "copy rest of line"))

;; repeating style operator mappings
(ryo-modal-keys
 ;; complex operator repeats
 ("c y y" sk/cut-region-or-line-to-clipboard :name "line/region" :exit t)
 ("d y y" sk/cut-region-or-line-to-clipboard :name "line/region")
 ("g c c" comment-dwim-2 :name "line/region")
 ("v r r" rectangle-mark-mode :name "rectangle mark")
 ("g u u" sk/select-inside-line :then '(downcase-region) :name "downcase line")
 ("g U U" sk/select-inside-line :then '(upcase-region) :name "upcase line")
 ;; basic operator repeats
 ("= =" sk/select-inside-line :then '(indent-region) :name "line")
 ("z z" vimish-fold :name "fold region")
 ("v v" er/expand-region :name "expand region")
 ("c c" sk/kill-region-or-line :name "line/region" :exit t)
 ("d d" sk/kill-region-or-line :name "line/region")
 ("y y" sk/copy-region-or-line :name "line/region"))

;; operator based extra repl maps
(ryo-modal-keys
 ("r r" sk/eshell-send-region-or-line :name "region")
 ("r w r" sk/shell-send-region-or-line :name "region")
 ("r q r" quickrun-region :name "region")
 ("r o r" sk/term-send-line-or-region :name "region")
 ("r u r" emamux:run-region :name "region")
 ("r s r" emamux:send-region :name "region"))

;; operator based extra fold maps
(ryo-modal-keys
 ("z d" vimish-fold-delete :name "delete")
 ("z m" vimish-fold-delete-all :name "deleta all")
 ("z n" vimish-fold-next-fold :name "next")
 ("z p" vimish-fold-previous-fold :name "previous")
 ("z o" vimish-fold-unfold :name "open")
 ("z c" vimish-fold-refold :name "close")
 ("z u" vimish-fold-unfold-all :name "unfold all")
 ("z r" vimish-fold-refold-all :name "refold all"))

;; operator based extra maps
(ryo-modal-keys
 ;; c based maps
 ("c r" string-rectangle :name "change rectangle")
 ;; d based maps
 ("d u" sk/ediff-dwim :name "diff update")
 ("d r" kill-rectangle :name "delete rectangle")
 ;; y based maps
 ("y p" sk/paste-from-clipboard :name "paste from clipboard")
 ("y r" copy-rectangle-as-kill :name "copy rectangle"))

;; operator based option maps
(ryo-modal-keys
 ;; v based maps
 ("v o x" set-frame-font :name "select font")
 ("v o m" sk/monaco-font :name "monaco font")
 ("v o c" sk/consolas-font :name "consolas font")
 ("v o h" sk/hack-font :name "hack font")
 ("v o u" sk/courier-font :name "courier font")
 ("v o d" sk/deja-vu-font :name "deja vu font")
 ("v o t" sk/tiny-type :name "tiny type")
 ("v o i" sk/miniscule-type :name "miniscule type")
 ("v o s" sk/small-type :name "small type")
 ("v o e" sk/medium-type :name "medium type")
 ("v o l" sk/large-type :name "large type")
 ;; c based maps
 ("c o n" display-line-numbers-mode :name "line numbers")
 ("c o o" tool-bar-mode :name "tool bar")
 ("c o r" scroll-bar-mode :name "scroll bar")
 ("c o t" display-time-mode :name "time")
 ("c o d" diminish :name "diminish")
 ("c o w" visual-line-mode :name "word wrap")
 ("c o c" company-mode :name "completion")
 ("c o s" smartparens-strict-mode :name "smartparens strict")
 ("c o S" smartparens-mode :name "smartparens")
 ("c o y" yas-minor-mode :name "local snippets")
 ("c o Y" yas-global-mode :name "global snippets")
 ("c o u" undo-tree-mode :name "undo tree")
 ;; y based maps
 ("y o r" yas-reload-all :name "reload all")
 ("y o f" yas-visit-snippet-file :name "snippet file")
 ("y o o" yas-tryout-snippet :name "tryout snippet")
 ("y o n" yas-new-snippet :name "new snippet"))

(if (fboundp 'fancy-battery-mode)
	(ryo-modal-key "c o b" 'fancy-battery-mode :name "battery")
  (ryo-modal-key "c o b" 'display-battery-mode :name "battery"))
 
;; provide ryo bindings
(provide 'sk-ryo-bindings)
