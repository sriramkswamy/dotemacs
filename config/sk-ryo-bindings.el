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
 ("SPC u" "C-u" :name "C-u prefix")
 ("m =" "C-c '" :name "C-c edit special"))

;; ryo modal general mappings
(ryo-modal-keys
 ("z z" recenter-top-bottom :name "recenter")
 ("%" sk/goto-matching-paren :name "matching paren")
 ("+" sk/counsel-bookmarks :name "add/see bookmarks" :norepeat t)
 ("SPC h" "C-h" :name "help" :norepeat t)
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
 ("DEL" mode-line-other-buffer :name "last buffer")
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
 ("s" "C-c g" :name "goto char(s)"))

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
 ("#" sk/swiper-all-at-point :name "search word ina ll files")
 ("E" ivy-occur :name "edit search results")
 ("g *" counsel-colors-emacs :name "search emacs colors")
 ("g #" counsel-colors-web :name "search web colors")
 ("g x" wgrep-save-all-buffers :name "save edited grep results")
 ("g X" wgrep-abort-changes :name "abort edited grep results")
 ("g E" ivy-wgrep-change-to-wgrep-mode :name "edit grep results")
 ("g G" counsel-git-log :name "git log")
 ("SPC ," counsel-descbinds :name "bindings" :norepeat t)
 ("SPC ." counsel-load-theme :name "themes" :norepeat t)
 ("J" dumb-jump-go :name "dumb jump")
 ("K" dash-at-point-with-docset :name "dash doc")
 ("/" "C-s" :name "search in buffer"))

;; ryo scroll maps
(ryo-modal-keys
 ("g j" diff-hl-next-hunk :name "next diff")
 ("g k" diff-hl-previous-hunk :name "previous diff"))

;; ryo modal repl maps
(ryo-modal-keys
 (:norepeat t)
 ("SPC v u" sk/call-terminal :name "os terminal")
 ("SPC v s" sk/shell :name "shell")
 ("SPC v t" sk/term :name "term")
 ("SPC v e" sk/eshell :name "split eshell")
 ("SPC v v" eshell :name "eshell"))

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
 ("w N" widen :name "widen window")
 ("w u" winner-undo :name "undo window config")
 ("w U" winner-redo :name "redo window config")
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
               '(("SPC" scratch :name "scratch" :norepeat t)
				 ("w" "C-x C-s" :name "save buffer" :norepeat t)
                 ("e" magit-status :name "git status" :norepeat t)
                 ("f" "C-x C-f" :name "open file" :norepeat t)
                 ("g" "C-g" :name "interrupt" :norepeat t)
                 ("r" counsel-recentf :name "recent files" :norepeat t)
                 ;; ("d" sk/counsel-file-jump-project-git :name "project files" :norepeat t)
                 ("d" sk/counsel-file-jump-project-fzf :name "project files" :norepeat t)
                 ("p" sk/counsel-ag-project :name "grep project" :norepeat t)
                 ("a" ivy-switch-view :name "switch views" :norepeat t)
                 ("l" flycheck-list-errors :name "list errors" :norepeat t)
                 ("y" "M-y" :name "copy history" :norepeat t)
                 ("k" "C-x b" :name "switch buffer" :norepeat t)
				 ("x" ibuffer :name "interactive buffer" :norepeat t)
				 ("z" prodigy :name "background services" :norepeat t)
				 ("q" quickrun :name "quickrun" :norepeat t)
				 ("b" sk/debug-hydra/body :name "begin debugging" :norepeat t)
				 ("i" sk/lsp-hydra/body :name "lsp" :norepeat t)
				 ("o" sk/dap-hydra/body :name "dap" :norepeat t)
				 ("j" "M-x" :name "commands" :norepeat t)))

(if (version< emacs-version "25.2")
	(ryo-modal-key "SPC n" 'neotree :name "file tree" :norepeat t)
  (ryo-modal-key "SPC n" 'treemacs :name "file tree" :norepeat t))

;; mapping with minor leader
(ryo-modal-key "SPC m"
			   '(("p" sk/org-rclone-sync-to-dropbox :name "push org to rclone" :norepeat t)
				 ("f" sk/org-rclone-sync-from-dropbox :name "fetch org from rclone" :norepeat t)
				 (":" eval-expression :name "eval expression" :norepeat t)))

;; use locate or spotlight depending on the system
(cond
 ((eq system-type 'darwin)
  (ryo-modal-key "SPC s" 'spotlight :name "desktop search"))
 ((eq system-type 'gnu/linux)
  (ryo-modal-key "SPC s" 'counsel-locate :name "desktop search")))

;; mapping with global prefix
(ryo-modal-key "g"
               '(("A" prog-fill :name "program fill" :norepeat t)
                 ("D" dash-at-point-with-docset :name "dash doc" :norepeat t)
                 ("S" electric-newline-and-maybe-indent :name "split line")
                 ("J" join-line :name "join line")
				 ("b" magit-blame :name "git blame" :norepeat t)
				 ("h" diff-hl-diff-goto-hunk :name "goto hunk" :norepeat t)
				 ("H" diff-hl-revert-hunk :name "hunk revert" :norepeat t)
				 ("T" git-timemachine-toggle :name "git time machine" :norepeat t)
				 ("P" sk/post-gist :name "gist post" :norepeat t)
				 ("I" browse-at-remote :name "internet remote" :norepeat t)
				 ("f" ffap :name "find file at point" :norepeat t)
				 ("W" wdired-change-to-wdired-mode :name "writeable dir" :norepeat t)
				 ("i" counsel-package :name "install package" :norepeat t)
				 ("`" async-shell-command :name "run shell command" :norepeat t)
				 ("a" describe-char :name "describe char" :norepeat t)
				 ("p" list-packages :name "list packages" :norepeat t)
				 ("V" sk/browse-current-file :name "view file in browser" :norepeat t)
				 ("R" sk/rename-current-buffer-file :name "rename file" :norepeat t)
				 ("K" sk/delete-current-buffer-file :name "remove file" :norepeat t)
				 ("Y" sk/copy-current-file-path :name "copy file path")
                 ("." sk/duplicate-current-line-or-region :name "duplicate line/region")
                 ("C" string-inflection-all-cycle :name "change case")
                 (";" goto-last-change :name "last change")
                 ("," goto-last-change-reverse :name "last change reverse")
                 ("n" back-button-global-backward :name "prev global mark")
                 ("N" back-button-global-forward :name "next global mark")
                 ("-" back-button-local-backward :name "next local mark")
                 ("+" back-button-local-forward :name "next local mark")
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

;; text objects
(defvar text-objects
       '(;; single-char style text object
         ("0" sk/mark-beginning-line :name "beginning of line")
         ("$" sk/mark-end-line :name "end of line")
         ("h" sk/mark-previous-char :name "previous char(s)")
         ("j" sk/mark-next-line :name "next line(s)")
         ("k" sk/mark-previous-line :name "previous line(s)")
         ("l" sk/mark-next-char :name "next char(s)")
         ("b" sk/mark-backward-word :name "to start of word")
         ("e" sk/mark-forward-word :name "to end of word")
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
         ("s" "C-c G" :name "till char(s)")
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
         ("i a" mark-whole-buffer :name "all")))

;; common operators
(eval `(ryo-modal-keys
          ("=" ,text-objects :then '(indent-region))
          ("z" ,text-objects :then '(vimish-fold))
          ("v" ,text-objects)
          ("c" ,text-objects :then '(kill-region) :exit t)
          ("d" ,text-objects :then '(kill-region))
          ("y" ,text-objects :then '(copy-region-as-kill))))

;; alignment
(eval `(ryo-modal-keys
          ("g l SPC" ,text-objects :then '(sk/align-whitespace))
          ("g l d" ,text-objects :then '(sk/align-semicolon))
          ("g l &" ,text-objects :then '(sk/align-ampersand))
          ("g l q" ,text-objects :then '(sk/align-quote-space))
          ("g l =" ,text-objects :then '(sk/align-equals))
          ("g l c" ,text-objects :then '(sk/align-comma))
          ("g l ." ,text-objects :then '(sk/align-dot))
          ("g l :" ,text-objects :then '(sk/align-colon))
          ("g l %" ,text-objects :then '(sk/align-percent))
          ("g l #" ,text-objects :then '(sk/align-hash))
          ("g l" ,text-objects :then '(align-regexp))))

;; complex operators
(eval `(ryo-modal-keys
        ("v r" ,text-objects :then '(rectangle-mark-mode))
        ("c y" ,text-objects :then '(sk/cut-region-or-line-to-clipboard) :exit t)
        ("d y" ,text-objects :then '(sk/cut-region-or-line-to-clipboard))
        ("g y" ,text-objects :then '(sk/copy-line-or-region-to-clipboard))
        ("g c" ,text-objects :then '(comment-dwim-2))
        ("w e" ,text-objects :then '(sk/eshell-send-region-or-line))
        ("r s" ,text-objects :then '(emamux:send-region))
        ("r z" ,text-objects :then '(emamux:run-region))
        ("w r" ,text-objects :then '(sk/shell-send-region-or-line))
        ("r q" ,text-objects :then '(quickrun-region))
        ("r Q" ,text-objects :then '(quickrun-replace-region))
        ("w t" ,text-objects :then '(sk/term-send-line-or-region))
		("g u" ,text-objects :then '(downcase-region))
        ("g U" ,text-objects :then '(upcase-region))
        ("g w" ,text-objects :then '(fill-region-as-paragraph))
        ("g r" ,text-objects :then '(query-replace))
        ("g v" ,text-objects :then '(query-replace-regexp))
        ("g e" ,text-objects :then '(sk/counsel-ag-project-selection))
        ("g s" ,text-objects :then '(sk/counsel-ag-directory-selection))
        ("w n" ,text-objects :then '(narrow-to-region))
        ("w SPC" ,text-objects :then '(ws-butler-clean-region))))

;; elisp mode specific operators
(eval `(ryo-modal-major-mode-keys
		  'emacs-lisp-mode
		  ("m s" ,text-objects :then '(eval-region))))

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
 ("g y y" sk/copy-line-or-region-to-clipboard :name "line/region")
 ("g u u" downcase-region :name "downcase")
 ("g U U" upcase-region :name "upcase")
 ("g w w" fill-paragraph :name "fill para")
 ("g r r" query-replace :name "replace")
 ("g v v" query-replace-regexp :name "replace regexp")
 ("g e e" sk/counsel-ag-project-selection :name "grep symbol")
 ("g s s" sk/counsel-ag-directory :name "search in directory")
 ("w n n" narrow-to-region :name "narrow")
 ("w SPC SPC" ws-butler-maybe-trim-eob-lines :name "trim whitespace")
 ;; alignment
 ("g l SPC SPC" sk/align-whitespace :name "align whitespace")
 ("g l d d" sk/align-semicolon :name "align semicolon")
 ("g l & &" sk/align-ampersand :name "align ampersand")
 ("g l q q" sk/align-quote-space :name "align quote")
 ("g l = =" sk/align-equals :name "align equal")
 ("g l c c" sk/align-comma :name "align comma")
 ("g l . ." sk/align-dot :name "align dot")
 ("g l : :" sk/align-colon :name "align colon")
 ("g l % %" sk/align-percent :name "align percent")
 ("g l # #" sk/align-hash :name "align hash")
 ("g l l" align-regexp :name "align regexp")
 ;; basic operator repeats
 ("= =" sk/select-inside-line :then '(indent-region) :name "line")
 ("z r" vimish-fold :name "fold region")
 ("z SPC" vimish-fold-toggle :name "toggle folds")
 ("v v" er/expand-region :name "expand region")
 ("c c" sk/kill-region-or-line :name "line/region" :exit t)
 ("d d" sk/kill-region-or-line :name "line/region")
 ("y y" sk/copy-region-or-line :name "line/region"))

;; operator based extra repl maps
(ryo-modal-keys
 ("w e e" sk/eshell-send-region-or-line :name "region")
 ("w r r" sk/shell-send-region-or-line :name "region")
 ("r q q" quickrun-region :name "region")
 ("r Q Q" quickrun-replace-region :name "region")
 ("w t t" sk/term-send-line-or-region :name "region")
 ("r z z" emamux:run-region :name "region")
 ("r s s" emamux:send-region :name "region"))

;; operator based extra fold maps
(ryo-modal-keys
 ("z d" vimish-fold-delete :name "delete")
 ("z m" vimish-fold-delete-all :name "deleta all")
 ("z n" vimish-fold-next-fold :name "next")
 ("z N" vimish-fold-previous-fold :name "previous")
 ("z o" vimish-fold-unfold :name "open")
 ("z c" vimish-fold-refold :name "close")
 ("z u" vimish-fold-unfold-all :name "unfold all")
 ("z p" vimish-fold-refold-all :name "refold all"))

;; operator based extra maps
(ryo-modal-keys
 ;; c based maps
 ("c r" string-rectangle :name "change rectangle")
 ;; d based maps
 ("d u" sk/ediff-dwim :name "diff update")
 ("d r" kill-rectangle :name "delete rectangle")
 ;; y based maps
 ("y s" embrace-commander :name "surround")
 ("y p" sk/paste-from-clipboard :name "paste from clipboard")
 ("y r" copy-rectangle-as-kill :name "copy rectangle"))

;; ;; doc-view bindings
;; (ryo-modal-keys
;;   ;; ("c p" doc-view-fit-page-to-window :name "doc fit")
;;  ("d p" sk/other-doc-fit :name "doc fit other window"))
;; ;; doc-view ryo scroll hydra
;; (ryo-modal-key
;;  "S" :hydra
;;  '(sk/hydra-scroll (:hint nil)
;; 				"
;; ^Window^                      ^Doc^                           ^Notes^         ^Quit^
;; ^^^^^^^^^^^^^-------------------------------------------------------------------------------
;;  _k_: up     _p_: other up      _b_: prev page  _a_: other revert _i_: note       _s_: smart
;;  _j_: down   _n_: other down    _f_: next page  _o_: other fit    _u_: notes up   _v_: ivy
;;  _h_: left   _l_: right                                       _d_: notes down _q_: quit
;; "
;; 				("p" sk/other-window-up)
;; 				("n" scroll-other-window)
;; 				("f" sk/other-doc-down)
;; 				("b" sk/other-doc-up)
;; 				("a" sk/doc-revert-other-window :color blue)
;; 				("o" sk/other-doc-fit :color blue)
;; 				("u" sk/interleave-other-window-previous)
;; 				("d" sk/interleave-other-window-next)
;; 				("i" sk/interleave-other-window-note :color blue)
;;  				("h" scroll-right)
;; 				("j" scroll-up-command)
;; 				("k" scroll-down-command)
;; 				("l" scroll-left)
;; 				("s" sk/smartparens-hydra/body :color blue)
;; 				("v" hydra-ivy/body :color blue)
;; 				("q" nil :color blue)))

;; pdf-view bindings
(ryo-modal-keys
 ("c p" pdf-view-fit-page-to-window :name "pdf fit")
 ("d p" sk/other-pdf-fit :name "pdf fit other window"))
;; doc-view ryo scroll hydra
(ryo-modal-key
 "S" :hydra
 '(sk/hydra-scroll (:hint nil)
				"
^Window^                      ^Doc^                           ^Notes^         ^Quit^
^^^^^^^^^^^^^-------------------------------------------------------------------------------
 _k_: up     _p_: other up      _b_: prev page  _a_: other revert _i_: note       _s_: smart
 _j_: down   _n_: other down    _f_: next page  _o_: other fit    _u_: notes up   _v_: ivy
 _h_: left   _l_: right                                       _d_: notes down _q_: quit
"
				("p" sk/other-window-up)
				("n" scroll-other-window)
				("f" sk/other-pdf-down)
				("b" sk/other-pdf-up)
				("a" sk/pdf-revert-other-window :color blue)
				("o" sk/other-pdf-fit :color blue)
				("u" sk/interleave-other-window-previous)
				("d" sk/interleave-other-window-next)
				("i" sk/interleave-other-window-note :color blue)
 				("h" scroll-right)
				("j" scroll-up-command)
				("k" scroll-down-command)
				("l" scroll-left)
				("s" sk/smartparens-hydra/body :color blue)
				("v" hydra-ivy/body :color blue)
				("q" nil :color blue)))

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
 ("c o t" display-time-mode :name "time")
 ("c o d" diminish :name "diminish")
 ("c o w" visual-line-mode :name "word wrap")
 ;; ("c o c" ycmd-mode :name "completion add source")
 ("c o a" super-save-mode :name "auto save")
 ("c o r" scroll-bar-mode :name "scroll bar mode")
 ("c o s" smartparens-strict-mode :name "smartparens strict")
 ("c o S" smartparens-mode :name "smartparens")
 ("c o y" yas-minor-mode :name "local snippets")
 ("c o Y" yas-global-mode :name "global snippets")
 ("c o u" undo-tree-mode :name "undo tree")
 ;; d based maps
 ("d o o" desktop+-load-auto :name "open session")
 ("d o n" desktop+-create :name "new session")
 ("d o j" desktop+-load :name "jump sessions")
 ("d o s" desktop+-create-auto :name "save session")
 ("d o m" desktop-save-mode :name "auto save session")
 ("d o d" desktop-remove :name "delete session")
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
