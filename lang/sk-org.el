;; org latex preview all fragments
(defun sk/org-preview-all-latex-fragments ()
  "preview all latex fragments in org"
  (interactive)
  (bookmark-set "org-latex")
  (beginning-of-buffer)
  (org-preview-latex-fragment)
  (bookmark-jump "org-latex"))

;; org table mode
(defun sk/org-table-mode ()
  "switch on orgtbl mode"
  (interactive)
  (require 'org)
  (if (bound-and-true-p orgtbl-mode)
      (orgtbl-mode -1)
	(orgtbl-mode 1)))

;; create checkboxes
(defun sk/org-create-checkbox ()
  (interactive)
  (setq current-prefix-arg '(4))
  (call-interactively 'org-toggle-checkbox))

;; mark inside subtree
(defun sk/mark-inside-subtree ()
  (interactive)
  (org-mark-subtree)
  (next-line 1))
;; org hydra for template expansion
(defun hot-expand (str)
  "Expand org template."
  (insert str)
  (org-try-structure-completion))
(defhydra sk/hydra-org-template (:color blue
										:hint nil)
  "
 ^One liners^                                        ^Blocks^                                      ^Properties^
--------------------------------------------------------------------------------------------------------------------------------------------------------
 _a_: author        _i_: interleave  _D_: description    _C_: center      _p_: python src    _n_: notes    _d_: defaults   _r_: properties        _<_: insert '<'
 _A_: date          _l_: label       _S_: subtitle       _e_: elisp src   _Q_: quote         _L_: latex    _x_: export     _I_: interleave        _q_: quit
 _c_: caption       _N_: name        _k_: keywords       _E_: example     _s_: src                       _X_: noexport   _T_: drill two-sided
 _f_: file tags     _o_: options     _M_: minted         _h_: html        _v_: verbatim
 _H_: latex header  _t_: title       _P_: publish        _m_: matlab src  _V_: verse
 "
  ("a" (hot-expand "<a"))
  ("A" (hot-expand "<A"))
  ("c" (hot-expand "<c"))
  ("f" (hot-expand "<f"))
  ("H" (hot-expand "<H"))
  ("i" (hot-expand "<i"))
  ("I" (hot-expand "<I"))
  ("l" (hot-expand "<l"))
  ("n" (hot-expand "<n"))
  ("N" (hot-expand "<N"))
  ("P" (hot-expand "<P"))
  ("o" (hot-expand "<o"))
  ("t" (hot-expand "<t"))
  ("C" (hot-expand "<C"))
  ("D" (hot-expand "<D"))
  ("e" (hot-expand "<e"))
  ("E" (hot-expand "<E"))
  ("h" (hot-expand "<h"))
  ("k" (hot-expand "<k"))
  ("M" (hot-expand "<M"))
  ("m" (hot-expand "<m"))
  ("p" (hot-expand "<p"))
  ("Q" (hot-expand "<q"))
  ("s" (hot-expand "<s"))
  ("S" (hot-expand "<S"))
  ("v" (hot-expand "<v"))
  ("V" (hot-expand "<V"))
  ("x" (hot-expand "<x"))
  ("X" (hot-expand "<X"))
  ("d" (hot-expand "<d"))
  ("L" (hot-expand "<L"))
  ("r" (hot-expand "<r"))
  ("I" (hot-expand "<I"))
  ("T" (hot-expand "<T"))
  ("b" (hot-expand "<b"))
  ("<" self-insert-command)
  ("q" nil :color blue))
(defun sk/org-template-hook ()
  (define-key org-mode-map "<"
	(lambda () (interactive)
	  (if (looking-back "^")
		  (sk/hydra-org-template/body)
		(self-insert-command 1)))))

(use-package org
  ;; :ensure org-plus-contrib
  :pin org
  :diminish org-indent-mode
  :commands
  (org-mobile-push
   org-mobile-pull
   org-refile
   org-shiftleft
   org-shiftdown
   org-shiftup
   org-shiftright
   org-metaright
   org-metaleft
   org-metadown
   org-metaup
   org-move-item-up
   org-move-item-down
   org-promote-subtree
   org-demote-subtree
   org-move-subtree-up
   org-move-subtree-down
   org-up-element
   org-down-element
   org-next-link
   org-previous-link
   org-next-block
   org-previous-block
   outline-next-visible-heading
   outline-previous-visible-heading
   org-open-at-point
   org-toggle-heading
   org-list-make-subtree
   org-ctrl-c-minus
   org-table-create-or-convert-from-region
   org-copy-subtree
   org-cut-subtree
   org-insert-link
   org-toggle-link-display
   org-add-note
   org-footnote-new
   org-insert-heading
   org-insert-heading-respect-content
   org-date-from-calendar
   org-goto-calendar
   org-clock-in
   org-clock-out
   org-set-tags-command
   org-todo
   org-deadline
   org-schedule
   org-export-dispatch
   org-toggle-latex-fragment
   interleave
   org-toggle-inline-images
   org-babel-tangle
   sk/org-create-checkbox)
  :hook ((org-mode . visual-line-mode)
		 (org-mode . flyspell-mode)
		 (org-mode . sk/org-template-hook))
  :init
  ;; Set to the location of your Org files on your local system
  (setq org-directory "~/Dropbox/org")
  ;; Set to the location of your Mobile Org files on your local system
  (setq org-mobile-inbox-for-pull "~/Dropbox/Apps/MobileOrg/mobileorg.org")
  (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
  ;; hide markup
  (setq org-hide-emphasis-markers t)
  ;; minimal outlines
  (setq org-startup-indented t
		org-hide-leading-stars t)
  ;; image handling
  (setq org-image-actual-width '(300))
  ;; use the default completion for org-goto
  (setq org-goto-interface 'outline-path-completion)
  ;; syntax for codes
  (setq org-src-fontify-natively t
		org-src-tab-acts-natively t)
  ;; exporting
  (setq org-export-with-smart-quotes t
		org-export-backends '(beamer html latex md))
  (setq org-latex-prefer-user-labels t)
  ;; syntax highligting in LaTeX documents
  ;; (require 'ox-latex)
  ;; (add-to-list 'org-latex-packages-alist '("" "minted"))
  ;; (setq org-latex-listings 'minted)
  ;; Let the exporter use the -shell-escape option to let latex
  ;; execute external programs.
  ;; This obviously and can be dangerous to activate!
  ;; (setq org-latex-minted-options
  ;; 		'(("mathescape" "true")
  ;; 		  ("linenos" "true")
  ;; 		  ("numbersep" "5pt")
  ;; 		  ("frame" "lines")
  ;; 		  ("framesep" "2mm")))
  (setq org-latex-pdf-process
		'("pdflatex -interaction nonstopmode -output-directory %o %f"
		  "bibtex %b"
		  "pdflatex -interaction nonstopmode -output-directory %o %f"
		  "pdflatex -interaction nonstopmode -output-directory %o %f"))

  ;; tags
  (setq org-tag-alist (quote (("article"	. ?a)
							  ("code"		. ?c)
							  ("errands"	. ?e)
							  ("books"		. ?b)
							  ("future"		. ?f)
							  ("next"		. ?n)
							  ("immediate"	. ?i)
							  ("reading"	. ?r)
							  ("read"		. ?d)
							  ("job"		. ?j)
							  ("meeting"	. ?m)
							  ("note"		. ?o)
							  ("personal"	. ?l)
							  ("language"	. ?u)
							  ("hobby"		. ?y)
							  ("habits"		. ?t)
							  ("health"		. ?h)
							  ("social"		. ?s)
							  ("gubby"		. ?g)
							  ("project"	. ?p)
							  ("plans"		. ?v)
							  ("work"		. ?w)
							  ("noexport"	. ?x))))
  ;; todo states
  (setq org-todo-keywords
		'((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)")))
  ;; agenda files
  (setq org-agenda-files (list
						  "~/Dropbox/org/personal.org"
						  "~/Dropbox/org/notes.org"
                          "~/Dropbox/org/work.org"
						  ))
  ;; deadline handling
  (setq org-deadline-warning-days 7
		org-agenda-span 'fortnight
		org-agenda-skip-scheduled-if-deadline-is-shown t)

  ;; capture templates - brace yourselves
  (setq org-capture-templates '(

								;; For code snippets
								("c"	; key
								 "Code"	; name
								 entry	; type
								 (file+headline "~/Dropbox/org/notes.org" "Code") ; target
								 "* %^{TITLE} %^G\n:PROPERTIES:\n:Created: %U\n:END:\n%i\#+BEGIN_SRC %^{language}\n%?\n\#END_SRC" ; template
								 :prepend t	 ; properties
								 :empty-lines 1	 ; properties
								 :created t	 ; properties
								 :kill-buffer t) ; properties

								;; For taking notes on random things
								("n"	; key
								 "Note"	; name
								 entry	; type
								 (file+headline "~/Dropbox/org/notes.org" "Notes") ; target
								 "* %? %^G\n:PROPERTIES:\n:Created: %U\n:Linked: %A\n:END:\n%i" ; template
								 :prepend t	 ; properties
								 :empty-lines 1	 ; properties
								 :created t	 ; properties
								 :kill-buffer t) ; properties

								;; For making future plans
								("f"	; key
								 "Plans"	; name
								 entry	; type
								 (file+headline "~/Dropbox/org/personal.org" "Plans") ; target
								 "* %? %^G\n:PROPERTIES:\n:Created: %U\n:END:\n%i" ; template
								 :prepend t	 ; properties
								 :empty-lines 1	 ; properties
								 :created t	 ; properties
								 :kill-buffer t) ; properties

								;; For hobby related notes/tasks
								("h"	; key
								 "Hobbies"	; name
								 entry	; type
								 (file+headline "~/Dropbox/org/personal.org" "Hobbies") ; target
								 "* %? %^G\n:PROPERTIES:\n:Created: %U\n:END:\n%i" ; template
								 :prepend t	 ; properties
								 :empty-lines 1	 ; properties
								 :created t	 ; properties
								 :kill-buffer t) ; properties

								;; For taking notes on personal things
								("p"	; key
								 "Personal Note"	; name
								 entry	; type
								 (file+headline "~/Dropbox/org/personal.org" "Notes") ; target
								 "* %? %^G\n:PROPERTIES:\n:Created: %U\n:Linked: %A\n:END:\n%i" ; template
								 :prepend t	 ; properties
								 :empty-lines 1	 ; properties
								 :created t	 ; properties
								 :kill-buffer t) ; properties

								;; Books
								("b"	; key
								 "Books" ; name
								 entry	; type
								 (file+headline "~/Dropbox/org/personal.org" "Books") ; target
								 "* %? %^G\n:PROPERTIES:\n:Created: %U\n::END:\n%i" ; template
								 :prepend t	 ; properties
								 :empty-lines 1	 ; properties
								 :created t	 ; properties
								 :kill-buffer t) ; properties

								;; For potating
								("g"	 ; key
								 "Gubby" ; name
								 entry	 ; type
								 (file+headline "~/Dropbox/org/personal.org" "Gubby") ; target
								 "* %? %^G\n:PROPERTIES:\n:Created: %U\n:Linked: %A\n:END:\n%i" ; template
								 :prepend t	 ; properties
								 :empty-lines 1	 ; properties
								 :created t	 ; properties
								 :kill-buffer t) ; properties

								;; For notes or something regarding more work
								("w"	; key
								 "Work"	; name
								 entry	; type
								 (file+headline "~/Dropbox/org/work.org" "Tasks")	; target
								 "* TODO %^{Title} %^G\n:PROPERTIES:\n:Created: %U\n:Linked: %A\n:END:\n%i\n%?" ; template
								 :prepend t	 ; properties
								 :empty-lines 1	 ; properties
								 :created t	 ; properties
								 :kill-buffer t) ; properties

								;; For capturing minutes of the meeting
								("m"	   ; key
								 "Meeting" ; name
								 entry	   ; type
								 (file+olp+datetree "~/Dropbox/org/work.org" "Meeting") ; target
								 "* %^{Title} %^G\n:PROPERTIES:\n:Created: %U\n:END:\n%i\n** Agenda:\n%?\n\n** Minutes of the meeting:\n" ; template
								 :prepend t	 ; properties
								 :empty-lines 1	 ; properties
								 :created t	 ; properties
								 :kill-buffer t) ; properties

								;; For capturing details of a job application/details
								("j"	    ; key
								 "Jobs"	    ; name
								 table-line ; type
								 (file+headline "~/Dropbox/org/notes.org" "Jobs") ; target
								 "| %^{Company} | [[%^{job link}][%^{position}]] | %^{referrals?} | %^{Experience?} | %^{Status} | " ; template
								 :prepend t ; properties
								 ;; :table-line-pos "II-3"   ; properties
								 :empty-lines 1	 ; properties
								 :created t	 ; properties
								 :kill-buffer t) ; properties

								;; To capture random todos
								("t"	    ; key
								 "Todos" ; name
								 entry	    ; type
								 (file+headline "~/Dropbox/org/personal.org" "Todos") ; target
								 "* TODO %^{Title} %^G\n:PROPERTIES:\n:Created: %U\n:END:\n%i\n%?" ; template
								 :prepend t	 ; properties
								 :empty-lines 1	 ; properties
								 :created t	 ; properties
								 :kill-buffer t) ; properties

								;; For capturing minutes of the meeting
								("d"	   ; key
								 "Diary" ; name
								 entry	   ; type
								 (file+olp+datetree "~/Dropbox/org/personal.org" "Diary") ; target
								 "* %^{Title} %^G\n:PROPERTIES:\n:Created: %U\n:END:\n%i\n %?" ; template
								 :prepend t	 ; properties
								 :empty-lines 1	 ; properties
								 :created t	 ; properties
								 :kill-buffer t) ; properties

								))

  ;; refiling
  (setq org-refile-targets '((nil :maxlevel . 9)
							 (org-agenda-files :maxlevel . 9)))
  (setq org-refile-use-outline-path t
		org-outline-path-complete-in-steps nil)

  :bind* (("C-c a" . org-agenda)
		  ("C-c c" . org-capture))

  :config

  ;; More of those nice template expansion
  (add-to-list 'org-structure-template-alist '("A" "#+DATE: ?"))
  (add-to-list 'org-structure-template-alist '("C" "#+BEGIN_CENTER\n?\n#+END_CENTER\n"))
  (add-to-list 'org-structure-template-alist '("D" "#+DESCRIPTION: ?"))
  (add-to-list 'org-structure-template-alist '("E" "#+BEGIN_EXAMPLE\n?\n#+END_EXAMPLE\n"))
  (add-to-list 'org-structure-template-alist '("H" "#+LATEX_HEADER: ?"))
  (add-to-list 'org-structure-template-alist '("I" ":INTERLEAVE_PDF: ?"))
  (add-to-list 'org-structure-template-alist '("L" "#+BEGIN_LATEX\n?\n#+END_LATEX"))
  (add-to-list 'org-structure-template-alist '("M" "#+LATEX_HEADER: \\usepackage{minted}\n"))
  (add-to-list 'org-structure-template-alist '("N" "#+NAME: ?"))
  (add-to-list 'org-structure-template-alist '("P" "#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"org.css\"/>\n"))
  (add-to-list 'org-structure-template-alist '("S" "#+SUBTITLE: ?"))
  (add-to-list 'org-structure-template-alist '("T" ":DRILL_CARD_TYPE: twosided"))
  (add-to-list 'org-structure-template-alist '("V" "#+BEGIN_VERSE\n?\n#+END_VERSE"))
  (add-to-list 'org-structure-template-alist '("X" "#+EXCLUDE_TAGS: ?"))
  (add-to-list 'org-structure-template-alist '("a" "#+AUTHOR: ?"))
  (add-to-list 'org-structure-template-alist '("c" "#+CAPTION: ?"))
  (add-to-list 'org-structure-template-alist '("d" "#+OPTIONS: ':nil *:t -:t ::t <:t H:3 \\n:nil ^:t arch:headline\n#+OPTIONS: author:t email:nil e:t f:t inline:t creator:nil d:nil date:t\n#+OPTIONS: toc:nil num:nil tags:nil todo:nil p:nil pri:nil stat:nil c:nil d:nil\n#+LATEX_HEADER: \\usepackage[margin=2cm]{geometry}\n#+LANGUAGE: en\n\n?"))
  (add-to-list 'org-structure-template-alist '("e" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))
  (add-to-list 'org-structure-template-alist '("f" "#+TAGS: @?"))
  (add-to-list 'org-structure-template-alist '("h" "#+BEGIN_HTML\n?\n#+END_HTML\n"))
  (add-to-list 'org-structure-template-alist '("i" "#+INTERLEAVE_PDF: ?"))
  (add-to-list 'org-structure-template-alist '("k" "#+KEYWORDS: ?"))
  (add-to-list 'org-structure-template-alist '("l" "#+LABEL: ?"))
  (add-to-list 'org-structure-template-alist '("m" "#+BEGIN_SRC matlab\n?\n#+END_SRC"))
  (add-to-list 'org-structure-template-alist '("n" "#+BEGIN_NOTES\n?\n#+END_NOTES"))
  (add-to-list 'org-structure-template-alist '("o" "#+OPTIONS: ?"))
  (add-to-list 'org-structure-template-alist '("p" "#+BEGIN_SRC python\n?\n#+END_SRC"))
  (add-to-list 'org-structure-template-alist '("q" "#+BEGIN_QUOTE\n?\n#+END_QUOTE"))
  (add-to-list 'org-structure-template-alist '("r" ":PROPERTIES:\n?\n:END:"))
  (add-to-list 'org-structure-template-alist '("s" "#+BEGIN_SRC ?\n#+END_SRC\n"))
  (add-to-list 'org-structure-template-alist '("t" "#+TITLE: ?"))
  (add-to-list 'org-structure-template-alist '("v" "#+BEGIN_VERBATIM\n?\n#+END_VERBATIM")))

;; babel
(use-package babel
  :ensure t
  :after (org)
  :init
  (setq org-confirm-babel-evaluate nil)
  :config
  ;; Babel load
  (org-babel-do-load-languages 'org-babel-load-languages
							   '((emacs-lisp . t)
								 ;; (dot . t)
								 ;; (ditaa . t)
								 (latex . t)
								 (gnuplot . t)
								 ;; (sh . t)
								 (js . t)
								 (C . t)
								 (ledger . t)
								 (R . t)
								 (octave . t)
								 (matlab . t)
								 (python . t))))

;; async execution
(use-package ob-async
  :ensure t
  :after (org))

;; ipython
(use-package ob-ipython
  :ensure t
  :after (org))

(use-package ox-twbs
  :ensure t
  :after (org))

(use-package ox-gfm
  :ensure t
  :after (org))

(use-package ox-rst
  :ensure t
  :after (org))

;; interleaved notes
(use-package interleave
  :ensure t
  :commands (interleave
			 interleave-pdf-mode)
  :after (org)
  :init
  (setq interleave-org-notes-dir-list '("~/Dropbox/Paper/notes"))
  :config
  (add-hook 'doc-view-mode-hook 'doc-view-fit-page-to-window))

;; mappings
(ryo-modal-major-mode-keys
 'org-mode
 ("m e b" org-shiftleft :name "shift left")
 ("m e u" org-shiftup :name "shift up")
 ("m e d" org-shiftdown :name "shift down")
 ("m e f" org-shiftright :name "shift right")
 ("m e h" org-metaleft :name "meta left")
 ("m e k" org-metaup :name "meta up")
 ("m e j" org-metadown :name "meta down")
 ("m e l" org-metaright :name "meta right")
 
 ("m s u" org-move-item-up :name "move item up")
 ("m s d" org-move-item-down :name "move item down")
 ("m s h" org-promote-subtree :name "promote")
 ("m s l" org-demote-subtree :name "demote")
 ("m s j" org-move-subtree-down :name "move down")
 ("m s k" org-move-subtree-up :name "move up")
 
 ;; motion bindings
 ("m h" org-up-element :name "up element")
 ("m j" outline-next-visible-heading :name "next heading")
 ("m k" outline-previous-visible-heading :name "previous heading")
 ("m l" org-down-element :name "down element")
 ("m f" org-next-link :name "next link")
 ("m b" org-previous-link :name "previous link")
 ("m n" org-next-block :name "next block")
 ("m p" org-previous-block :name "previous block")
 ("m /" ace-link-org :name "ace link org")
 ("m RET" org-open-at-point :name "open at point")

 ;; editing bindings
 ("m ," org-toggle-heading :name "toggle heading")
 ("m *" org-list-make-subtree :name "list to subtree")
 ("m -" org-ctrl-c-minus :name "C-c -")
 ("m |" org-table-create-or-convert-from-region :name "create/convert table")
 ("m y" org-copy-subtree :name "copy subtree")
 ("m d" org-cut-subtree :name "delete subtree")
 ("m i" org-insert-link :name "insert link")
 ("m r" org-refile :name "refile")
 ("m o" org-add-note :name "add note")
 ("m z" org-footnote-new :name "add footnote")
 ("m w" org-insert-heading :name "insert heading" :exit t)
 ("m v" org-insert-heading-respect-content :name "insert heading dwim" :exit t)
 ("m c c" org-date-from-calendar :name "date from calendar")
 ("m c v" org-goto-calendar :name "goto calendar")
 ("m c i" org-clock-in :name "clock in")
 ("m c o" org-clock-out :name "clock out")

 ;; toggle things
 ("m t l" org-toggle-link-display :name "links")
 ("m t t" org-toggle-latex-fragment :name "tex")
 ("m t i" org-toggle-inline-images :name "images")
 ("m t o" org-toggle-inline-images :name "ordered property")

 ;; misc
 ("m a a" org-set-tags-command :name "tags")
 ("m a t" org-todo :name "todo")
 ("m a d" org-deadline :name "deadline")
 ("m a s" org-schedule :name "schedule")
 ;; ("m x" org-reftex-citation :name "reftex")
 ("m m" org-export-dispatch :name "export")
 ("m u" interleave :name "interleave")
 ("m g" org-babel-tangle :name "tangle")

 ;; custom
 ("m q" sk/org-create-checkbox :name "create checkbox"))

(which-key-add-major-mode-key-based-replacements 'org-mode
  "m e" "edit heading"
  "m s" "subtree/item"
  "m t" "toggle"
  "m c" "cal/clock"
  "m a" "add")

;; provide org configuration
(provide 'sk-org)
