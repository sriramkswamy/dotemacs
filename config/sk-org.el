;; setup org
(use-package org
  ;; :ensure org-plus-contrib
  :pin org
  :diminish org-indent-mode
  :commands (org-mobile-push
			 org-mobile-pull)

  :init
  ;; Set to the location of your Org files on your local system
  (setq org-directory "~/Dropbox/org")
  ;; Set to the name of the file where new notes will be stored
  (setq org-mobile-inbox-for-pull "~/Dropbox/org/notes.org")
  ;; Set to <your Dropbox root directory>/MobileOrg.
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
  (setq org-tag-alist (quote (("article"   . ?A)
							  ("courses"   . ?O)
							  ("code"      . ?C)
							  ("errands"   . ?E)
							  ("blog"      . ?B)
							  ("idea"      . ?D)
							  ("job"       . ?J)
							  ("meeting"   . ?M)
							  ("note"      . ?N)
							  ("personal"  . ?I)
							  ("learning"  . ?L)
							  ("language"  . ?U)
							  ("hobbies"   . ?H)
							  ("gubby"     . ?G)
							  ("project"   . ?P)
							  ("reveal"    . ?R)
							  ("vague"     . ?V)
							  ("work"      . ?W)
							  ("noexport"  . ?X))))
  ;; todo states
  (setq org-todo-keywords
		'((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)")
		  ;; trello based highligting
		  (sequence "TALKY(k)" "TRAVEL(v)" "|" "LINKS(l)" "PROJECTS(p)" "WEDDING(m)")
		  (sequence "|" "POTATO")
		  (sequence "HOME(h)" "|" "HOBBIES(o)" "FUN(f)" "CAT(x)")
		  ))
  ;; agenda files
  (setq org-agenda-files (list
						  "~/Dropbox/org/personal.org"
						  "~/Dropbox/org/phd.org"
						  "~/Dropbox/org/potato.org"
						  "~/Dropbox/org/life.org"
						  ))
  ;; deadline handling
  (setq org-deadline-warning-days 7
		org-agenda-span 'fortnight
		org-agenda-skip-scheduled-if-deadline-is-shown t)

  ;; capture templates - brace yourselves
  (setq org-capture-templates '(
								;; For taking notes on random things
								("n"	; key
								 "Note"	; name
								 entry	; type
								 (file "~/Dropbox/org/personal.org") ; target
								 "* NOTES %?\n%i" ; template
								 :prepend t	 ; properties
								 :empty-lines 1	 ; properties
								 :created t	 ; properties
								 :kill-buffer t) ; properties

								;; For potating
								("r"	 ; key
								 "Potato" ; name
								 entry	 ; type
								 (file "~/Dropbox/org/personal.org") ; target
								 "* POTATO %?\n%i" ; template
								 :prepend t	 ; properties
								 :empty-lines 1	 ; properties
								 :created t	 ; properties
								 :kill-buffer t) ; properties

								;; To capture of errands
								("e"	   ; key
								 "Errands" ; name
								 entry	   ; type
								 (file "~/Dropbox/org/personal.org") ; target
								 "* TODO %^{Title}\n%i\n%?" ; template
								 :prepend t	 ; properties
								 :empty-lines 1	 ; properties
								 :created t	 ; properties
								 :kill-buffer t) ; properties

								;; For travel
								("t"	; key
								 "Travel"	; name
								 entry	; type
								 (file "~/Dropbox/org/potato.org")	; target
								 "* TRAVEL %^{Title}\n%i\n%?" ; template
								 :prepend t	 ; properties
								 :empty-lines 1	 ; properties
								 :created t	 ; properties
								 :kill-buffer t) ; properties

								;; for sharing links
								("l"	; key
								 "Links"	; name
								 entry	; type
								 (file "~/Dropbox/org/potato.org")	; target
								 "* LINKS %^{Description}\n%i\n%?" ; template
								 :prepend t	 ; properties
								 :empty-lines 1	 ; properties
								 :created t	 ; properties
								 :kill-buffer t) ; properties

								;; for discussing
								("d"	; key
								 "Discuss"	; name
								 entry	; type
								 (file "~/Dropbox/org/potato.org")	; target
								 "* TALKY %^{Description}\n%i\n%?" ; template
								 :prepend t	 ; properties
								 :empty-lines 1	 ; properties
								 :created t	 ; properties
								 :kill-buffer t) ; properties

								;; Projects
								("p"	; key
								 "Projects"	; name
								 entry	; type
								 (file "~/Dropbox/org/potato.org")	; target
								 "* PROJECTS %?\n%i" ; template
								 :prepend t	 ; properties
								 :empty-lines 1	 ; properties
								 :created t	 ; properties
								 :kill-buffer t) ; properties

								;; common hobbies
								("h"	; key
								 "Hobbies"	; name
								 entry	; type
								 (file "~/Dropbox/org/life.org")	; target
								 "* HOBBIES %?\n%i" ; template
								 :prepend t	 ; properties
								 :empty-lines 1	 ; properties
								 :created t	 ; properties
								 :kill-buffer t) ; properties

								;; home maintenence
								("o"	; key
								 "Home"	; name
								 entry	; type
								 (file "~/Dropbox/org/life.org")	; target
								 "* HOME %?\n%i" ; template
								 :prepend t	 ; properties
								 :empty-lines 1	 ; properties
								 :created t	 ; properties
								 :kill-buffer t) ; properties

								;; fun stuff to do
								("f"	; key
								 "Fun"	; name
								 entry	; type
								 (file "~/Dropbox/org/life.org")	; target
								 "* FUN %?\n%i" ; template
								 :prepend t	 ; properties
								 :empty-lines 1	 ; properties
								 :created t	 ; properties
								 :kill-buffer t) ; properties

								;; For notes or something regarding more work
								("w"	; key
								 "Work"	; name
								 entry	; type
								 (file "~/Dropbox/org/phd.org")	; target
								 "* TODO %^{Title}\n%i\n%?" ; template
								 :prepend t	 ; properties
								 :empty-lines 1	 ; properties
								 :created t	 ; properties
								 :kill-buffer t) ; properties
								)) ; end capture

  ;; refiling
  (setq org-refile-targets '((nil :maxlevel . 9)
							 (org-agenda-files :maxlevel . 9)))
  (setq org-refile-use-outline-path t
		org-outline-path-complete-in-steps nil)

  :bind* (("C-c a" . org-agenda)
		  ("C-c c" . org-capture))

  :config
  ;; babel
  (use-package babel
	:ensure t
	:init
	(setq org-confirm-babel-evaluate nil)
	:defer t)
  ;; async execution
  (use-package ob-async
	:ensure t
	:defer t)
  ;; ipython
  (use-package ob-ipython
	:ensure t
	:defer t)
  ;; extra exports
  (use-package ox-reveal
  	:ensure t
  	:defer t
  	:init
  	(setq org-reveal-title-slide-template "<h1>%t</h1>\n<h3>%a</h3>")
  	(setq org-reveal-root "file:///Users/sriramkswamy/Documents/github/reveal.js")
  	(use-package htmlize
  	  :ensure t))
  (use-package ox-twbs
	:ensure t
	:defer t)
  (use-package ox-gfm
	:ensure t
	:defer t)
  (use-package ox-rst
  	:ensure t
  	:defer t)

  ;; mark inside subtree
  (defun sk/mark-inside-subtree ()
	(interactive)
	(org-mark-subtree)
	(next-line 1))

  ;; create checkboxes
  (defun sk/org-create-checkbox ()
	(interactive)
	(setq current-prefix-arg '(4))
	(call-interactively 'org-toggle-checkbox))

  ;; org return - doesn't work with org 8.x and babel clashes with org 9.x
  ;; Thanks to http://kitchingroup.cheme.cmu.edu/blog/2017/04/09/A-better-return-in-org-mode
;;   (defun sk/org-return (&optional ignore)
;; 	"Add new list item, heading or table row with RET.
;; A double return on an empty element deletes it.
;; Use a prefix arg to get regular RET. "
;; 	(interactive "P")
;; 	(if ignore
;; 		(org-return)
;; 	  (cond
;; 	   ((org-in-item-p)
;; 		(if (org-element-property :contents-begin (org-element-context))
;; 			(org-insert-heading)
;; 		  (beginning-of-line)
;; 		  (setf (buffer-substring
;; 				 (line-beginning-position) (line-end-position)) "")
;; 		  (org-return)))
;; 	   ((org-at-heading-p)
;; 		(if (not (string= "" (org-element-property :title (org-element-context))))
;; 			(progn (org-end-of-meta-data)
;; 				   (org-insert-heading))
;; 		  (beginning-of-line)
;; 		  (setf (buffer-substring
;; 				 (line-beginning-position) (line-end-position)) "")))
;; 	   ((org-at-table-p)
;; 		(if (-any?
;; 			 (lambda (x) (not (string= "" x)))
;; 			 (nth
;; 			  (- (org-table-current-dline) 1)
;; 			  (org-table-to-lisp)))
;; 			(org-return)
;; 		  ;; empty row
;; 		  (beginning-of-line)
;; 		  (setf (buffer-substring
;; 				 (line-beginning-position) (line-end-position)) "")
;; 		  (org-return)))
;; 	   (t
;; 		(org-return)))))
;; (define-key org-mode-map (kbd "RET") 'sk/org-return)

  ;; some nice hooks for org
  (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'org-mode-hook 'flyspell-mode)

  ;; More of those nice template expansion
  (add-to-list 'org-structure-template-alist '("A" "#+DATE: ?"))
  (add-to-list 'org-structure-template-alist '("C" "#+BEGIN_CENTER\n?\n#+END_CENTER\n"))
  (add-to-list 'org-structure-template-alist '("D" "#+DESCRIPTION: ?"))
  (add-to-list 'org-structure-template-alist '("E" "#+BEGIN_EXAMPLE\n?\n#+END_EXAMPLE\n"))
  (add-to-list 'org-structure-template-alist '("H" "#+LATEX_HEADER: ?"))
  (add-to-list 'org-structure-template-alist '("I" ":INTERLEAVE_PDF: ?"))
  (add-to-list 'org-structure-template-alist '("L" "#+BEGIN_LaTeX\n?\n#+END_LaTeX"))
  (add-to-list 'org-structure-template-alist '("M" "#+LATEX_HEADER: \\usepackage{minted}\n"))
  (add-to-list 'org-structure-template-alist '("N" "#+NAME: ?"))
  (add-to-list 'org-structure-template-alist '("P" "#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"org.css\"/>\n"))
  (add-to-list 'org-structure-template-alist '("S" "#+SUBTITLE: ?"))
  (add-to-list 'org-structure-template-alist '("T" ":DRILL_CARD_TYPE: twosided"))
  (add-to-list 'org-structure-template-alist '("V" "#+BEGIN_VERSE\n?\n#+END_VERSE"))
  (add-to-list 'org-structure-template-alist '("X" "#+EXCLUDE_TAGS: reveal?"))
  (add-to-list 'org-structure-template-alist '("a" "#+AUTHOR: ?"))
  (add-to-list 'org-structure-template-alist '("c" "#+CAPTION: ?"))
  (add-to-list 'org-structure-template-alist '("d" "#+OPTIONS: ':nil *:t -:t ::t <:t H:3 \\n:nil ^:t arch:headline\n#+OPTIONS: author:t email:nil e:t f:t inline:t creator:nil d:nil date:t\n#+OPTIONS: toc:nil num:nil tags:nil todo:nil p:nil pri:nil stat:nil c:nil d:nil\n#+LATEX_HEADER: \\usepackage[margin=2cm]{geometry}\n#+LANGUAGE: en\n\n#+REVEAL_TRANS: slide\n#+REVEAL_THEME: white\n#+REVEAL_ROOT: file:///Users/sriramkswamy/Documents/github/reveal.js\n\n?"))
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
  (add-to-list 'org-structure-template-alist '("v" "#+BEGIN_VERBATIM\n?\n#+END_VERBATIM"))

  ;; organization bindings
  (ryo-modal-key "m e b" 'org-shiftleft :mode 'org-mode)
  (ryo-modal-key "m e u" 'org-shiftup :mode 'org-mode)
  (ryo-modal-key "m e d" 'org-shiftdown :mode 'org-mode)
  (ryo-modal-key "m e f" 'org-shiftright :mode 'org-mode)
  (ryo-modal-key "m e h" 'org-metaleft :mode 'org-mode)
  (ryo-modal-key "m e k" 'org-metaup :mode 'org-mode)
  (ryo-modal-key "m e j" 'org-metadown :mode 'org-mode)
  (ryo-modal-key "m e l" 'org-metaright :mode 'org-mode)
  (ryo-modal-key "m s u" 'org-move-item-up :mode 'org-mode)
  (ryo-modal-key "m s d" 'org-move-item-down :mode 'org-mode)
  (ryo-modal-key "m s h" 'org-promote-subtree :mode 'org-mode)
  (ryo-modal-key "m s l" 'org-demote-subtree :mode 'org-mode)
  (ryo-modal-key "m s j" 'org-move-subtree-down :mode 'org-mode)
  (ryo-modal-key "m s k" 'org-move-subtree-up :mode 'org-mode)

  ;; motion bindings
  (ryo-modal-key "m h" 'org-up-element :mode 'org-mode)
  (ryo-modal-key "m j" 'outline-next-visible-heading :mode 'org-mode)
  (ryo-modal-key "m k" 'outline-previous-visible-heading :mode 'org-mode)
  (ryo-modal-key "m l" 'org-down-element :mode 'org-mode)
  (ryo-modal-key "m f" 'org-next-link :mode 'org-mode)
  (ryo-modal-key "m b" 'org-previous-link :mode 'org-mode)
  (ryo-modal-key "m n" 'org-next-block :mode 'org-mode)
  (ryo-modal-key "m p" 'org-previous-block :mode 'org-mode)
  (ryo-modal-key "m /" 'ace-link-org :mode 'org-mode)
  (ryo-modal-key "m RET" 'org-open-at-point :mode 'org-mode)

  ;; editing bindings
  (ryo-modal-key "m ," 'org-toggle-heading :mode 'org-mode)
  (ryo-modal-key "m *" 'org-list-make-subtree :mode 'org-mode)
  (ryo-modal-key "m -" 'org-ctrl-c-minus :mode 'org-mode)
  (ryo-modal-key "m |" 'org-table-create-or-convert-from-region :mode 'org-mode)
  (ryo-modal-key "m y" 'org-copy-subtree :mode 'org-mode)
  (ryo-modal-key "m d" 'org-cut-subtree :mode 'org-mode)
  (ryo-modal-key "m i" 'org-insert-link :mode 'org-mode)
  (ryo-modal-key "m t" 'org-toggle-link-display :mode 'org-mode)
  (ryo-modal-key "m r" 'org-add-note :mode 'org-mode)
  (ryo-modal-key "m z" 'org-footnote-new :mode 'org-mode)
  (ryo-modal-key "m w" 'org-insert-heading :mode 'org-mode :exit t)
  (ryo-modal-key "m v" 'org-insert-heading-respect-content :mode 'org-mode :exit t)
  (ryo-modal-key "m c c" 'org-date-from-calendar :mode 'org-mode)
  (ryo-modal-key "m c v" 'org-goto-calendar :mode 'org-mode)
  (ryo-modal-key "m c i" 'org-clock-in :mode 'org-mode)
  (ryo-modal-key "m c o" 'org-clock-out :mode 'org-mode)

  ;; misc
  (ryo-modal-key "m a a" 'org-set-tags-command :mode 'org-mode)
  (ryo-modal-key "m a t" 'org-todo :mode 'org-mode)
  (ryo-modal-key "m a d" 'org-deadline :mode 'org-mode)
  (ryo-modal-key "m a s" 'org-schedule :mode 'org-mode)
  (ryo-modal-key "m m" 'org-export-dispatch :mode 'org-mode)
  (ryo-modal-key "m x" 'org-preview-latex-fragment :mode 'org-mode)
  (ryo-modal-key "m u" 'interleave :mode 'org-mode)
  (ryo-modal-key "m g" 'org-toggle-inline-images :mode 'org-mode)
  (ryo-modal-key "m o" 'org-babel-tangle :mode 'org-mode)

  ;; custom functions
  (ryo-modal-key "m q" 'sk/org-create-checkbox :mode 'org-mode))

;; interleaved notes
(use-package interleave
  :ensure t
  :commands (interleave
			 interleave-pdf-mode)
  :init
  (setq interleave-org-notes-dir-list '("~/Dropbox/org/notes"))
  :config
  (add-hook 'doc-view-mode-hook 'doc-view-fit-page-to-window))

;; Trello and Org - an experiment
(use-package org-trello
  :ensure t
  :init
  ;; (require 'helm)
  ;; (setq org-trello-input-completion-mechanism 'helm)
  (setq org-trello-current-prefix-keybinding "C-c O")
  :diminish org-trello-mode
  :commands (org-trello-abort-sync
			 org-trello-add-card-comment
			 org-trello-archive-card
			 org-trello-archive-cards
			 org-trello-assign-me
			 org-trello-bug-report
			 org-trello-check-setup
			 org-trello-clean-org-trello-data
			 org-trello-close-board
			 org-trello-create-board-and-install-metadata
			 org-trello-delete-card-comment
			 org-trello-delete-setup
			 org-trello-help-describing-bindings
			 org-trello-install-board-metadata
			 org-trello-install-key-and-token
			 org-trello-jump-to-trello-board
			 org-trello-jump-to-trello-card
			 org-trello-kill-cards
			 org-trello-kill-entity
			 org-trello-show-board-labels
			 org-trello-sync-buffer
			 org-trello-sync-card
			 org-trello-sync-comment
			 org-trello-toggle-assign-me
			 org-trello-toggle-assign-user
			 org-trello-update-board-metadata
			 org-trello--version
			 org-trello-mode)
  :config
  (org-trello-mode))

;; sync the last stored capture
(defun sk/trello-last-captured ()
  (interactive)
  (org-capture-goto-last-stored)
  (org-trello-sync-buffer)
  (mode-line-other-buffer))

;; after capturing, send it to trello
(add-hook 'org-capture-after-finalize-hook 'sk/trello-last-captured)

;; bindings and hints
(ryo-modal-key "SPC u"
			   '(("a" org-trello-sync-buffer)
				 ("r" org-trello-sync-card)
				 ("m" org-trello-sync-comment)
				 ("b" org-trello-install-board-metadata)
				 ("n" org-trello-create-board-and-install-metadata)
				 ("u" org-trello-update-board-metadata)
				 ("c" org-trello-add-card-comment)
				 ("g" org-trello-jump-to-trello-card)
				 ("j" org-trello-jump-to-trello-board)
				 ("i" org-trello-toggle-assign-me)
				 ("o" org-trello-toggle-assign-user)
				 ("p" org-trello-clean-org-trello-data)
				 ("k" org-trello-kill-cards)
				 ("d" org-trello-delete-card-comment)
				 ("x" org-trello-close-board)
				 ("e" org-trello-kill-entity)
				 ("h" org-trello-help-describing-bindings)
				 ("q" org-trello-archive-cards)
				 ("w" org-trello-archive-card)
				 ("z" org-trello-abort-sync)
				 ("l" org-trello-show-board-labels)
				 ("f" org-trello-install-key-and-token)
				 ("v" org-trello-check-setup)))

;; which key hints
(which-key-add-key-based-replacements
  "SPC u" "trello"
  "SPC u a" "sync all"
  "SPC u r" "sync card"
  "SPC u m" "sync comment"
  "SPC u b" "board metadata"
  "SPC u n" "new board"
  "SPC u u" "update board"
  "SPC u c" "add comment"
  "SPC u g" "goto card"
  "SPC u j" "jump to board"
  "SPC u i" "assign me"
  "SPC u o" "assign other"
  "SPC u p" "clean data"
  "SPC u k" "kill cards"
  "SPC u d" "delete comment"
  "SPC u x" "close board"
  "SPC u e" "kill entity"
  "SPC u h" "help"
  "SPC u q" "archive cards"
  "SPC u w" "archive one card"
  "SPC u z" "abort sync"
  "SPC u l" "show labels"
  "SPC u f" "first time setup"
  "SPC u v" "check setup")

;; load the extra org goodness
(defun sk/org-custom-load ()
  (interactive)
  (require 'org)
  (require 'ob)
  (require 'ox)
  ;; Some extra exports
  (require 'ox-reveal)
  (require 'ox-twbs)
  (require 'ox-rst)
  (require 'ox-gfm)
  ;; async repl support
  (require 'ob-async)
  (add-to-list 'org-ctrl-c-ctrl-c-hook 'ob-async-org-babel-execute-src-block)
  ;; JavaScript repl support
  (require 'ob-js)
  ;; use ipython for python
  ;; (require 'ob-ipython)
  ;; Babel load
  (org-babel-do-load-languages 'org-babel-load-languages
							   '((emacs-lisp . t)
								 ;; (dot . t)
								 ;; (ditaa . t)
								 (latex . t)
								 (gnuplot . t)
								 (sh . t)
								 (js . t)
								 (C . t)
								 (ledger . t)
								 (R . t)
								 (octave . t)
								 (matlab . t)
								 (python . t)))
  ;; fix the default c and c++ compilers
  (defvar org-babel-C-compiler "gcc"
	"Command used to compile a C source code file into an executable.")
  (defvar org-babel-C++-compiler "g++"
	"Command used to compile a C++ source code file into an executable."))
(ryo-modal-key "c o g" 'sk/org-custom-load)

;; diminish org indent mode
(defun sk/diminish-org-indent ()
  (interactive)
  (diminish 'org-indent-mode ""))
(add-hook 'org-indent-mode-hook 'sk/diminish-org-indent)

;; push agenda files to mobile viewer when idle for 5 mins
(run-with-idle-timer (* 5 60) t
					 (lambda ()
					   (org-mobile-pull)
					   (org-mobile-push)))

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
 _A_: date          _l_: label       _S_: subtitle       _e_: elisp src   _Q_: quote                     _L_: latex      _I_: interleave        _q_: quit
 _c_: caption       _N_: name        _k_: keywords       _E_: example     _s_: src                       _x_: export     _T_: drill two-sided
 _f_: file tags     _o_: options     _M_: minted         _h_: html        _v_: verbatim                  _X_: noexport
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
(add-hook 'org-mode-hook 'sk/org-template-hook)

;; ryo major mode
(which-key-add-major-mode-key-based-replacements 'org-mode

  ;; motion
  "m h" "up level"
  "m j" "next level"
  "m k" "prev level"
  "m l" "down level"
  "m f" "next link"
  "m b" "prev link"
  "m n" "next block"
  "m p" "prev block"
  "m /" "ace link"
  "m RET" "open"

  ;; org ref
  "m o" "org ref"
  "m o c" "cite"
  "m o l" "label"
  "m o r" "reference"
  "m o f" "list of figures"
  "m o t" "list of tables"
  "m o i" "list index"

  ;; organizing
  "m e" "edit heading"
  "m e u" "priority up/shift up"
  "m e b" "todo left/shift left"
  "m e f" "todo right/shift right"
  "m e d" "priority up/shift down"
  "m e k" "heading up/meta up"
  "m e h" "heading promote/meta left"
  "m e l" "heading demote/meta right"
  "m e j" "heading down/meta down"
  "m s" "subtree/item"
  "m s h" "subtree promote"
  "m s j" "subtree down"
  "m s k" "subtree up"
  "m s l" "subtree demote"
  "m s u" "item up"
  "m s d" "item down"

  ;; editing bindings
  "m ," "toggle heading"
  "m *" "list to subtree"
  "m -" "toggle list"
  "m |" "create or convert to table"
  "m y" "copy subtree"
  "m d" "cut subtree"
  "m i" "insert link"
  "m u" "interleave"
  "m t" "toggle links"
  "m r" "add note"
  "m z" "add footnote"
  "m w" "insert heading"
  "m v" "heading respect content"
  "m q" "create checkbox"
  "m c" "cal/clock"
  "m c c" "cal date"
  "m c v" "goto cal"
  "m c i" "clock in"
  "m c o" "clock out"

  ;; misc
  "m m" "export"
  "m o" "tangle"
  "m x" "toggle latex"
  "m g" "toggle image"
  "m a" "agenda"
  "m a a" "set tags"
  "m a t" "todo"
  "m a d" "deadline"
  "m a s" "schedule")

;; provide this configuration
(provide 'sk-org)
