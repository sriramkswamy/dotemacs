;;; sk-org.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Org mode configurations and helpers

;;; Code:

;; Basic settings
(setq org-directory "~/Dropbox/org"
      org-completion-use-ido nil
      ;; Emphasis
      org-hide-emphasis-markers t
      ;; Indent
      org-startup-indented t
      org-hide-leading-stars t
      ;; Images
      org-image-actual-width '(300)
      ;; Source code
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      ;; Links
      org-return-follows-link t
      ;; Quotes
      org-export-with-smart-quotes t
      ;; Citations
      org-latex-to-pdf-process '("pdflatex %f" "biber %b" "pdflatex %f" "pdflatex %f")
      org-export-backends '(ascii beamer html latex md))

;; Refile settings
(setq org-refile-targets '((nil :maxlevel . 9)
                           (org-agenda-files :maxlevel . 9)))
(setq org-refile-use-outline-path t
      org-outline-path-complete-in-steps nil)

;; Tags with fast selection keys
(setq org-tag-alist (quote (("errand" . ?e)
                            ("books" . ?b)
                            ("meeting" . ?m)
                            ("article" . ?a) ;; temporary
                            ("reference" . ?r) ;; temporary
                            ("courses" . ?c) ;; temporary
			    ("coding" . ?C)
                            ("films" . ?f)
                            ("story" . ?s)
                            ("ledger" . ?l)
                            ("gubby" . ?g)
			    ("online" . ?o)
                            ("cash" . ?$)
                            ("card" . ?d)
                            ("idea" . ?i)
			    ("technical" . ?t)
                            ("personal" . ?p)
                            ("project" . ?P)
                            ("job" . ?j)
                            ("work" . ?w)
                            ("home" . ?h)
                            ("vague" . ?v)
                            ("noexport" . ?x)
                            ("note" . ?n))))

;; TODO Keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "HOLD(h@/!)" "|" "DONE(d!)")
        (sequence "|" "CANCELLED(c@)")))

;; Agenda settings
(setq org-agenda-files (list
			"~/Dropbox/org/blog.org"
			"~/Dropbox/org/errands.org"
			"~/Dropbox/org/phd.org"
			"~/Dropbox/org/references/articles.org"
			"~/Dropbox/org/ledger.org"
			"~/Dropbox/org/notes.org"
			"~/Dropbox/org/fun.org"
			))

(setq org-deadline-warning-days 2
      org-agenda-span 'fortnight
      org-agenda-skip-scheduled-if-deadline-is-shown t)

;; Links
(setq org-link-abbrev-alist
      '(("bugzilla"  . "http://10.1.2.9/bugzilla/show_bug.cgi?id=")
        ("url-to-ja" . "http://translate.google.fr/translate?sl=en&tl=ja&u=%h")
        ("google"    . "http://www.google.com/search?q=")
        ("gmaps"      . "http://maps.google.com/maps?q=%s")))

;; Lispy prompt for org
(defvar oc-capture-prmt-history nil
  "History of prompt answers for org capture.")
(defun oc/prmt (prompt variable)
  "PROMPT for string, save it to VARIABLE and insert it."
  (make-local-variable variable)
  (set variable (read-string (concat prompt ": ") nil oc-capture-prmt-history)))

;; Capture templates
(setq org-capture-templates
      '(("n"               ; key
         "Note"            ; name
         entry             ; type
         (file+headline "~/Dropbox/org/notes.org" "Notes")  ; target
         "* %? %(org-set-tags)  :note:
:PROPERTIES:
:Created: %U
:Linked: %A
:END:
%i"  ; template
         :prepend t        ; properties
         :empty-lines 1    ; properties
         :created t        ; properties
        )
        ("l"               ; key
         "Ledger"          ; name
         entry             ; type
         (file+datetree "~/Dropbox/org/ledger.org" "Ledger")  ; target
         "* %(oc/prmt \"Name of expense\" 'expense-name) %(org-set-tags)  :accounts:
:PROPERTIES:
:Created: %U
:END:
%i
#+NAME: %(progn expense-name)-%t
#+BEGIN_SRC ledger :noweb yes
%^{Date of expense (yyyy/mm/dd)} %^{'*' if cleared, else blank}
    %^{Account name}                                $%^{Amount}
    %?
#+END_SRC
"  ; template
         :prepend t        ; properties
         :empty-lines 1    ; properties
         :created t        ; properties
        )
        ("w"               ; key
         "Work"            ; name
         entry             ; type
         (file+headline "~/Dropbox/org/phd.org" "Work")  ; target
         "* TODO %^{Todo} %(org-set-tags)  :work:
:PROPERTIES:
:Created: %U
:END:
%i
%?"  ; template
         :prepend t        ; properties
         :empty-lines 1    ; properties
         :created t        ; properties
        )
        ("s"               ; key
         "story"           ; name
         entry             ; type
         (file+headline "~/Dropbox/org/fun.org" "Reading")  ; target
         "* %^{Title} %(org-set-tags)
:PROPERTIES:
:Created: %U
:END:
%i
%?"  ; template
         :prepend t        ; properties
         :empty-lines 1    ; properties
         :created t        ; properties
        )
        ("m"               ; key
         "Meeting"         ; name
         entry             ; type
         (file+datetree "~/Dropbox/org/phd.org" "Meeting")  ; target
         "* %^{Title} %(org-set-tags)  :meeting:
:PROPERTIES:
:Created: %U
:END:
%i
Minutes of the meeting:
%?"  ; template
         :prepend t        ; properties
         :empty-lines 1    ; properties
         :created t        ; properties
        )
        ("j"               ; key
         "Jobs"            ; name
         entry             ; type
         (file+headline "~/Dropbox/org/notes.org" "Jobs")  ; target
         "* %^{Title, Company} %(org-set-tags)  :job:
:PROPERTIES:
:Created: %U
:END:
%i
Referral: %^{referral}
Experience: %^{experience}
Description:
%?"  ; template
         :prepend t        ; properties
         :empty-lines 1    ; properties
         :created t        ; properties
        )
        ("f"               ; key
         "films"          ; name
         entry             ; type
         (file+headline "~/Dropbox/org/fun.org" "Movies")  ; target
         "* %^{Movie} %(org-set-tags)  :film:
:PROPERTIES:
:Created: %U
:END:
%i
Netflix?: %^{netflix? Yes/No}
Genre: %^{genre}
Description:
%?"  ; template
         :prepend t        ; properties
         :empty-lines 1    ; properties
         :created t        ; properties
        )
        ("b"               ; key
         "Blog"            ; name
         entry             ; type
         (file+headline "~/Dropbox/org/blog.org" "Blog")  ; target
         "* %^{Title} %(org-set-tags)  :blog:
:PROPERTIES:
:Created: %U
:END:
%i
%?"  ; template
         :prepend t        ; properties
         :empty-lines 1    ; properties
         :created t        ; properties
        )
        ("e"               ; key
         "Errands"         ; name
         entry             ; type
         (file+headline "~/Dropbox/org/errands.org" "Errands")  ; target
         "* TODO %^{Todo} %(org-set-tags)  :errands:
:PROPERTIES:
:Created: %U
:END:
%i
%?"  ; template
         :prepend t        ; properties
         :empty-lines 1    ; properties
         :created t        ; properties
        )
        ("c"               ; key
         "Courses"         ; name
         entry             ; type
         (file+headline "~/Dropbox/org/phd.org" "Courses")  ; target
         "* %^{Course} %(org-set-tags)  :courses:
:PROPERTIES:
:Created: %U
:END:
%i
%?"  ; template
         :prepend t        ; properties
         :empty-lines 1    ; properties
         :created t        ; properties
        )
        ))

;; LaTeX
(sk/require-package 'cdlatex)
(add-hook 'org-mode-hook 'org-cdlatex-mode)

;; Babel for languages
(sk/require-package 'babel)
(setq org-confirm-babel-evaluate nil)

;; For PDF note taking
(sk/require-package 'interleave)

;; Python support
(sk/require-package 'ob-ipython)

;; Export using reveal and impress-js
(sk/require-package 'ox-reveal)
(setq org-reveal-title-slide-template
      "<h1>%t</h1>
<h3>%a</h3>
"
      )

;; Export using twitter bootstrap
(sk/require-package 'ox-twbs)

;; Restructred text and pandoc
(sk/require-package 'ox-rst)
(sk/require-package 'ox-pandoc)

;; Drag and drop images into org mode
(sk/require-package 'org-download)

;; Put a file system tree right into org
(sk/require-package 'org-fstree)

;; Deft for quick org notes access
(sk/require-package 'deft)
(setq deft-extensions '("org")
      deft-recursive t
      deft-use-filename-as-title t
      deft-directory "~/Dropbox/org")

;; Org ref for academic papers
(sk/require-package 'org-ref)
(setq org-ref-notes-directory "~/Dropbox/org/references/notes"
      org-ref-bibliography-notes "~/Dropbox/org/references/articles.org"
      org-ref-default-bibliography '("~/Dropbox/org/references/multiphysics.bib" "~/Dropbox/org/references/chanceconstraints.bib")
      org-ref-pdf-directory "~/Dropbox/org/references/pdfs/")
(defun sk/org-ref-bibtex-custom-load ()
  (interactive)
  (require 'org-ref)
  (require 'org-ref-pdf)
  (require 'org-ref-url-utils))
(defun sk/org-ref-latex-custom-load ()
  (interactive)
  (require 'org-ref-latex))
(add-hook 'bibtex-mode-hook 'sk/org-ref-bibtex-custom-load)
(add-hook 'latex-mode-hook 'sk/org-ref-latex-custom-load)

;; Fancy bullets
(sk/require-package 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Blog with org
(sk/require-package 'org-page)
(setq op/repository-directory "/Users/sriramkswamy/Downloads/blog/"
      op/site-domain "http://blah.blah"
      op/theme 'mdo)

;; Org load languages
(defun sk/org-custom-load ()
  (interactive)
  (require 'org-page)
  (require 'org-fstree)
  (require 'ox-reveal)
  (require 'ox-twbs)
  (require 'org-ref)
  (require 'org-ref-latex)
  (require 'org-ref-pdf)
  (require 'org-ref-url-utils)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     ;; (dot . t)
     ;; (ditaa . t)
     (latex . t)
     ;; (gnuplot . t)
     (sh . t)
     ;; (C . t)
     (ledger . t)
     ;; (R . t)
     ;; (octave . t)
     (matlab . t)
     (python . t))))

;; which key explanations
(require 'sk-org-hydra)
(require 'sk-org-bindings)
(require 'sk-org-diminish)

(provide 'sk-org)
;;; sk-org.el ends here
