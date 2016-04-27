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
                            ("blog" . ?b)
                            ("meeting" . ?m)
                            ("article" . ?a) ;; temporary
                            ("research" . ?r) ;; temporary
                            ("courses" . ?c) ;; temporary
                            ("fall" . ?f) ;; temporary
                            ("spring" . ?s) ;; temporary
                            ("ledger" . ?l)
                            ("gubby" . ?g)
                            ("movies" . ?M)
                            ("online" . ?o)
                            ("cash" . ?$)
                            ("card" . ?d)
                            ("idea" . ?i)
                            ("personal" . ?p)
                            ("project" . ?t)
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
(setq org-agenda-files (list "~/Dropbox/org/blog.org"
                             "~/Dropbox/org/errands.org"
                             "~/Dropbox/org/phd.org"
                             "~/Dropbox/org/ledger.org"
                             "~/Dropbox/org/notes.org"))
(setq org-deadline-warning-days 2
      org-agenda-span 'fortnight
      org-agenda-skip-scheduled-if-deadline-is-shown t)

;; Links
(setq org-link-abbrev-alist
      '(("bugzilla"  . "http://10.1.2.9/bugzilla/show_bug.cgi?id=")
        ("url-to-ja" . "http://translate.google.fr/translate?sl=en&tl=ja&u=%h")
        ("google"    . "http://www.google.com/search?q=")
        ("gmaps"      . "http://maps.google.com/maps?q=%s")))

;; Capture templates
(setq org-capture-templates
      '(("n"               ; key
         "Note"            ; name
         entry             ; type
         (file+headline "~/Dropbox/org/notes.org" "Notes")  ; target
         "* %? %(org-set-tags)  :note: \n:PROPERTIES:\n:Created: %U\n:Linked: %A\n:END:\n%i"  ; template
         :prepend t        ; properties
         :empty-lines 1    ; properties
         :created t        ; properties
        )
        ("l"               ; key
         "Ledger"          ; name
         entry             ; type
         (file+datetree+prompt "~/Dropbox/org/ledger.org" "Ledger")  ; target
         "* %? %(org-set-tags)  :accounts: \n:PROPERTIES:\n:Created: %U\n:END:\n%i\nAmount: $%^{amount}\n"  ; template
         :prepend t        ; properties
         :empty-lines 1    ; properties
         :created t        ; properties
        )
        ("w"               ; key
         "Work"            ; name
         entry             ; type
         (file+headline "~/Dropbox/org/phd.org" "Work")  ; target
         "* TODO %^{Todo} %(org-set-tags)  :work: \n:PROPERTIES:\n:Created: %U\n:END:\n%i\n%?"  ; template
         :prepend t        ; properties
         :empty-lines 1    ; properties
         :created t        ; properties
        )
        ("a"               ; key
         "Article"         ; name
         entry             ; type
         (file+headline "~/Dropbox/org/phd.org" "Article")  ; target
         "* %^{Title} %(org-set-tags)  :article: \n:PROPERTIES:\n:Created: %U\n:Linked: %A\n:END:\n%i\nBrief description:\n%?"  ; template
         :prepend t        ; properties
         :empty-lines 1    ; properties
         :created t        ; properties
        )
        ("m"               ; key
         "Meeting"         ; name
         entry             ; type
         (file+datetree "~/Dropbox/org/phd.org" "Meeting")  ; target
         "* %^{Title} %(org-set-tags)  :meeting: \n:PROPERTIES:\n:Created: %U\n:END:\n%i\nMinutes of the meeting:\n%?"  ; template
         :prepend t        ; properties
         :empty-lines 1    ; properties
         :created t        ; properties
        )
        ("j"               ; key
         "Jobs"            ; name
         entry             ; type
         (file+headline "~/Dropbox/org/notes.org" "Jobs")  ; target
         "* %^{Title, Company} %(org-set-tags)  :job: \n:PROPERTIES:\n:Created: %U\n:END:\n%i\nReferral: %^{referral}\nExperience: %^{experience}\nDescription:\n%?"  ; template
         :prepend t        ; properties
         :empty-lines 1    ; properties
         :created t        ; properties
        )
        ("M"               ; key
         "Movies"          ; name
         entry             ; type
         (file+headline "~/Dropbox/org/notes.org" "Movies")  ; target
         "* %^{Movie} %(org-set-tags)  :film: \n:PROPERTIES:\n:Created: %U\n:END:\n%i\nNetflix?: %^{netflix? Yes/No}\nGenre: %^{genre}\nDescription:\n%?"  ; template
         :prepend t        ; properties
         :empty-lines 1    ; properties
         :created t        ; properties
        )
        ("b"               ; key
         "Blog"            ; name
         entry             ; type
         (file+headline "~/Dropbox/org/blog.org" "Blog")  ; target
         "* %^{Title} %(org-set-tags)  :blog: \n:PROPERTIES:\n:Created: %U\n:Tags: %^{Tags}p\n:END:\n%i\n%?"  ; template
         :prepend t        ; properties
         :empty-lines 1    ; properties
         :created t        ; properties
        )
        ("e"               ; key
         "Errands"         ; name
         entry             ; type
         (file+headline "~/Dropbox/org/errands.org" "Errands")  ; target
         "* TODO %^{Todo} %(org-set-tags)  :errands: \n:PROPERTIES:\n:Created: %U\n:END:\n%i\n%?"  ; template
         :prepend t        ; properties
         :empty-lines 1    ; properties
         :created t        ; properties
        )
        ("c"               ; key
         "Courses"         ; name
         entry             ; type
         (file+headline "~/Dropbox/org/phd.org" "Courses")  ; target
         "* %^{Course} %(org-set-tags)  :courses: \n:PROPERTIES:\n:Created: %U\n:END:\n%i\n%?"  ; template
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
(require 'ox-reveal)
(setq org-reveal-title-slide-template
 "<h1>%t</h1>
<h3>%a</h3>
")

;; Restructred text and pandoc
(sk/require-package 'ox-rst)
(sk/require-package 'ox-pandoc)

;; Drag and drop files into org mode
(sk/require-package 'org-download)

;; Deft for quick org notes access
(sk/require-package 'deft)
(setq deft-extensions '("org")
      deft-recursive t
      deft-use-filename-as-title t
      deft-directory "~/Dropbox/org")

;; Org ref for academic papers
(sk/require-package 'org-ref)
(setq org-ref-bibliography-notes "~/Dropbox/references/references.org"
      org-ref-default-bibliography '("~/Dropbox/references/references.bib")
      org-ref-pdf-directory "~/Dropbox/references/pdfs/")

;; Fancy bullets
(sk/require-package 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Blog with org
(sk/require-package 'org-jekyll)

;; Beautify theme
(load "org-beautify-theme.el")

;; which key explanations
(require 'sk-org-hydra)
(require 'sk-org-bindings)
(require 'sk-org-functions)
(require 'sk-org-diminish)

(provide 'sk-org)
;;; sk-org.el ends here
