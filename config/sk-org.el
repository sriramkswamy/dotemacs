;; Basic settings
(setq org-directory "~/Dropbox/notes/"
      org-completion-use-ido nil
      ;; Indent
      org-startup-indented t
      org-hide-leading-stars t
      ;; Images
      org-image-actual-width '(300)
      ;; Source code
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      ;; Quotes
      org-export-with-smart-quotes t
      ;; Citations
      org-latex-to-pdf-process '("pdflatex %f" "biber %b" "pdflatex %f" "pdflatex %f"))

;; Tags with fast selection keys
(setq org-tag-alist (quote (("errand" . ?e)
                            ("blog" . ?b)
                            ("personal" . ?k)
                            ("report" . ?r)
                            ("thesis" . ?t) ;; temporary
                            ("accounts" . ?a)
                            ("lubby" . ?l)
                            ("movie" . ?m)
                            ("netflix" . ?N)
                            ("via" . ?v)
                            ("idea" . ?i)
                            ("project" . ?p)
                            ("job" . ?j)
                            ("work" . ?w)
                            ("home" . ?h)
                            ("noexport" . ?x)
                            ("Technial" . ?T)
                            ("Random" . ?R)
                            ("Crafts" . ?C)
                            ("story/news" . ?s)
                            ("note" . ?n))))

;; TODO Keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "HOLD(h@/!)" "|" "DONE(d!)")
        (sequence "|" "CANCELLED(c@)")))

;; Agenda settings
(setq org-agenda-files (list "~/Dropbox/notes/work.org"
                             "~/Dropbox/notes/blog.org"
                             "~/Dropbox/notes/ledger.org"
                             "~/Dropbox/notes/notes.org"))
(setq org-deadline-warning-days 7
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
      '(("n" "Note" entry (file+headline "~/Dropbox/notes/notes.org" "Notes")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("r" "Report" entry (file+headline "~/Dropbox/notes/work.org" "Thesis Notes") ;; temporary
         "* %?\nEntered on %U\n  %i\n  %a")
        ("j" "Job leads/status" entry (file+headline "~/Dropbox/notes/notes.org" "Job leads/status")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("s" "Story/News" entry (file+headline "~/Dropbox/notes/notes.org" "Story/News")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("a" "Accounts - Ledger" entry (file+datetree "~/Dropbox/notes/ledger.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("b" "Blog" entry (file+datetree "~/Dropbox/notes/blog.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("p" "Project" entry (file+headline "~/Dropbox/notes/notes.org" "Projects")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("w" "Work" entry (file+headline "~/Dropbox/notes/work.org" "Work")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("h" "Home" entry (file+headline "~/Dropbox/notes/notes.org" "Home")
         "* %?\nEntered on %U\n  %i\n  %a")))

;; LaTeX
(sk/require-package 'cdlatex)
(defun sk/diminish-org ()
  (interactive)
  (diminish 'org-indent-mode "")
  (diminish 'org-cdlatex-mode ""))
(add-hook 'org-mode-hook 'sk/diminish-org)
(add-hook 'org-mode-hook 'org-cdlatex-mode)

;; Babel for languages
(sk/require-package 'babel)
(setq org-confirm-babel-evaluate nil)
;; Org load languages
(defun sk/org-custom-load ()
  (interactive)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     ;; (dot . t)
     ;; (ditaa . t)
     (latex . t)
     ;; (gnuplot . t)
     ;; (sh . t)
     ;; (C . t)
     ;; (R . t)
     ;; (octave . t)
     (matlab . t)
     (python . t))))
(sk/require-package 'ob-ipython)

;; Export using reveal and impress-js
(sk/require-package 'ox-reveal)
(sk/require-package 'ox-impress-js)

;; Restructred text and pandoc
(sk/require-package 'ox-rst)
(sk/require-package 'ox-pandoc)

;; Org navigation and manipulation - hydra
(defhydra sk/hydra-org-manipulate (:color red
                                   :hint nil)
  "
^Move heading^      | ^Item^ | ^Menu^
^^^^^^^^^^^^------------------|------|--------------
^ ^ _k_ ^ ^   _<_ promote | _u_p   | _H_ome  e_x_ecute
_h_ ^+^ _l_   _>_ demote  | _d_own | _O_rg   _q_uit
^ ^ _j_ ^ ^             |      |
"
  ("h" org-metaleft)
  ("l" org-metaright)
  ("j" org-metadown)
  ("k" org-metaup)
  ("<" org-promote)
  (">" org-demote)
  ("d" org-move-item-down)
  ("u" org-move-item-up)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("O" sk/hydra-of-org/body :exit t)
  ("x" counsel-M-x :color blue)
  ("q" nil :color blue))

;; Org table manipulation - hydra
(defhydra sk/hydra-org-tables (:color red
                               :hint nil)
  "
^Field^ | ^Shift^ | ^Insert^  | ^Delete^ | ^Field^ | ^Table^    | ^Formula^ | ^Menu^
^^^^^^^^^^^------|-------|---------|--------|-------|----------|---------|-------------
^ ^ _k_ ^ ^ | ^ ^ _p_ ^ ^ | _r_ow     | _R_ow    | _e_dit  | _a_lign    | _+_ sum   | _O_rg  e_x_ecute
_h_ ^+^ _l_ | _b_ ^+^ _f_ | _c_olumn  | _C_olumn | _b_lank | _|_ create | _=_ eval  | _H_ome _q_uit
^ ^ _j_ ^ ^ | ^ ^ _n_ ^ ^ | _-_ hline |        | _i_nfo  |          | _f_ edit  |
"
  ("a" org-table-align)
  ("l" org-table-next-field)
  ("h" org-table-previous-field)
  ("j" org-table-end-of-field)
  ("k" org-table-beginning-of-field)
  ("r" org-table-insert-row)
  ("c" org-table-insert-column)
  ("-" org-table-insert-hline)
  ("n" org-table-move-row-down)
  ("p" org-table-move-row-up)
  ("b" org-table-move-column-left)
  ("f" org-table-move-column-right)
  ("R" org-table-kill-row)
  ("C" org-table-delete-column)
  ("b" org-table-blank-field)
  ("e" org-table-edit-field)
  ("i" org-table-field-info)
  ("+" org-table-sum)
  ("=" org-table-eval-formula)
  ("f" org-table-edit-formulas)
  ("|" org-table-create-or-convert-from-region)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("O" sk/hydra-of-org/body :exit t)
  ("x" counsel-M-x :color blue)
  ("q" nil :color blue))

;; Org clock manipulation - hydra
(defhydra sk/hydra-org-clock (:color red
                              :hint nil)
  "
^Clock^              | ^Timer^ | ^Stamp^    | ^Menu^
^^^^^^^^^-------------------|-------|----------|---------
_i_n       _z_ resolve | _b_egin | _s_tamp    | _O_rg
_o_ut      _l_ast      | _e_nd   | _I_nactive | _H_ome
_r_eport   _c_ancel    | _t_imer |          | e_x_ecute
_d_isplay  _g_oto      | _T_ set |          | _q_uit
"
  ("i" org-clock-in)
  ("o" org-clock-out)
  ("r" org-clock-report)
  ("z" org-resolve-clocks)
  ("c" org-clock-cancel)
  ("d" org-clock-display)
  ("l" org-clock-in-last)
  ("g" org-clock-goto)
  ("t" org-timer)
  ("T" org-timer-set-timer)
  ("b" org-timer-start)
  ("e" org-timer-stop)
  ("s" org-time-stamp)
  ("I" org-time-stamp-inactive)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("O" sk/hydra-of-org/body :exit t)
  ("x" counsel-M-x :color blue)
  ("q" nil :color blue))

;; Org tags and todo manipulation - hydra
(defhydra sk/hydra-org-tag-todo (:color red
                                 :hint nil)
  "
^tags^        | ^TODO^     | ^Checkbox^     | ^Priority^   | ^Menu^
^^^^^^^^^^^^^------------|----------|--------------|------------|---------
_t_ags        | _T_ODO     | _c_heckbox     | _#_ priority | _O_rg
_v_iew        | _d_eadline | t_o_ggle       | _+_ increase | _H_ome
_m_atch       | _C_lose    | _u_pdate stats | _-_ decrease | e_x_ecute
s_p_arse-tree | _s_chedule | _r_eset        |            | _q_uit
            | _a_genda   | _U_pdate count |            |
"
  ("t" org-set-tags-command :color blue)
  ("v" org-tags-view)
  ("m" org-match-sparse-tree :color blue)
  ("p" org-sparse-tree :color blue)
  ("#" org-priority)
  ("+" org-priority-up)
  ("-" org-priority-down)
  ("T" org-todo :color blue)
  ("d" org-deadline :color blue)
  ("C" org-deadline-close :color blue)
  ("s" org-schedule :color blue)
  ("a" org-check-deadlines)
  ("c" org-checkbox)
  ("o" org-toggle-checkbox)
  ("U" org-update-checkbox-count-maybe)
  ("r" org-reset-checkbox-state-subtree)
  ("u" org-update-statistics-cookies)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("O" sk/hydra-of-org/body :exit t)
  ("x" counsel-M-x :color blue)
  ("q" nil :color blue))

;; Org drawer - hydra
(defhydra sk/hydra-org-drawer (:color red
                                 :hint nil)
  "
 ^Drawer^ | ^Property^ | ^Menu^
^^^^^^^^^^--------|----------|--------
 _i_nsert | _p_roperty | _O_rg
        | _s_et      | _H_ome
        | _d_elete   | e_x_ecute
        | _t_oggle   | _q_uit
"
  ("i" org-insert-drawer)
  ("p" org-insert-property-drawer)
  ("s" org-set-property)
  ("d" org-delete-property)
  ("t" org-toggle-ordered-property)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("O" sk/hydra-of-org/body :exit t)
  ("x" counsel-M-x :color blue)
  ("q" nil :color blue))

;; Deft for quickly accessing notes
(sk/require-package 'deft)
(setq deft-extensions '("org" "md" "txt" "tex")
      deft-recursive t
      deft-use-filename-as-title t
      deft-directory "~/Dropbox/notes")

;; Hydra for deft
(defhydra sk/hydra-deft (:color red
                         :hint nil)
  "
^Move^         | ^Deft^                       | ^Menu^
^^^^^^^^^^^^^-------------|----------------------------|--------------
^ ^ _k_ ^ ^   _g_oto | _n_ew       _f_ilter   _v_ersion | _O_rg   e_x_ecute
_h_ ^+^ _l_        | _o_pen      _c_lear    rena_m_e  | _H_ome  _q_uit
^ ^ _j_ ^ ^        | _a_rchive   _r_efresh          |
"
  ("j" next-line)
  ("k" previous-line)
  ("h" beginning-of-buffer)
  ("l" end-of-buffer)
  ("g" avy-goto-line)
  ("n" deft-new-file-named :color blue)
  ("f" deft-filter)
  ("c" deft-filter-clear)
  ("o" deft-open-file-other-window :color blue)
  ("r" deft-refresh)
  ("m" deft-rename-file)
  ("v" deft-show-version)
  ("a" deft-archive-file)
  ("x" counsel-M-x :color blue)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("O" sk/hydra-of-org/body :exit t)
  ("q" nil :color blue))
(defun sk/open-deft-and-start-hydra ()
  (interactive)
  (deft)
  (sk/hydra-deft/body))

;; Hydra of org-mode
(defhydra sk/hydra-of-org (:color pink
                           :hint nil)
  "
 ^Outline^       | ^Item^  | ^Table^ | ^Block^  | ^Link^    | ^State^     | ^Toggle^              | ^Subtree^ | ^Org and Menu^
 ^^^^^^^^^^^--------------|-------|-------|--------|---------|-----------|---------------------|---------|--------------------
 ^ ^ _k_ ^ ^  ^ ^ _p_ ^ ^  | ^ ^ _u_ ^ ^ | ^ ^ ^ ^ ^ ^ | ^ ^ _[_ ^ ^  | ^ ^ _{_ ^ ^   | _t_ag/_T_odo  | n_o_te      lat_e_x     | _s_ubtree | _c_apture  _R_eorganize
 _h_ ^+^ _l_  ^ ^ ^+^ ^ ^  | ^ ^ ^+^ ^ ^ | _b_ ^+^ _f_ | ^ ^ ^+^ ^ ^  | ^ ^ ^+^ ^ ^   | re_v_eal    | _F_ootnote  _i_mages    | _w_iden   | _a_genda   _|_ table
 ^ ^ _j_ ^ ^  ^ ^ _n_ ^ ^  | ^ ^ _d_ ^ ^ | ^ ^ ^ ^ ^ ^ | ^ ^ _]_ ^ ^  | ^ ^ _}_ ^ ^   | _r_efile    | RefTe_X_    _*_ heading | _K_ill    | _-_ code   _H_ome
               |       | c_L_ear | _U_pdate | _D_isplay | _A_rchive   | eff_O_rt    _E_xport    | cop_y_    | _C_lock    e_x_ecute
               |       |       |        | _S_tore   |           |           _>_ cal     |         | dra_W_er   _q_uit
               |       |       |        | _I_nsert  |           |           _<_ date    |         | _g_oto
"
  ("j" outline-next-visible-heading)
  ("k" outline-previous-visible-heading)
  ("l" org-down-element)
  ("h" org-up-element)
  ("g" sk/open-deft-and-start-hydra :exit t)
  ("u" org-next-item)
  ("d" org-previous-item)
  ("n" org-forward-heading-same-level)
  ("p" org-backward-heading-same-level)
  ("]" org-next-block)
  ("[" org-previous-block)
  ("U" org-update-all-dblocks :color blue)
  ("}" org-next-link)
  ("{" org-previous-link)
  ("D" org-toggle-link-display)
  ("S" org-store-link)
  ("I" org-insert-link)
  ("f" org-table-next-field)
  ("b" org-table-previous-field)
  ("L" org-table-blank-field :color blue)
  ("t" sk/hydra-org-tag-todo/body :exit t)
  ("T" sk/hydra-org-tag-todo/body :exit t)
  ("|" sk/hydra-org-tables :exit t)
  ("v" org-reveal)
  ("r" org-refile :color blue)
  ("A" org-archive-subtree-default :color blue)
  ("o" org-note)
  ("F" org-footnote)
  ("W" sk/hydra-org-drawer/body :exit t)
  ("e" org-preview-latex-fragment)
  ("i" org-display-inline-images)
  ("*" org-toggle-heading)
  ("E" org-export-dispatch)
  ("C" sk/hydra-org-clock/body :exit t)
  ("X" org-reftex-citation :color blue)
  (">" org-goto-calendar :color blue)
  ("<" org-date-from-calendar)
  ("O" org-set-effort)
  ("s" org-narrow-to-subtree)
  ("w" widen)
  ("K" org-cut-subtree)
  ("y" org-copy-subtree)
  ("c" org-capture)
  ("a" org-agenda)
  ("-" org-edit-src-code)
  ("R" sk/hydra-org-manipulate/body :exit t)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("O" sk/hydra-of-org/body :exit t)
  ("x" counsel-M-x :color blue)
  ("q" nil :color blue))

(provide 'sk-org)

;;; sk-org.el ends here
