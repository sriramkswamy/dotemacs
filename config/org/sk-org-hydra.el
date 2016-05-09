;;; sk-org-hydra.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Org mode hydras

;;; Code:

;; Org organize hydra
(defhydra sk/hydra-org-organize (:color red
                                 :hint nil)
  "
 ^Move^                                       ^Item^
^^^^^^^^^^^^^--------------------------------------------------------------------
 ^ ^ _k_ ^ ^   ^ ^ _K_ ^ ^   ^ ^ _p_ ^ ^  ^ ^ _P_ ^ ^   _<_: promote  _u_: up     _q_: quit
 _h_ ^+^ _l_   _H_ ^+^ _L_   _b_ ^+^ _f_  _B_ ^+^ _F_   _>_: demote   _d_: down
 ^ ^ _j_ ^ ^   ^ ^ _J_ ^ ^   ^ ^ _n_ ^ ^  ^ ^ _N_ ^ ^
"
  ("h" org-metaleft)
  ("l" org-metaright)
  ("j" org-metadown)
  ("k" org-metaup)
  ("H" org-shiftleft)
  ("L" org-shiftright)
  ("J" org-shiftdown)
  ("K" org-shiftup)
  ("b" org-shiftmetaleft)
  ("f" org-shiftmetaright)
  ("n" org-shiftmetadown)
  ("p" org-shiftmetaup)
  ("B" org-shiftcontrolleft)
  ("F" org-shiftcontrolright)
  ("P" org-shiftcontroldown)
  ("N" org-shiftcontrolup)
  ("<" org-promote)
  (">" org-demote)
  ("d" org-move-item-down)
  ("u" org-move-item-up)
  ("q" nil :color blue))

;; Org todo hydra
(defhydra sk/hydra-org-todo (:color red
                             :hint nil)
  "
 _d_: deadline    _o_: over    _s_: schedule   _c_: check   _q_: quit
"
  ("d" org-deadline :color blue)
  ("o" org-deadline-close :color blue)
  ("s" org-schedule :color blue)
  ("c" org-check-deadlines)
  ("q" nil :color blue))

;; Org checkbox hydra
(defhydra sk/hydra-org-checkbox (:color red
                                 :hint nil)
  "
 _t_: toggle   _s_: stats    _r_: reset    _c_: count    _q_: quit
"
  ("t" org-toggle-checkbox)
  ("c" org-update-checkbox-count-maybe)
  ("r" org-reset-checkbox-state-subtree)
  ("s" org-update-statistics-cookies)
  ("q" nil :color blue))

;; Org property hydra
(defhydra sk/hydra-org-property (:color red
                                 :hint nil)
  "
 _i_: insert  _p_: property   _s_: set    _d_: delete    _t_: toggle    _q_: quit
"
  ("i" org-insert-drawer)
  ("p" org-insert-property-drawer)
  ("s" org-set-property)
  ("d" org-delete-property)
  ("t" org-toggle-ordered-property)
  ("q" nil :color blue))

;; Org clock hydra
(defhydra sk/hydra-org-clock (:color red
                              :hint nil)
  "
 ^Clock^                     ^Timer^     ^Stamp^
^^^^^^^^^^-------------------------------------------------
 _i_: in       _z_: resolve    _b_: begin  _t_: stamp       _q_: quit
 _o_: out      _l_: last       _e_: end    _u_: inactive
 _r_: report   _c_: cancel     _m_: timer
 _d_: display  _g_: goto       _s_: set
"
  ("i" org-clock-in)
  ("o" org-clock-out)
  ("r" org-clock-report)
  ("z" org-resolve-clocks)
  ("c" org-clock-cancel)
  ("d" org-clock-display)
  ("l" org-clock-in-last)
  ("g" org-clock-goto)
  ("m" org-timer)
  ("s" org-timer-set-timer)
  ("b" org-timer-start)
  ("e" org-timer-stop)
  ("t" org-time-stamp)
  ("u" org-time-stamp-inactive)
  ("q" nil :color blue))

;; Org manipulation tables
(defhydra sk/hydra-org-tables (:color red
                               :hint nil)
  "
 ^Field^   ^Shift^   ^Insert^      ^Delete^         ^Field^     ^Table^      ^Formula^
^^^^^^^^^^^^------------------------------------------------------------------------------
 ^ ^ _k_ ^ ^   ^ ^ _K_ ^ ^   _r_: row      _dr_: del row    _e_: edit   _a_: align   _+_: sum    _q_: quit
 _h_ ^+^ _l_   _H_ ^+^ _L_   _c_: column   _dc_: del col    _b_: blank  _|_: create  _=_: eval
 ^ ^ _j_ ^ ^   ^ ^ _J_ ^ ^   _-_: hline                   _i_: info             _f_: edit
"
  ("a" org-table-align)
  ("l" org-table-next-field)
  ("h" org-table-previous-field)
  ("j" org-table-end-of-field)
  ("k" org-table-beginning-of-field)
  ("r" org-table-insert-row)
  ("c" org-table-insert-column)
  ("-" org-table-insert-hline)
  ("J" org-table-move-row-down)
  ("K" org-table-move-row-up)
  ("H" org-table-move-column-left)
  ("L" org-table-move-column-right)
  ("dr" org-table-kill-row)
  ("dc" org-table-delete-column)
  ("b" org-table-blank-field)
  ("e" org-table-edit-field)
  ("i" org-table-field-info)
  ("+" org-table-sum)
  ("=" org-table-eval-formula)
  ("f" org-table-edit-formulas)
  ("|" org-table-create-or-convert-from-region)
  ("q" nil :color blue))

;; Hydra of jump
(defhydra sk/hydra-org-jump (:color pink
                             :hint nil)
  "
 ^Outline^          ^Item^   ^Table^   ^Block^   ^Link^
 ^^^^^^^^^^^-------------------------------------------------------------------------------
 ^ ^ _k_ ^ ^   ^ ^ _K_ ^ ^   ^ ^ _u_ ^ ^   ^ ^ ^ ^ ^ ^   ^ ^ _p_ ^ ^   ^ ^ _P_ ^ ^    _q_ quit
 _h_ ^+^ _l_   ^ ^ ^+^ ^ ^   ^ ^ ^+^ ^ ^   _b_ ^+^ _f_   ^ ^ ^+^ ^ ^   ^ ^ ^+^ ^ ^
 ^ ^ _j_ ^ ^   ^ ^ _J_ ^ ^   ^ ^ _d_ ^ ^   ^ ^ ^ ^ ^ ^   ^ ^ _n_ ^ ^   ^ ^ _N_ ^ ^
"
  ("j" outline-next-visible-heading)
  ("k" outline-previous-visible-heading)
  ("l" org-down-element)
  ("h" org-up-element)
  ("J" org-forward-heading-same-level)
  ("K" org-backward-heading-same-level)
  ("u" org-next-item)
  ("d" org-previous-item)
  ("f" org-table-next-field)
  ("b" org-table-previous-field)
  ("n" org-next-block)
  ("p" org-previous-block)
  ("N" org-next-link)
  ("P" org-previous-link)
  ("q" nil :color blue))

;; Org agenda view hydra
(defhydra sk/hydra-org-agenda-view (:color red
                                    :hint nil)
  "
 _d_: day        _g_: time grid    _a_: arch-trees    _L_: log closed clock
 _w_: week       _i_: inactive     _A_: arch-files    _c_: log clock check
 _t_: fortnight  _f_: follow       _r_: report        _l_: log mode toggle
 _m_: month      _e_: entry        _D_: diary         _q_: quit
 _y_: year       _!_: deadlines    _R_: reset
"
  ("R" org-agenda-reset-view)
  ("d" org-agenda-day-view)
  ("w" org-agenda-week-view)
  ("t" org-agenda-fortnight-view)
  ("m" org-agenda-month-view)
  ("y" org-agenda-year-view)
  ("l" org-agenda-log-mode)
  ("L" (org-agenda-log-mode '(4)))
  ("c" (org-agenda-log-mode 'clockcheck))
  ("f" org-agenda-follow-mode)
  ("a" org-agenda-archives-mode)
  ("A" (org-agenda-archives-mode 'files))
  ("r" org-agenda-clockreport-mode)
  ("e" org-agenda-entry-text-mode)
  ("g" org-agenda-toggle-time-grid)
  ("D" org-agenda-toggle-diary)
  ("!" org-agenda-toggle-deadlines)
  ("i"
   (let ((org-agenda-include-inactive-timestamps t))
     (org-agenda-check-type t 'timeline 'agenda)
     (org-agenda-redo)))
  ("q" nil :color blue))

;; Org ref file hydra
(defhydra sk/org-ref-bibtex-file (:color blue
                                  :hint nil)
  "
_v_: validate     _s_: sort     _r_: reformat     _c_: count     _p_: PDF      _q_: quit
  "
  ("v" bibtex-validate)
  ("s" bibtex-sort-buffer)
  ("r" bibtex-reformat)
  ("c" bibtex-count-entries)
  ("p" org-ref-build-full-bibliography)
  ("q" nil :color blue))

;; Org ref new entry
(defhydra sk/org-ref-bibtex-new-entry (:color blue
                                       :hint nil)
  "
_a_: article                 _b_: book      _p_: in proceedings   _M_: Manual      _u_: unpublished
_c_: article in collection   _i_: in book   _P_: proceedings      _t_: PhD thesis  _q_: quit
_r_: report                  _l_: booklet   _m_: Misc             _T_: MS thesis
  "
  ("a" bibtex-Article)
  ("c" bibtex-InCollection)
  ("r" bibtex-TechReport)
  ("b" bibtex-Book)
  ("i" bibtex-InBook)
  ("l" bibtex-Booklet)
  ("p" bibtex-InProceedings)
  ("P" bibtex-Proceedings)
  ("m" bibtex-Misc)
  ("M" bibtex-Manual)
  ("t" bibtex-PhdThesis)
  ("T" bibtex-MastersThesis)
  ("u" bibtex-Unpublished)
  ("q" nil :color blue))

;; Hydra of org ref
(defhydra sk/org-ref-bibtex-hydra (:color blue
                                   :hint nil)
  "
_p_: Open pdf     _y_: Copy key               _n_: New entry     _w_: WOS
_b_: Open url     _f_: Copy formatted entry   _o_: Copy entry    _c_: WOS citing
_r_: Refile entry _k_: Add keywords           _d_: delete entry  _a_: WOS related
_e_: Email entry  _K_: Edit keywords          _L_: clean entry   _P_: Pubmed
_U_: Update entry _N_: Open notes             _R_: Crossref      _g_: Google Scholar
_s_: Sort entry   _A_: Remove nonascii        _C_: Cite entry    _q_: quit
_u_: Update field _F_: file funcs
"
  ("p" org-ref-open-bibtex-pdf)
  ("b" org-ref-open-in-browser)
  ("r" (lambda () (interactive)
         (bibtex-beginning-of-entry)
         (bibtex-kill-entry)
         (find-file (ido-completing-read
                     "Bibtex file: "
                     (f-entries "." (lambda (f) (f-ext? f "bib")))))
         (goto-char (point-max))
         (bibtex-yank)
         (save-buffer)
         (kill-buffer)))
  ("e" org-ref-email-bibtex-entry)
  ("U" (doi-utils-update-bibtex-entry-from-doi (org-ref-bibtex-entry-doi)))
  ("s" org-ref-sort-bibtex-entry)
  ("u" doi-utils-update-field)
  ("y" (kill-new  (bibtex-autokey-get-field "=key=")))
  ("f" bibtex-copy-summary-as-kill)
  ("k" helm-tag-bibtex-entry)
  ("K" (lambda ()
         (interactive)
         (org-ref-set-bibtex-keywords
          (read-string "Keywords: "
                       (bibtex-autokey-get-field "keywords"))
          t)))
  ("N" org-ref-open-bibtex-notes)
  ("A" org-ref-replace-nonascii)
  ("F" sk/org-ref-bibtex-file/body)
  ("n" sk/org-ref-bibtex-new-entry/body)
  ("o" bibtex-copy-entry-as-kill)
  ("d" bibtex-kill-entry)
  ("L" org-ref-clean-bibtex-entry)
  ("R" org-ref-bibtex-crossref)
  ("w" org-ref-bibtex-wos)
  ("c" org-ref-bibtex-wos-citing)
  ("a" org-ref-bibtex-wos-related)
  ("P" org-ref-bibtex-pubmed)
  ("g" org-ref-bibtex-google-scholar)
  ("C" sk/org-ref-cite-hydra/body)
  ("q" nil :color blue))

;; Org ref hydra
(defhydra sk/hydra-org-ref (:color blue
                            :hint nil)
  "
 _e_: bib new entry     _r_: ref link     _b_: bib file options
 _t_: crossref entry    _l_: label link   _f_: file format
 _d_: doi entry         _c_: cite link    _q_: quit
  "
  ("e" sk/org-ref-bibtex-new-entry/body)
  ("t" crossref-add-bibtex-entry)
  ("d" doi-add-bibtex-entry)
  ("b" sk/org-ref-bibtex-hydra/body)
  ("r" org-ref-ivy-insert-ref-link)
  ("l" org-ref-ivy-insert-label-link)
  ("c" org-ref-ivy-insert-cite-link)
  ("f" sk/org-ref-bibtex-file/body)
  ("q" nil :color blue))

;; Org blog page hydra
(defhydra sk/hydra-org-page (:color blue
                             :hint nil)
  "
 _p_: publish    _v_: preview    _n_: new post   _r_: new repo    _i_: insert options    _q_: quit
"
  ("p" op/do-publication)
  ("v" op/do-publication-and-preview-site)
  ("n" op/new-post)
  ("r" op/new-repository)
  ("i" op/insert-options-template)
  ("q" nil :color blue))

;; bindings
(require 'sk-org-hydra-bindings)

(provide 'sk-org-hydra)
;;; sk-org-hydra.el ends here
