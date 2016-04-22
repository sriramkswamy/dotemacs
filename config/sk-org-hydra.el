;;; sk-org-hydra.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Org mode hydras

;;; Code:

;; Org organize hydra
(defhydra sk/hydra-org-organize (:color red
                                 :hint nil)
  "
 ^Move^                                      ^Item^
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

;; Org tags hydra
(defhydra sk/hydra-org-tag (:color red
                            :hint nil)
  "
 _t_: tags    _v_: view    _m_: match   _s_: sparse-tree    _q_: quit
"
  ("t" org-set-tags-command :color blue)
  ("v" org-tags-view)
  ("m" org-match-sparse-tree :color blue)
  ("s" org-sparse-tree :color blue)
  ("q" nil :color blue))

;; Org todo hydra
(defhydra sk/hydra-org-todo (:color red
                             :hint nil)
  "
 _t_: todo     _d_: deadline    _o_: over    _s_: schedule   _c_: check   _q_: quit
"
  ("t" org-todo :color blue)
  ("d" org-deadline :color blue)
  ("o" org-deadline-close :color blue)
  ("s" org-schedule :color blue)
  ("c" org-check-deadlines)
  ("q" nil :color blue))

;; Org checkbox hydra
(defhydra sk/hydra-org-checkbox (:color red
                                 :hint nil)
  "
 _c_: checkbox   _t_: toggle   _s_: stats    _r_: reset    _c_: count    _q_: quit
"
  ("c" org-checkbox)
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
 ^Clock^                   ^Timer^    ^Stamp^
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
 ^Field^   ^Shift^   ^Insert^     ^Delete^        ^Field^    ^Table^     ^Formula^
^^^^^^^^^^^^------------------------------------------------------------------------------
 ^ ^ _k_ ^ ^   ^ ^ _K_ ^ ^   _r_: row      _dr_: del row    _e_: edit   _a_: align   _+_: sum    _q_: quit
 _h_ ^+^ _l_   _H_ ^+^ _L_   _c_: column   _dc_: del col    _b_: blank  _|_: create  _=_: eval
 ^ ^ _j_ ^ ^   ^ ^ _J_ ^ ^   _-_: hline                  _i_: info             _f_: edit
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

;; bindings
(require 'sk-org-hydra-bindings)

(provide 'sk-org-hydra)
;;; sk-org-hydra.el ends here
