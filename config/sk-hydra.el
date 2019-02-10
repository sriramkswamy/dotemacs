;; hydra
(use-package hydra
  :ensure t
  :defer t)

;; smartparens hydra
(defhydra sk/smartparens-hydra (:color red :hint nil)
  "
 ^Move^              ^Edit^                                              ^Splice^
^^^^^^^^^^^^^--------------------------------------------------------------------------------------------------
 ^ ^ _k_ ^ ^    ^ ^ _p_ ^ ^    _<_: barf backward    _u_: unwrap       _x_: transpose  _S_: splice   _q_: quit
 _h_ ^+^ _l_    _b_ ^+^ _f_    _>_: barf forward     _U_: unwrap back  _c_: convolute  _F_: forward
 ^ ^ _j_ ^ ^    ^ ^ _n_ ^ ^    _)_: slurp forward    _d_: delete       _r_: raise      _B_: backward
 _a_: start _e_: end   _(_: slurp backward   _y_: copy         _s_: split      _A_: around
"
  ("h" sp-backward-sexp)
  ("l" sp-forward-sexp)
  ("j" sp-next-sexp)
  ("k" sp-previous-sexp)
  ("p" sp-backward-down-sexp)
  ("n" sp-up-sexp)
  ("f" sp-down-sexp)
  ("b" sp-backward-up-sexp)
  ("a" sp-beginning-of-sexp)
  ("e" sp-end-of-sexp)
  ("<" sp-backward-barf-sexp)
  (">" sp-forward-barf-sexp)
  ("(" sp-backward-slurp-sexp)
  (")" sp-forward-slurp-sexp)
  ("u" sp-unwrap-sexp)
  ("U" sp-backward-unwrap-sexp)
  ("d" sp-kill-sexp)
  ("y" sp-copy-sexp)
  ("x" sp-transpose-sexp)
  ("c" sp-convolute-sexp)
  ("r" sp-raise-sexp)
  ("s" sp-split-sexp)
  ("S" sp-splice-sexp)
  ("F" sp-splice-sexp-killing-forward)
  ("B" sp-splice-sexp-killing-backward)
  ("A" sp-splice-sexp-killing-around)
  ("q" nil :color blue))

;; hydra for debugging
(defhydra sk/debug-hydra (:pre (sk/gud-mode) :color red :hint nil)
  "
 ^Debuggers^   ^Breakpoints^    ^Navigation^             ^Quit^
^^^^^^^^^^^^^--------------------------------------------------------------------------
 _g_: gdb      _b_: toggle      _j_: next  _i_: step in    _k_: kill debug
 _p_: pdb      _r_: remove      _s_: step  _o_: step out   _q_: quit hydra
                            _c_: cont  _f_: finish
"
  ;; debuggers
  ("g" realgud:gdb)
  ("p" realgud:pdb)
  ;; breakpoints
  ("b" sk/gud-break)
  ("r" sk/gud-remove)
  ;; navigation
  ("j" sk/gud-next)
  ("s" sk/gud-step)
  ("i" sk/gud-down)
  ("o" sk/gud-up)
  ("c" sk/gud-cont)
  ("f" sk/gud-finish)
  ;; quit
  ("k" gdb-exit :color blue)
  ("q" nil :color blue))

;; debug adapter protocol
(defhydra sk/dap-hydra (:color red :hint nil)
  "
^Stepping^          ^Switch^                 ^Breakpoints^           ^Eval
^^^^^^^^-----------------------------------------------------------------------------------------
_n_: Next           _ss_: Session            _bt_: Toggle            _ee_: Eval
_i_: Step in        _st_: Thread             _bd_: Delete            _er_: Eval region
_o_: Step out       _sf_: Stack frame        _ba_: Add               _es_: Eval thing at point
_c_: Continue       _sl_: List locals        _bc_: Set condition     _eii_: Inspect
_r_: Restart frame  _sb_: List breakpoints   _bh_: Set hit count     _eir_: Inspect region
_Q_: Disconnect     _sS_: List sessions      _bl_: Set log message   _eis_: Inspect thing at point
"
  ("n" dap-next)
  ("i" dap-step-in)
  ("o" dap-step-out)
  ("c" dap-continue)
  ("r" dap-restart-frame)
  ("ss" dap-switch-session)
  ("st" dap-switch-thread)
  ("sf" dap-switch-stack-frame)
  ("sl" dap-ui-locals)
  ("sb" dap-ui-breakpoints)
  ("sS" dap-ui-sessions)
  ("bt" dap-breakpoint-toggle)
  ("ba" dap-breakpoint-add)
  ("bd" dap-breakpoint-delete)
  ("bc" dap-breakpoint-condition)
  ("bh" dap-breakpoint-hit-condition)
  ("bl" dap-breakpoint-log-message)
  ("ee" dap-eval)
  ("er" dap-eval-region)
  ("es" dap-eval-thing-at-point)
  ("eii" dap-ui-inspect)
  ("eir" dap-ui-inspect-region)
  ("eis" dap-ui-inspect-thing-at-point)
  ("Q" dap-disconnect :color blue)
  ("q" nil "quit" :color blue))

;; lsp hydra
(defhydra sk/lsp-hydra (:color pink :hint nil :foreign-keys run)
  "
^Basics^             ^Find^                ^Peek^                  ^Workspace
^^^^^^^^-----------------------------------------------------------------------------------------
_h_: Highlight       _d_: Definition       _pt_: Type              _wa_: Add folder
_n_: Rename          _t_: Type             _pd_: Definition        _wr_: Remove folder
_a_: Code action     _i_: Implementation   _pi_: Implementation    _ws_: Switch folder
_f_: Format          _l_: Documentation    _ps_: Workspace sym     _wd_: Description
_R_: Restart         _e_: Declaration      _po_: Outline
_Q_: Disconnect      _r_: References       _pr_: References
"
  ("h" lsp-document-highlight :color red)
  ("n" lsp-rename :color red)
  ("a" lsp-execute-code-action :color red)
  ("f" lsp-format-buffer :color red)
  ("d" lsp-find-definition :color red)
  ("t" lsp-find-type-definition :color red)
  ("i" lsp-find-implementation :color red)
  ("l" lsp-ui-doc-show :color red)
  ("r" lsp-find-references :color red)
  ("e" lsp-find-declaration :color red)
  ("wa" lsp-workspace-folders-add :color red)
  ("wr" lsp-workspace-folders-remove :color red)
  ("ws" lsp-workspace-folders-switch :color red)
  ("wd" lsp-describe-session :color red)
  ("pt" lsp-ui-peek-find-type-definition :color red)
  ("pd" lsp-ui-peek-find-definitions :color red)
  ("pi" lsp-ui-peek-find-implementation :color red)
  ("pr" lsp-ui-peek-find-references :color red)
  ("po" lsp-ui-imenu :color red)
  ("ps" lsp-ui-peek-find-workspace-symbol :color red)
  ("R" lsp-restart-workspace :color red)
  ("Q" lsp-shutdown-workspace :color blue)
  ("q" nil "quit" :color blue))

;; provide hydra
(provide 'sk-hydra)
