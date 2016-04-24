;;; sk-editing-hydra.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Hydras for editing

;;; Code:

;; Rectangle marks
(defhydra sk/hydra-rectangle (:color red
                              :hint nil)
  "
 _p_: paste   _c_: clear   _r_: replace   _q_: quit
 _y_: copy    _o_: open    _i_: insert
 _d_: kill               _n_: number
"
  ("h" backward-char nil)
  ("l" forward-char nil)
  ("k" previous-line nil)
  ("j" next-line nil)
  ("y" copy-rectangle-as-kill)
  ("d" kill-rectangle nil)
  ("c" clear-rectangle nil)
  ("o" open-rectangle nil)
  ("p" yank-rectangle)
  ("r" string-rectangle)
  ("i" string-insert-rectangle)
  ("n" rectangle-number-lines)
  ("q" nil :color blue))

;; Hydra of macros
(defhydra sk/hydra-of-macros (:color red
                              :hint nil)
  "
 _m_: macro  _l_: lossage  _v_: view      _f_: forward    _d_: delete   _q_: quit
 _p_: prev   _e_: edit     _r_: register  _b_: backward   _k_: key
  "
  ("m" kmacro-call-macro)
  ("p" kmacro-call-ring-2nd)
  ("l" kmacro-edit-lossage :color blue)
  ("e" kmacro-edit-macro :color blue)
  ("v" kmacro-view-macro :color blue)
  ("r" kmacro-to-register :color blue)
  ("f" kmacro-cycle-ring-next)
  ("b" kmacro-cycle-ring-previous)
  ("d" kmacro-delete-ring-head :color blue)
  ("k" kmacro-bind-to-key :color blue)
  ("q" nil :color blue))

;; Hydra registers
(defhydra sk/hydra-registers (:color blue
                              :hint nil)
  "
 _a_: append   _i_: insert    _j_: jump       _r_: rectangle-copy   _+_: increment   _q_: quit
 _c_: copy-to  _f_: frameset  _n_: number-to  _w_: window-config    _p_: point-to
  "
  ("a" append-to-register)
  ("c" copy-to-register)
  ("i" insert-register)
  ("f" frameset-to-register)
  ("j" jump-to-register)
  ("n" number-to-register)
  ("r" copy-rectangle-to-register)
  ("w" window-configuration-to-register)
  ("+" increment-register)
  ("p" point-to-register)
  ("q" nil :color blue))

;; bindings
(require 'sk-editing-hydra-bindings)

(provide 'sk-editing-hydra)
;;; sk-editing-hydra.el ends here
