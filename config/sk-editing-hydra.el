;;; sk-editing-hydra.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Hydras for editing

;;; Code:

;; Hydra of macros
(defhydra sk/hydra-of-macros (:color red
                              :hint nil)
  "
 ^Macro^
 ^^^^^^^^^------------------------------------------------------------
 _m_ macro  _l_ lossage  _v_ view      _f_ forward    _d_ delete   _q_ quit
 _p_ prev   _e_ edit     _r_ register  _b_ backward   _k_ key
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

;; global and modalka bindings
(require 'sk-editing-hydra-bindings)
(require 'sk-editing-hydra-modalka)

(provide 'sk-editing-hydra)
;;; sk-editing-hydra.el ends here
