;;; sk-cpp-hydra.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Hydras for C++

;;; Code:

;; Make hydra
(defhydra sk/hydra-for-cpp (:color blue
			    :hint nil)
  "
 _b_: build            _o_: omp math      _O_: omp simple      _q_: quit
 _c_: compile math     _m_: mpi math      _M_: mpi simple
 _C_: compile simple   _h_: hybrid math   _H_: hybrid simple
"
  ("b" compile)
  ("o" sk/compile-cpp-omp-math)
  ("O" sk/compile-cpp-omp-simple)
  ("m" sk/compile-cpp-mpi-math)
  ("M" sk/compile-cpp-mpi-simple)
  ("h" sk/compile-cpp-hybrid-math)
  ("H" sk/compile-cpp-hybrid-simple)
  ("c" sk/compile-cpp-math)
  ("C" sk/compile-cpp-simple)
  ("q" nil :color blue))

;; Code jump hydra
(defhydra sk/hydra-for-cpp-jump (:color red
			         :hint nil)
  "
 _s_: sym-at-pt   _r_: ref-at-pt    _D_: diag    _f_: for-stack    _n_: next    _i_: info
 _S_: symbol      _R_: references   _F_: fixit   _b_: back-stack   _p_: prev    _t_: type
 _v_: vir-at-pt   _N_: rename       _P_: preproc _d_: depends      _e_: enum    _q_: quit
"
  ("s" rtags-find-symbol-at-point)
  ("S" rtags-find-symbol)
  ("r" rtags-find-references-at-point)
  ("R" rtags-find-references)
  ("v" rtags-find-virtuals-at-point)
  ("D" rtags-diagnostics)
  ("F" rtags-fixit)
  ("P" rtags-preprocess-file)
  ("f" rtags-location-stack-forward)
  ("b" rtags-location-stack-back)
  ("n" rtags-next-match)
  ("p" rtags-previous-match)
  ("d" rtags-print-dependencies)
  ("i" rtags-print-symbol-info)
  ("t" rtags-symbol-type)
  ("e" rtags-print-enum-value-at-point)
  ("N" rtags-rename-symbol)
  ("q" nil :color blue))

;; aux requirements
(require 'sk-cpp-hydra-bindings)

(provide 'sk-cpp-hydra)
;;; sk-cpp-hydra.el ends here
