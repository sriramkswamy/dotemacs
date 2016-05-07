;;; sk-cpp-hydra.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Hydras for C++

;;; Code:

;; Code jump hydra
(defhydra sk/hydra-for-cpp (:color blue
			    :hint nil)
  "
 ^Send^                                                    ^Navigate^
^^^^^^^^^^----------------------------------------------------------------------------------------------------------------------------------
 _B_: build            _o_: omp math      _O_: omp simple      _s_: sym-at-pt   _r_: ref-at-pt    _D_: diag    _f_: for-stack    _n_: next    _i_: info
 _c_: compile math     _m_: mpi math      _M_: mpi simple      _S_: symbol      _R_: references   _F_: fixit   _b_: back-stack   _p_: prev    _t_: type
 _C_: compile simple   _h_: hybrid math   _H_: hybrid simple   _v_: vir-at-pt   _N_: rename       _P_: preproc _d_: depends      _e_: enum    _q_: quit
"
  ("B" compile)
  ("o" sk/compile-cpp-omp-math)
  ("O" sk/compile-cpp-omp-simple)
  ("m" sk/compile-cpp-mpi-math)
  ("M" sk/compile-cpp-mpi-simple)
  ("h" sk/compile-cpp-hybrid-math)
  ("H" sk/compile-cpp-hybrid-simple)
  ("c" sk/compile-cpp-math)
  ("C" sk/compile-cpp-simple)
  ("s" rtags-find-symbol-at-point :color red)
  ("S" rtags-find-symbol :color red)
  ("r" rtags-find-references-at-point :color red)
  ("R" rtags-find-references :color red)
  ("v" rtags-find-virtuals-at-point :color red)
  ("D" rtags-diagnostics :color red)
  ("F" rtags-fixit :color red)
  ("P" rtags-preprocess-file :color red)
  ("f" rtags-location-stack-forward :color red)
  ("b" rtags-location-stack-back :color red)
  ("n" rtags-next-match :color red)
  ("p" rtags-previous-match :color red)
  ("d" rtags-print-dependencies :color red)
  ("i" rtags-print-symbol-info :color red)
  ("t" rtags-symbol-type :color red)
  ("e" rtags-print-enum-value-at-point :color red)
  ("N" rtags-rename-symbol :color red)
  ("q" nil :color blue))

;; Binding
(global-set-key (kbd "C-c h c c") 'sk/hydra-for-cpp/body)

;; Modal binding
(modalka-define-kbd "c c" "C-c h c c")

;; Which key explanation
(which-key-add-key-based-replacements
  "c c" "c/cpp code")

(provide 'sk-cpp-hydra)
;;; sk-cpp-hydra.el ends here
