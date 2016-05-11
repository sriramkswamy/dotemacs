;;; sk-programming-hydra.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; For programming hydras

;;; Code:

;; Elisp eval hydra
(defhydra sk/hydra-for-elisp (:color red
	                      :hint nil)
  "
 _r_: region    _e_: expression   _s_: ielm    _q_: quit
 _f_: func      _l_: last sexp
 _b_: buffer
"
  ("r" eval-region)
  ("f" eval-defun)
  ("b" eval-buffer)
  ("e" eval-expression :color blue)
  ("l" eval-last-sexp)
  ("s" ielm :color blue)
  ("q" nil :color blue))
;; Bind and modal bind it
(global-set-key (kbd "C-c h c e") 'sk/hydra-for-elisp/body)
(modalka-define-kbd "c e" "C-c h c e")

;; Debugging code
(defhydra sk/hydra-debug (;; :pre (load-library "realgud")
			  :color blue
	                  :hint nil)
  "
 _G_: c-gdb         _P_: py-pdb        _I_: py-ipdb        _q_: quit
 _g_: realgud-gdb   _p_: realgud-pdb   _i_: realgud-ipdb
"
  ("G" gdb)
  ("g" realgud:gdb)
  ("P" pdb)
  ("p" realgud:pdb)
  ("I" ipdb)
  ("i" realgud:ipdb)
  ("q" nil :color blue))
;; Bind and modal bind it
(global-set-key (kbd "C-c h c d") 'sk/hydra-debug/body)
(modalka-define-kbd "c d" "C-c h c d")

;; Some explanations
(which-key-add-key-based-replacements
  "C-c h c" "code prefix"
  "c" "code prefix"

  "c e" "elisp code"
  "c d" "debug code")

(provide 'sk-programming-hydra)
;;; sk-programming-hydra.el ends here
