;;; sk-web-hydra.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Hydras for web mode and javascript

;;; Code:

;; javascript hydra
(defhydra sk/hydra-for-js (:color blue
                           :hint nil)
  "
 ^Node^                                ^Tern^                                  ^Json^
^^^^^^^^^^-----------------------------------------------------------------------------------------
 _r_: region    _s_: start    _l_: load    _d_: definition    _h_: highlight refs    _j_: path
 _b_: buffer    _S_: switch              _n_: def by name   _u_: use-server        _q_: quit
 _x_: sexp      _e_: exec                _t_: type          _D_: doc
"
  ("r" nodejs-repl-send-region)
  ("b" nodejs-repl-send-buffer)
  ("x" nodejs-repl-send-last-sexp)
  ("s" nodejs-repl)
  ("S" nodejs-repl-switch-to-repl)
  ("e" nodejs-repl-execute)
  ("l" nodejs-repl-load-file :color red)
  ("d" tern-find-definition :color red)
  ("n" tern-find-definition-by-name :color red)
  ("t" tern-get-type :color red)
  ("D" tern-get-docs :color red)
  ("u" tern-use-server :color red)
  ("h" tern-highlight-refs)
  ("R" tern-rename-variable)
  ("j" jsons-print-path)
  ("q" nil :color blue))

;; Binding
(global-set-key (kbd "C-c h c j") 'sk/hydra-for-js/body)

;; Modal binding
(modalka-define-kbd "c j" "C-c h c j")

;; javascript hydra
(defhydra sk/hydra-for-web (:color red
                            :hint nil)
  "
 ^Beautify^                                 ^Server^
^^^^^^^^^^-------------------------------------------------------------------
 _h_: html        _c_: css       _j_: js        _s_: httpd start   _i_: html real-time
 _H_: html buf    _C_: css buf   _J_: js buf    _S_: httpd stop    _q_: quit
"
  ("h" web-beautify-html)
  ("H" web-beautify-html-buffer)
  ("c" web-beautify-css)
  ("C" web-beautify-css-buffer)
  ("j" web-beautify-js)
  ("J" web-beautify-js-buffer)
  ("s" httpd-start :color blue)
  ("S" httpd-stop :color blue)
  ("i" impatient-mode :color blue)
  ("q" nil :color blue))

;; Binding
(global-set-key (kbd "C-c h c w") 'sk/hydra-for-web/body)

;; Modal binding
(modalka-define-kbd "c w" "C-c h c w")

;; Which key explanation
(which-key-add-key-based-replacements
  "c j" "javscript code"
  "c w" "web code")

(provide 'sk-web-hydra)
;;; sk-web-hydra.el ends here
