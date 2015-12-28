;; Irony mode for C++
(sk/require-package 'irony)
(defun sk/diminish-irony ()
  (interactive)
  (diminish 'irony-mode " Γ"))
(add-hook 'irony-mode-hook 'sk/diminish-irony)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun sk/irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook  'sk/irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; Cmake ide
(sk/require-package 'cmake-ide)

;; Company irony
(sk/require-package 'company-irony)
(sk/require-package 'company-irony-c-headers)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

;; Irony for flycheck
(sk/require-package 'flycheck-irony)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; Hydra - for cpp
(defhydra sk/hydra-for-cpp (:color blue
                            :hint nil)
  "
 ^Compile^ | ^Eshell^       | ^Terminal^     | ^Menu^
 ^^^^^^^^^--------|--------------|--------------|---------------
 _m_ake    | _+_ vertical   | _|_ vertical   | _H_ome   e_x_ecute
 _c_ompile | _-_ horizontal | ___ horizontal | _L_ang   _Q_uit
 _r_un     |              |              |
"
  ("m" compile)
  ("c" multi-compile-run)
  ("r" quickrun)
  ("+" sk/eshell-vertical)
  ("-" sk/eshell-horizontal)
  ("|" sk/multi-term-vertical)
  ("_" sk/multi-term-horizontal)
  ("L" sk/hydra-of-langs/body :exit t)
  ("H" sk/hydra-of-hydras/body :exit t)
  ("x" counsel-M-x :color blue)
  ("Q" nil :color blue))

(provide 'sk-cpp)

;;; sk-cpp.el ends here
