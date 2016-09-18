;; dired to view directories
(use-package dired                                                             ; directory viewing and manipulating
  :general                                                                     ; map using the `general.el' package
  (general-evil-define-key '(normal visual) dired-mode-map                     ; dired mode map
    "y" (general-simulate-keys "w" t "copy file name")
    "gw" 'hydra-windows/body
    "SPC" nil
    "G" (general-simulate-keys "M->" t "end of buffer")
    "gg" (general-simulate-keys "M-<" t "beginning of buffer"))
  :init                                                                        ; settings to be loaded before package loading
  (setq dired-dwim-target t                                                    ; do what i mean
	dired-recursive-copies 'top                                            ; copy recursively
	dired-recursive-deletes 'top                                           ; delete recursively
	dired-listing-switches "-alh")                                         ; how to simulate the 'ls' command
  :config                                                                      ; settings to be loaded after package loading
  (add-hook 'dired-mode-hook 'dired-hide-details-mode))                        ; hide details

;; doc-view to view pdf files
(use-package doc-view
  :mode ("\\.pdf$" . doc-view-mode)
  :general
  (general-evil-define-key '(normal visual) doc-view-mode-map ; doc-view mode map
    "l" (general-simulate-keys "n" t "next page")
    "h" (general-simulate-keys "p" t "prev page")
    "j" (general-simulate-keys "SPC" t "scroll down")
    "k" (general-simulate-keys "DEL" t "scroll up")
    "q" (general-simulate-keys "q" t "quit")
    "/" (general-simulate-keys "C-s" t "search")
    "G" (general-simulate-keys "M->" t "end of pdf")
    "gg" (general-simulate-keys "M-<" t "beginning of pdf"))
  :init
  (setq doc-view-continuous t))		; show continuous PDFs when in DocView

;; package menu mode
(use-package package-menu
  :general
  (general-evil-define-key '(normal visual) package-menu-mode-map ; package-menu mode map
    "j" (general-simulate-keys "n" t "next package")
    "k" (general-simulate-keys "p" t "prev package")
    "u" (general-simulate-keys "U" t "update")
    "c" (general-simulate-keys "i" t "choose")
    "x" (general-simulate-keys "x" t "execute")
    "q" (general-simulate-keys "q" t "quit")
    "G" (general-simulate-keys "M->" t "end of package list")
    "gg" (general-simulate-keys "M-<" t "beginning of package list")))

;; debugger mode
(use-package debugger
  :general
  (general-evil-define-key '(normal visual) debugger-mode-map ; debugger mode map
    "q" (general-simulate-keys "q" t "quit")))

;; man mode
(general-evil-define-key '(normal visual) man-mode-map ; man mode map
    "q" (general-simulate-keys "q" t "quit"))

;; flyspell mode
(use-package flyspell
  :diminish (flyspell-mode . " φ"))

;; eww mode
(use-package eww
  :general
  (general-nmap "gx" 'eww)
  (general-nmap "g:" 'eww-browse-with-external-browser)
  (general-nmap "gX" 'eww-list-histories)
  (general-nmap "g{" 'eww-back-url)
  (general-nmap "g}" 'eww-forward-url)
  :config
  (progn
    (add-hook 'eww-mode-hook 'visual-line-mode)))

;; eshell
(use-package eshell
  :general
  (general-nvmap :prefix sk--evil-global-leader
		 "u" 'eshell)
  :init
  (setq eshell-glob-case-insensitive t
	eshell-scroll-to-bottom-on-input 'this
	eshell-buffer-shorthand t
	eshell-history-size 1024
	eshell-cmpl-ignore-case t
	eshell-prompt-function (lambda () (concat " $ "))
	eshell-aliases-file (concat user-emacs-directory ".eshell-aliases")
	eshell-last-dir-ring-size 512)
  :config
  (add-hook 'shell-mode-hook 'goto-address-mode))

;; diminish auto-revert mode
(defun sk/diminish-auto-revert ()
  (interactive)
  (diminish 'auto-revert-mode ""))
(add-hook 'auto-revert-mode-hook 'sk/diminish-auto-revert)

;; special mode
(use-package special
  :general
  (general-evil-define-key '(normal visual) special-mode-map
    "q" (general-simulate-keys "q" t "quit")))

;; help mode
(use-package help
  :general
  (general-evil-define-key '(normal visual) help-mode-map
    "q" (general-simulate-keys "q" t "quit")))

;; provide this
(provide 'sk-included)
