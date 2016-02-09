;; Commentary: Packages stuff. Initialize and install couple helpers
(require 'package)

;; Don't initialize it until I say so
(setq package-enable-at-startup nil)

;; packages based on versions
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t))
  (package-initialize)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;; ;; Prioritize
;; (setq package-archive-priorities
;;       '(("gnu" . 10)
;;         ("melpa" . 20)))

;; Add homebrew packages
(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; Define function for a required package
(defun sk/require-package (package)
  (setq-default highlight-tabs t)
  "Install given PACKAGE."
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

;; Get the proper path
(sk/require-package 'exec-path-from-shell)
;; (exec-path-from-shell-copy-env "PYTHONPATH")
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Paradox for package
(sk/require-package 'paradox)
(setq paradox-github-token t)

;; Garbage collector - increase threshold
(setq gc-cons-threshold (* 1024 1024 1024 1024))

;; Load path
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

;; Hydra
(sk/require-package 'hydra)

;; Diminish
(sk/require-package 'diminish)

;; Change some defaults
(require 'sk-defaults)

;; Custom functions
(require 'sk-functions)

;; Search
(require 'sk-search)

;; Motion
(require 'sk-motion)

;; Navigate
(require 'sk-navigate)

;; Editing
(require 'sk-editing)

;; REPL
(require 'sk-repl)

;; Version Control
(require 'sk-vcs)

;; Auto complete
(require 'sk-completion)

;; Snippets
(require 'sk-snippets)

;; Flycheck
(require 'sk-flycheck)

;; C/C++
(require 'sk-cpp)

;; Python
(require 'sk-python)

;; ESS
(require 'sk-ess)

;; Web
(require 'sk-web)

;; Lisp
(require 'sk-lisp)

;; ;; MATLAB
;; (require 'sk-matlab)

;; Java
(require 'sk-java)

;; Writing
(require  'sk-writing)

;; Org
(require 'sk-org)

;; Helpers
(require 'sk-helpers)

;; Hydras
(require 'sk-hydras)

;; ;; Jabber
;; (require 'sk-jabber)

;; God mode
(require 'sk-god)

;; ;; Evil
;; (require 'sk-evil)

;; Bindings
(require 'sk-bindings)

;; Garbage collector - increase threshold
(setq gc-cons-threshold (* 1024 1024 1024))

;;; .emacs ends here
