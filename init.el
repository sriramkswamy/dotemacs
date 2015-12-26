;; Garbage collector - increase threshold
(setq gc-cons-threshold 100000000)

;; Load path
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

;; Packages config
(require 'sk-packages)

;; Hydra
(sk/require-package 'hydra)

;; Diminish
(sk/require-package 'diminish)

;; ;; Evil config
;; (require 'sk-evil)

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
(require 'sk-company)

;; Snippets
(require 'sk-snippets)

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

;; MATLAB
(require 'sk-matlab)

;; Java
(require 'sk-java)

;; Org
(require 'sk-org)

;; Helpers
(require 'sk-helpers)

;; Hydras
(require 'sk-hydras)

;; Jabber
(require 'sk-jabber)

;; Interface
(require 'sk-interface)

;; ;; Evil maps for packages
;; (require 'sk-evilmaps)

;; Themes
(load-theme 'leuven t)

;; Garbage collector - decrease threshold by an order
(setq gc-cons-threshold 10000000)

;; Start server - not very useful since I start emacs-mac these days
;; (server-start)
;;; .emacs ends here
