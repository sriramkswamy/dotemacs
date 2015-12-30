;; Garbage collector - increase threshold
(setq gc-cons-threshold (* 1024 1024 1024 1024))

;; Load path
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

;; Packages config
(require 'sk-packages)

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
(require 'sk-company)

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

;; Interface
(require 'sk-interface)

;; ;; Evil
;; (require 'sk-evil)

;; Some maps
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-r") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)

;; Themes
(sk/require-package 'zenburn-theme)
(load-theme 'leuven t)

;; Garbage collector - increase threshold
(setq gc-cons-threshold (* 1024 1024 1024))

;;; .emacs ends here
