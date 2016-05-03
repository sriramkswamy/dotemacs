;;; init.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Here be dragons

;;; Code:

;; Load path

;; Package.el
(require 'package)

;; Don't initialize it until I say so
(setq package-enable-at-startup nil)

(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

;; Garbage collector - increase threshold
(setq gc-cons-threshold (* 1024 1024 1024 1024))

;; Now initiliaze packages - 0.7 seconds
(package-initialize)

;; Defaults - 0.7 seconds
(require 'sk-defaults)

;; packages configuration - 0.8 seconds - adds 0.1 seconds
(require 'sk-package)

;; Emacs 25 has a nice improvement - 0.8 seconds
(when (>= emacs-major-version 25)
  (setq
   package-selected-packages
   '(;; Package configuration and Profiler
     exec-path-from-shell      ; initializes the correct variables
     paradox                   ; better interface to list-packages
     esup                      ; profiler
     restart-emacs             ; restart emacs from within emacs
     cl-lib                    ; emacs library
     dash                      ; emacs library
     s                         ; emacs library
     ;; Diminish stuff
     diminish                  ; diminish mode lines
     ;; which key
     which-key                 ; hint for bindings
     ;; modal editing
     modalka                   ; modal editing made easy
     hydra                     ; semi modal sticky bindings
     ;; navigation
     beacon                    ; blink cursor when moved
     undo-tree                 ; undo navigation
     avy                       ; jump anywhere on screen
     highlight-symbol          ; highlight selected symbol
     projectile                ; project navigation
     swoop                     ; swoop word under cursor
     ag                        ; silver searcher
     pt                        ; platinum searcher
     wgrep-ag                  ; silver searcher writeable grep
     wgrep-pt                  ; platinum searcher writeable grep
     dired+                    ; improve dired
     neotree                   ; nice sidebar navigation
     ggtags                    ; tag based navigation
     perspective               ; organize workspaces
     ;; editing
     expand-region             ; Better marking
     comment-dwim-2            ; comments better
     smartparens               ; Bracket based navigation and wrapping
     yasnippet                 ; snippets
     visual-regexp             ; replacing with regexp
     visual-regexp-steroids    ; use python/pcre regex syntax
     iedit                     ; Edit multiple places at once
     vlf                       ; very large file support
     ;; visual helpers
     alert                     ; notifications
     volatile-highlights       ; highlight kills and undos
     column-enforce-mode       ; highlight if 100 chars are crossed
     highlight-indentation     ; highlight indentation
     fill-column-indicator     ; margin show
     ws-butler                 ; remove trailing whitespace
     region-state              ; info about the selected region
     smart-tab                 ; know when to use tabs and spaces
     origami                   ; syntax based folding
     vimish-fold               ; visual folding
     badwolf-theme             ; decent dark theme
     color-theme-solarized     ; everybody needs solarized
     dracula-theme             ; dark theme
     gotham-theme              ; decent dark theme
     moe-theme                 ; light and dark variants
     monokai-theme             ; sublime text
     zenburn-theme             ; low contrast theme
     reykjavik-theme           ; dark solarized-ish theme
     railscasts-theme          ; nice theme
     caroline-theme            ; dark theme
     pastelmac-theme           ; light theme
     white-sand-theme          ; solarized-ish light theme
     paper-theme               ; light theme
     ample-theme               ; current theme - light and dark
     anti-zenburn-theme        ; people hate zenburn?
     spaceline                 ; better modeline configuration
     fancy-battery             ; I need fancy stuff!
     wttrin                    ; Weather
     focus                     ; Focus on a particular set of the buffer
     ;; writing
     markdown-mode             ; major mode for markdown
     pandoc-mode               ; pandoc based conversion
     auctex                    ; auctex for latex
     auctex-latexmk            ; latexmk integration for auctex
     fountain-mode             ; fountain mode for screenwriting
     olivetti                  ; distraction free writing
     writegood-mode            ; pick out weasel words
     pdf-tools                 ; better pdf navigation/helpers
     latex-preview-pane        ; preview latex mate
     ;; org
     cdlatex                   ; latex in org
     babel                     ; code in org
     interleave                ; for pdf note taking
     ob-ipython                ; ipython support
     ox-reveal                 ; reveal.js support
     ox-impress-js             ; impress.js support
     ox-rst                    ; .rst support
     ox-pandoc                 ; pandoc org support
     org-download              ; drag and drop stuff
     deft                      ; quick notes navigation
     org-ref                   ; references for org
     org-bullets               ; nice looking bullets
     org-page                  ; blog from org
     ;; version control
     magit                     ; best git interface ever
     magit-gh-pulls            ; github pulls
     diff-hl                   ; highlight VC changes
     git-timemachine           ; blow through file history
     yagist                    ; post gists
     gitconfig-mode            ; edit configs
     gitignore-mode            ; gitignore syntax
     ;; programming
     editorconfig              ; Keep those indentations regulated
     cmake-ide                 ; cmake setup configuration
     rtags	               ; tags based navigation
     anaconda-mode             ; python navigation and completion
     cython-mode               ; cython stuff
     pyenv-mode                ; python virtual environment
     py-yapf                   ; YAPF formatting
     pytest                    ; python testing
     nose                      ; nose tests for python
     ess                       ; Emacs Speaks Statistics
     matlab-mode               ; matlab coding
     web-mode                  ; nicely integrated js/css/html
     web-beautify              ; Beautify web stuff
     skewer-mode               ; javascript immediate evaluation
     impatient-mode            ; see html instantly
     js2-mode                  ; javascript mode
     tern                      ; tern for javascript
     nodejs-repl               ; node repl
     coffee-mode               ; coffeescript support
     nginx-mode                ; nginx server support
     json-mode                 ; json syntax support
     json-snatcher             ; json stuff
     scss-mode                 ; scss mode support
     emmet-mode                ; write fast html and css
     flycheck                  ; error checking
     company                   ; autocomplete stuff
     company-quickhelp         ; show docs during company completions
     company-c-headers         ; give me c headers
     company-web               ; company mode for web
     company-jedi              ; company support for python
     company-auctex            ; latex completion for company
     company-tern              ; tern js completion
     prodigy                   ; start services
     realgud                   ; debugger for the win
     arduino-mode              ; arduino syntax support
     yaml-mode                 ; yaml syntax support
     ;; repl
     multi-term                ; multi term support in emacs
     emamux                    ; support for sending stuff to tmux
     ;; ivy et all
     ivy                       ; the core
     counsel                   ; the wrapper functions based on ivy
     swiper                    ; search based on ivy
     counsel-projectile        ; projectile support
     spotlight                 ; search mac spotlight
     ivy-bibtex                ; bibtex entries
     ;; Fun stuff
     jabber                    ; IM in Emacs
     xkcd                      ; read xkcd
     rainbow-delimiters        ; color brackets stuff
     circe                     ; IRC support
     ledger-mode               ; accounting support
     sx                        ; stackexchange
     fireplace                 ; Emacs is a fireplace now
     elfeed                    ; RSS reader in Emacs
     typit                     ; Typing games
     google-this               ; google stuff
     )
   ))

;; Diminish minor modes - 0.8 seconds
(require 'sk-diminish)

;; Discover bindings - 0.9 seconds - adds 0.1 seconds
(require 'sk-which-key)

;; Modal editing - 0.9 seconds
(require 'sk-modal)

;; Navigation using search/completion/motion - 1.1 seconds - adds 0.2 seconds
(require 'sk-navigation)

;; Editing - 1.3 seconds - adds 0.2 seconds
(require 'sk-editing)

;; Visual helpers - 1.5 seconds - adds 0.2 seconds
(require 'sk-visual)

;; Writing - 1.5 seconds
(require 'sk-writing)

;; Org - you beauty - 1.6 seconds - adds 0.1 seconds
(require 'sk-org)

;; Version control - 1.7 seconds - adds 0.1 seconds
(require 'sk-versions)

;; Programming - 2.0 seconds - adds 0.3 seconds (don't know why yet)
(require 'sk-programming)

;; ivy - 2.1 seconds - adds 0.1 seconds
(require 'sk-ivy)

;; Fun stuff! - 2.1 seconds
(require 'sk-fun)

;; Garbage collector - decrease threshold
(setq gc-cons-threshold (* 1024 1024 1024))
;;; init.el ends here
