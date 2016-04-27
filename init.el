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

;; Now initiliaze packages
(package-initialize)

;; Defaults
(require 'sk-defaults)

;; packages configuration
(require 'sk-package)

;; Emacs 25 has a nice improvement
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
     dired+                    ; improve dired
     neotree                   ; nice sidebar navigation
     ggtags                    ; tag based navigation
     back-button               ; global and local marks
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
     org-jekyll                ; blog from org
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
     company-jedi              ; company mode for python
     company-anaconda          ; company support for anaconda mode
     company-auctex            ; latex completion for company
     company-tern              ; tern js completion
     prodigy                   ; start services
     realgud                   ; debugger for the win
     arduino-mode              ; arduino syntax support
     yaml-mode                 ; yaml syntax support
     ;; repl
     multi-term                ; multi term support in emacs
     emamux                    ; support for sending stuff to tmux
     ;; helm
     helm                      ; the core
     flx                       ; flx matching algorithm
     helm-flx                  ; flx integration with helm
     helm-fuzzier              ; fuzzier matching
     helm-ag                   ; helm interface to ag
     helm-descbinds            ; helm to describe bindings
     helm-themes               ; search and apply themes
     helm-swoop                ; swoop word under cursor
     helm-projectile           ; interface for projectile
     helm-bibtex               ; bibtex entries
     helm-flycheck             ; flycheck
     helm-css-scss             ; helm for css selectors
     ;; Fun stuff
     jabber                    ; IM in Emacs
     xkcd                      ; read xkcd
     rainbow-delimiters        ; color brackets stuff
     circe                     ; IRC support
     ledger-mode               ; accounting support
     sx                        ; stackexchange
     fireplace                 ; Emacs is a fireplace now
     elfeed                    ; RSS reader in Emacs
     )
   ))

;; Diminish minor modes
(require 'sk-diminish)

;; Discover bindings
(require 'sk-which-key)

;; Modal editing
(require 'sk-modal)

;; Navigation using search/completion/motion
(require 'sk-navigation)

;; Editing
(require 'sk-editing)

;; Visual helpers
(require 'sk-visual)

;; Writing
(require 'sk-writing)

;; Org - you beauty
(require 'sk-org)

;; Version control
(require 'sk-versions)

;; Programming
(require 'sk-programming)

;; Helm
(require 'sk-helm)

;; Fun stuff!
(require 'sk-fun)

;; Garbage collector - decrease threshold
(setq gc-cons-threshold (* 1024 1024 1024))
;;; init.el ends here
