;; language client registration for clang
(defun sk/lsp-register-clang ()
  "Registers language client for clang"
  (interactive)
  (require 'lsp-mode)
  (require 'lsp-ui)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "clangd")
					:major-modes '(c++-mode)
					:server-id 'clangd))
  (lsp-mode)
  (lsp-ui-mode)
  (lsp))

;; language client registration for python
(defun sk/lsp-register-python ()
  "Registers language client for python"
  (interactive)
  (require 'lsp-mode)
  (require 'lsp-ui)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection
  									 (concat (getenv "HOME") "/.local/bin/pyls"))
  					:major-modes '(python-mode)
  					:server-id 'pyls))
  (lsp-mode)
  (lsp-ui-mode)
  (lsp))

;; Microsoft Language Server Protocol (LSP)
(use-package lsp-mode
  :ensure t
  :hook (c++-mode python-mode)
  :hook ((python-mode . sk/lsp-register-python)
		 (c++-mode . sk/lsp-register-clang))
  :commands
  (lsp-register-client
   make-lsp-client
   lsp-stdio-connection
   lsp-workspace-folders-add
   lsp-format-buffer
   lsp-execute-command
   lsp-goto-type-definition
   lsp-execute-code-action
   lsp-find-declaration
   lsp-find-locations
   lsp-hover
   lsp-goto-definition
   lsp-describe-session
   lsp-rename
   lsp-find-definition
   lsp-find-implementation
   lsp-restart-workspace
   lsp-find-references
   lsp-find-session-folder
   lsp-find-type-definition
   lsp-workspace-folders-remove
   lsp-workspace-folders-switch
   lsp-find-workspace))

;; LSP user interface
(use-package lsp-ui
  :ensure t
  :hook ((lsp-mode . lsp-ui-mode))
  :commands
  (lsp-ui-doc
   lsp-ui-peek-find-workspace-symbol
   lsp-ui-peek-find-definitions
   lsp-ui-peek-find-references
   lsp-ui-peek-find-implementation
   lsp-ui-imenu)
  :config
  (require 'lsp-mode)
  (require 'lsp-ui)
  
  ;; mapping with leader and a key
  (ryo-modal-key "SPC i"
				 '(("a" lsp-workspace-folders-add :name "add workspace folder" :norepeat t)
				   ("b" lsp-format-buffer :name "buffer format" :norepeat t)
				   ("c" lsp-execute-command :name "command" :norepeat t)
				   ("d" lsp-goto-type-definition :name "goto type def" :norepeat t)
				   ("e" lsp-execute-code-action :name "execute action" :norepeat t)
				   ("f" lsp-find-declaration :name "find declaration" :norepeat t)
				   ("g" lsp-find-locations :name "goto locations" :norepeat t)
				   ("h" lsp-hover :name "hover info" :norepeat t)
				   ("i" lsp-ui-peek-find-definitions :name "peek defs" :norepeat t)
				   ("j" lsp-goto-definition :name "jump to def" :norepeat t)
				   ("k" lsp-ui-peek-find-workspace-symbol :name "workspace symbol" :norepeat t)
				   ("l" lsp-ui-peek-find-references :name "peek references" :norepeat t)
				   ("m" lsp-describe-session :name "describe mode" :norepeat t)
				   ("n" lsp-rename :name "rename" :norepeat t)
				   ("o" lsp-find-definition :name "open def" :norepeat t)
				   ("p" lsp-find-implementation :name "implementation" :norepeat t)
				   ("q" lsp-restart-workspace :name "restart" :norepeat t)
				   ("r" lsp-find-references :name "any references" :norepeat t)
				   ("s" lsp-find-session-folder :name "session folder" :norepeat t)
				   ("t" lsp-find-type-definition :name "type def" :norepeat t)
				   ("u" lsp-workspace-folders-remove :name "undo workspace folder" :norepeat t)
				   ("v" lsp-workspace-folders-switch :name "switch workspace folder" :norepeat t)
				   ("w" lsp-find-workspace :name "references" :norepeat t)
				   ("x" lsp-ui-peek-find-implementation :name "peek implementation" :norepeat t)
				   ("y" lsp-ui-doc :name "documentation" :norepeat t)
				   ("z" lsp-ui-imenu :name "outline" :norepeat t))))

;; LSP debugging support
(use-package dap-mode
  :ensure t
  :after (lsp-mode)
  :hook ((lsp-mode . dap-mode)
		 (dap-mode . dap-ui-mode))
  :commands
  (dap-breakpoint-add
   dap-breakpoint-toggle
   dap-breakpoint-condition
   dap-breakpoint-delete
   dap-eval-thing-at-point
   dap-continue
   dap-go-to-output-buffer
   dap-breakpoint-hit-condition
   dap-ui-inspect-thing-at-point
   dap-step-in
   dap-step-out
   dap-breakpoint-log-message
   dap-ui-breakpoints
   dap-next
   dap-debug
   dap-switch-stack-frame
   dap-restart-frame
   dap-ui-repl
   dap-switch-session
   dap-switch-thread
   dap-stop-thread
   dap-ui-locals
   dap-debug-edit-template
   dap-disconnect
   dap-ui-inspect
   dap-eval)
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  
  ;; mapping with leader and a key
  (ryo-modal-key "SPC o"
				 '(("a" dap-breakpoint-add :name "add breakpoint" :norepeat t)
				   ("b" dap-breakpoint-toggle :name "breakpoint toggle" :norepeat t)
				   ("c" dap-breakpoint-condition :name "conditional breakpoint" :norepeat t)
				   ("d" dap-breakpoint-delete :name "delete breakpoint" :norepeat t)
				   ("e" dap-eval-thing-at-point :name "eval thing" :norepeat t)
				   ("f" dap-continue :name "continue" :norepeat t)
				   ("g" dap-go-to-output-buffer :name "goto output" :norepeat t)
				   ("h" dap-breakpoint-hit-condition :name "hit conditional" :norepeat t)
				   ("i" dap-ui-inspect-thing-at-point :name "inspect thing" :norepeat t)
				   ("j" dap-step-in :name "jump in" :norepeat t)
				   ("k" dap-step-out :name "get out" :norepeat t)
				   ("l" dap-breakpoint-log-message :name "log breakpoint" :norepeat t)
				   ("m" dap-ui-breakpoints :name "breakpoints" :norepeat t)
				   ("n" dap-next :name "next" :norepeat t)
				   ("o" dap-debug :name "debug on" :norepeat t)
				   ("p" dap-switch-stack-frame :name "switch stack frame" :norepeat t)
				   ("q" dap-restart-frame :name "restart" :norepeat t)
				   ("r" dap-ui-repl :name "repl" :norepeat t)
				   ("s" dap-switch-session :name "switch sessions" :norepeat t)
				   ("t" dap-switch-thread :name "thread" :norepeat t)
				   ("u" dap-stop-thread :name "stop thread" :norepeat t)
				   ("v" dap-ui-locals :name "variables" :norepeat t)
				   ("w" dap-debug-edit-template :name "work with template" :norepeat t)
				   ("x" dap-disconnect :name "disconnect" :norepeat t)
				   ("y" dap-ui-inspect :name "inspect" :norepeat t)
				   ("z" dap-eval :name "eval" :norepeat t))))

;; LSP for Java
(use-package lsp-java
  :ensure t
  :after (lsp-mode)
  :hook ((java-mode . lsp-mode)
		 (java-mode . lsp-ui-mode)
		 (java-mode . lsp))
  :config
  (require 'lsp-java))

;; LSP completion support
(use-package company-lsp
  :ensure t
  :after (company)
  :bind* (("C-j l"	. company-lsp))
  :config

  ;; python
  (defun sk/company-python ()
	"Add backends for python completion in company mode"
	(interactive)
	(require 'company)
	(setq company-backends
		  '((;; list of backends
			 company-files          ; files & directory
			 company-yasnippet      ; snippets
			 company-keywords       ; keywords
			 company-dabbrev-code   ; code words
			 company-lsp            ; python lsp completion
			 company-anaconda       ; python anaconda completion
			 company-capf))))

  ;; clang
  (defun sk/company-clang ()
	"Add backends for clang completion in company mode"
	(interactive)
	(require 'company)
	(setq company-backends
		  '((;; list of backends
			 company-files          ; files & directory
			 company-yasnippet      ; snippets
			 company-keywords       ; keywords
			 company-dabbrev-code   ; code words
			 company-lsp            ; clangd lsp completion
			 company-rtags          ; clang rtags completion
			 ;; company-irony          ; clang irony completion
			 company-capf)))))

;; provide the configuration
(provide 'sk-lsp)
