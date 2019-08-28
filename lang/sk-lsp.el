;; language client registration for typescript
(defun sk/lsp-register-typescript ()
  "Registers language client for typescript"
  (interactive)
  (require 'lsp-mode)
  (require 'lsp-ui)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "typescript-language-server --stdio")
					:major-modes '(js-mode)
					:server-id 'typescript))
  (lsp-mode)
  (lsp-ui-mode)
  (lsp)
  (dap-mode 1)
  (dap-ui-mode 1))

;; language client registration for javascript
(defun sk/lsp-register-javascript ()
  "Registers language client for javascript"
  (interactive)
  (require 'lsp-mode)
  (require 'lsp-ui)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "typescript-language-server --stdio")
					:major-modes '(js-mode)
					:server-id 'javascript))
  (lsp-mode)
  (lsp-ui-mode)
  (lsp)
  (dap-mode 1)
  (dap-ui-mode 1))

;; language client registration for golang
(defun sk/lsp-register-gopls ()
  "Registers language client for gopls"
  (interactive)
  (require 'lsp-mode)
  (require 'lsp-ui)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection
									 (concat (getenv "HOME")
											 "/go/bin/"
											 "gopls"))
					:major-modes '(go-mode)
					:server-id 'golang))
  (lsp-mode)
  (lsp-ui-mode)
  (lsp)
  (dap-mode 1)
  (dap-ui-mode 1))

;; language client registration for golang
(defun sk/lsp-register-golsp ()
  "Registers language client for golsp"
  (interactive)
  (require 'lsp-mode)
  (require 'lsp-ui)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection
									 (concat (getenv "HOME")
											 "/go/bin/"
											 "go-langserver"))
					:major-modes '(go-mode)
					:server-id 'golang))
  (lsp-mode)
  (lsp-ui-mode)
  (lsp)
  (dap-mode 1)
  (dap-ui-mode 1))

;; language client registration for clang
(defun sk/lsp-register-clangd ()
  "Registers language client for clangd"
  (interactive)
  (require 'lsp-mode)
  (require 'lsp-ui)
  (require 'dap-lldb)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "clangd-9")
					:major-modes '(c++-mode)
					:server-id 'clangd))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "clangd-9")
					:major-modes '(c-mode)
					:server-id 'clangd))
  (lsp-mode)
  (lsp-ui-mode)
  (lsp)
  (dap-mode 1)
  (dap-ui-mode 1))

;; language client registration for cquery
(defun sk/lsp-register-cquery ()
  "Registers language client for cquery"
  (interactive)
  (require 'lsp-mode)
  (require 'lsp-ui)
  (require 'dap-lldb)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection
									 (concat (getenv "HOME")
											 "/sources/cquery/build/cquery"
											 "--log-all-to-stderr"))
					:major-modes '(c++-mode)
					:server-id 'cquery))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection
									 (concat (getenv "HOME")
											 "/sources/cquery/build/cquery"
											 "--log-all-to-stderr"))
					:major-modes '(c-mode)
					:server-id 'cquery))
  (lsp-mode)
  (lsp-ui-mode)
  (lsp)
  (dap-mode 1)
  (dap-ui-mode 1))

;; language client registration for system python2
(defun sk/lsp-register-python2-system ()
  "Registers language client for system python2"
  (interactive)
  (require 'lsp-mode)
  (require 'lsp-ui)
  (require 'dap-python)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection
  									 (concat (getenv "HOME") "/.local/bin/pyls"))
  					:major-modes '(python-mode)
  					:server-id 'pyls))
  (flycheck-set-checker-executable "python-flake8"
								   (concat (getenv "HOME")
										   "/.local/bin/flake8"))
  (flycheck-set-checker-executable "python-pylint"
								   (concat (getenv "HOME")
										   "/.local/bin/pylint"))
  (setq python-shell-interpreter
		(concat (getenv "HOME") "/.local/bin/ipython2")
  		python-shell-interpreter-args "--simple-prompt -i")

  (lsp-mode)
  (lsp-ui-mode)
  (lsp)
  (dap-mode 1)
  (dap-ui-mode 1))

;; language client registration for system python3
(defun sk/lsp-register-python3-system ()
  "Registers language client for system python3"
  (interactive)
  (require 'lsp-mode)
  (require 'lsp-ui)
  (require 'dap-python)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection
  									 (concat (getenv "HOME") "/.local/bin/pyls"))
  					:major-modes '(python-mode)
  					:server-id 'pyls))
  (flycheck-set-checker-executable "python-flake8"
								   (concat (getenv "HOME")
										   "/.virtualenvs/global/bin/flake8"))
  (flycheck-set-checker-executable "python-pylint"
								   (concat (getenv "HOME")
										   "/.virtualenvs/global/bin/pylint"))
  (setq python-shell-interpreter
		(concat (getenv "HOME") "/.virtualenvs/global/bin/ipython")
  		python-shell-interpreter-args "--simple-prompt -i") (lsp-mode)

  (lsp-ui-mode)
  (lsp)
  (dap-mode 1)
  (dap-ui-mode 1))

;; language client registration for global python3 virtualenv
(defun sk/lsp-register-python-global-venv ()
  "Registers language client for global python virtualenv"
  (interactive)
  (require 'lsp-mode)
  (require 'lsp-ui)
  (require 'dap-python)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection
  									 (concat (getenv "HOME")
											 "/.virtualenvs/global/bin/pyls"))
  					:major-modes '(python-mode)
  					:server-id 'pyls))
  (flycheck-set-checker-executable "python-flake8"
								   (concat (getenv "HOME")
										   "/.virtualenvs/global/bin/flake8"))
  (flycheck-set-checker-executable "python-pylint"
								   (concat (getenv "HOME")
										   "/.virtualenvs/global/bin/pylint"))
  (setq python-shell-interpreter
		(concat (getenv "HOME") "/.virtualenvs/global/bin/ipython")
  		python-shell-interpreter-args "--simple-prompt -i")

  (lsp-mode)
  (lsp-ui-mode)
  (lsp)
  (dap-mode 1)
  (dap-ui-mode 1))

(defun sk/lsp-clear-blacklist ()
  "clears the blacklist in LSP"
  (interactive)
  (setf (lsp-session-folders-blacklist (lsp-session)) nil)
  (lsp--persist-session (lsp-session)))

;; Microsoft Language Server Protocol (LSP)
(use-package lsp-mode
  :ensure t
  :hook (c++-mode python-mode)
  :hook ((python-mode . sk/lsp-register-python3-system)
		 (c++-mode . sk/lsp-register-clangd))

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
   lsp-find-workspace)

  :init
  (setq lsp-clients-clangd-executable "clangd-9")

  :config
  (ryo-modal-major-mode-keys
   'go-mode
   ("m k g" sk/lsp-register-gopls :name "lsp gopls")
   ("m k l" sk/lsp-register-golsp :name "lsp golsp"))
   (ryo-modal-major-mode-keys
   'js-mode
   ("m k t" sk/lsp-register-typescript :name "lsp ts")
   ("m k j" sk/lsp-register-javascript :name "lsp js"))
  (ryo-modal-major-mode-keys
   'c++-mode
   ("m k d" sk/lsp-register-clangd :name "lsp clang")
   ("m k q" sk/lsp-register-cquery :name "lsp cquery"))
  (ryo-modal-major-mode-keys
   'c-mode
   ("m k d" sk/lsp-register-clangd :name "lsp clang")
   ("m k q" sk/lsp-register-cquery :name "lsp cquery"))
  (ryo-modal-major-mode-keys
   'python-mode
   ("m k 2" sk/lsp-register-python2-system :name "lsp python2")
   ("m k 3" sk/lsp-register-python3-system :name "lsp python3")
   ("m k g" sk/lsp-register-python-global-venv :name "lsp global venv")))

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
  (require 'lsp-ui))

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
   dap-eval
   dap-hydra)
  :config
  (dap-mode 1)
  (dap-ui-mode 1))

;; LSP for Java
(use-package lsp-java
  :ensure t
  :after (lsp-mode)
  :hook ((java-mode . lsp-mode)
		 (java-mode . lsp-ui-mode)
		 (java-mode . lsp))
  :config
  (require 'lsp-java))

;; provide the configuration
(provide 'sk-lsp)
