;; sql operator bindings
(defun sk/sql-operator ()
  "operator text object bindings for sql"
  (interactive)
  (eval `(ryo-modal-major-mode-keys
		  'sql-mode
		  ("m s" ,text-objects :then '(sql-send-region)))))

(use-package sql
  :ensure t
  :mode ("\\.sql\\'" . sql-mode)
  :hook ((sql-mode . sk/sql-operator)
		 (sql-mode . sk/company-sql))
  :commands
  (sql-ms
   sql-db2
   sql-help
   sql-mode
   sql-mysql
   sql-solid
   sql-ingres
   sql-linter
   sql-oracle
   sql-sqlite
   sql-sybase
   sql-connect
   sql-vertica
   sql-informix
   sql-postgres
   sql-interbase
   sql-magic-go
   sql-mode-menu
   sql-copy-column
   sql-send-buffer
   sql-send-region
   sql-send-string
   sql-set-product
   sql-rename-buffer
   sql-send-paragraph
   sql-save-connection
   sql-magic-semicolon
   sql-product-interactive
   sql-set-sqli-buffer
   sql-show-sqli-buffer
   sql-end-of-statement
   sql-accumulate-and-indent
   sql-highlight-ms-keywords
   sql-interactive-mode-menu
   sql-beginning-of-statement
   sql-highlight-db2-keywords
   sql-highlight-ansi-keywords
   sql-highlight-mysql-keywords
   sql-highlight-solid-keywords
   sql-highlight-ingres-keywords
   sql-highlight-oracle-keywords
   sql-highlight-sqlite-keywords
   sql-highlight-sybase-keywords
   sql-highlight-vertica-keywords
   sql-highlight-informix-keywords
   sql-highlight-postgres-keywords
   sql-highlight-interbase-keywords
   sql-set-sqli-buffer-generally
   sql-toggle-pop-to-buffer-after-send-region
   sql-list-all
   sql-list-table
   sql-highlight-linter-keywords
   sql-send-line-and-next)
  )

;; bindings
(ryo-modal-major-mode-keys
 'sql-mode
 ;; sql db
 ("m r r" sql-connect :name "connect")
 ("m r s" sql-mysql :name "mysql")
 ("m r p" sql-postgres :name "postgres")
 ("m r l" sql-sqlite :name "sqlite")

 ;; interactive
 ("m z" sql-show-sqli-buffer :name "show shell")
 ("m m" sql-send-buffer :name "run file")
 ("m c" sql-send-string :name "command")
 ("m s r" sql-send-region :name "region")
 ("m k" sql-send-paragraph :name "send para")
 ("m l" sql-send-line-and-next :name "send line")

 ;; functions
 ("m a" sql-list-all :name "list all")
 ("m t" sql-list-table :name "list table")
 ("m n" sql-rename-buffer :name "rename")
 ("m o" sql-copy-column :name "copy column")
 ("m i" sql-product-interactive :name "interactive prod")
 ("m p" sql-set-product :name "set product"))

;; which key hints
(which-key-add-major-mode-key-based-replacements 'sql-mode
  "m r" "run"

  "m s" "eval"
  "m s i" "inside"
  "m s a" "around"
  "m s g" "global")

;; bindings
(ryo-modal-major-mode-keys
 'sql-interactive-mode
 ;; sql db
 ("m r r" sql-connect :name "connect")
 ("m r s" sql-mysql :name "mysql")
 ("m r p" sql-postgres :name "postgres")
 ("m r l" sql-sqlite :name "sqlite")

 ;; interactive
 ("m z" sql-show-sqli-buffer :name "show shell")
 ("m m" sql-send-buffer :name "run file")
 ("m c" sql-send-string :name "command")
 ("m s r" sql-send-region :name "region")
 ("m k" sql-send-paragraph :name "send para")
 ("m l" sql-send-line-and-next :name "send line")

 ;; functions
 ("m a" sql-list-all :name "list all")
 ("m t" sql-list-table :name "list table")
 ("m n" sql-rename-buffer :name "rename")
 ("m o" sql-copy-column :name "copy column")
 ("m i" sql-product-interactive :name "interactive prod")
 ("m p" sql-set-product :name "set product"))

;; which key hints
(which-key-add-major-mode-key-based-replacements 'sql-interactive-mode
  "m r" "run"

  "m s" "eval"
  "m s i" "inside"
  "m s a" "around"
  "m s g" "global")

;; provide this configuration
(provide 'sk-sql)
