(use-package semantic
  :commands
  (semantic-mode
   semantic-debug
   semantic-symref
   semantic-lex-test
   semantic-add-label
   semantic-gcc-setup
   semantic-lex-debug
   semantic-show-label
   semanticdb-ref-test
   semanticdb-reset-log
   semantic-describe-tag
   semantic-symref-symbol
   semantic-force-refresh
   semantic-symref-regexp
   semantic-adebug-analyze
   semantic-describe-buffer
   semantic-decoration-mode
   semantic-speedbar-analysis
   semantic-ia-fast-jump
   semantic-narrow-to-tag
   semanticdb-save-all-db
   semantic-ia-show-summary
   semantic-ia-show-variants
   semantic-lex-spp-describe
   semantic-symref-find-text
   semantic-idle-summary-mode
   semantic-highlight-func-menu
   semantic-highlight-func-mode
   semantic-dump-parser-warnings
   semantic-highlight-edits-mode
   semanticdb-toggle-global-mode
   semanticdb-find-toggle-logging
   semantic-toggle-decoration-style
   semantic-complete-jump
   semantic-complete-done
   semantic-calculate-scope
   semantic-complete-symbol
   semantic-ia-fast-mouse-jump
   semantic-show-parser-state-mode
   semantic-analyze-proto-impl-toggle
   semantic-highlight-func-popup-menu
   semantic-show-unmatched-syntax-mode
   semantic-show-unmatched-syntax-next
   semanticdb-find-test-translate-path
   semanticdb-enable-gnu-global-databases
   semantic-sanity-check
   semantic-complete-inline-up
   semantic-complete-inline-TAB
   semantic-complete-jump-local
   semantic-clear-toplevel-cache
   semantic-complete-inline-done
   semantic-complete-inline-down
   semantic-complete-inline-exit
   semantic-complete-inline-quit
   semantic-complete-self-insert
   semantic-complete-complete-tab
   semanticdb-cleanup-cache-files
   semantic-c-describe-environment
   semantic-complete-analyze-inline
   semantic-complete-complete-space
   semantic-complete-inline-project
   semantic-symref-find-tags-by-name
   semantic-symref-find-tags-by-regexp
   semantic-lex-spp-enable-debug-symbol
   semantic-symref-find-references-by-name
   semanticdb-save-current-db
   semantic-analyze-current-tag
   semantic-analyze-current-context
   semantic-test-data-cache
   semantic-analyze-possible-completions
   semantic-c-debug-mode-init
   global-semanticdb-minor-mode
   global-semantic-decoration-mode
   semantic-c-add-preprocessor-symbol
   semantic-complete-jump-local-members
   semantic-complete-analyze-and-replace
   semantic-complete-analyze-inline-idle
   semantic-customize-system-include-path
   semantic-symref-find-file-references-by-name
   semanticdb-find-test-translate-path-no-loading
   global-semantic-idle-summary-mode
   global-semantic-mru-bookmark-mode
   global-semantic-highlight-func-mode
   global-semantic-highlight-edits-mode
   semantic-c-evaluate-symbol-for-hideif
   semantic-ia-complete-tip
   semantic-ia-complete-symbol
   semantic-idle-completions-mode
   semantic-ia-describe-class
   global-semantic-show-parser-state-mode
   global-semantic-show-unmatched-syntax-mode
   semantic-ia-complete-symbol-menu
   semantic-symref-find-tags-by-completion
   global-semantic-idle-completions-mode
   semantic-idle-scheduler-mode
   semantic-ia-show-doc
   semantic-idle-breadcrumbs-mode
   semantic-decoration-include-menu
   semantic-decoration-include-visit
   semantic-decoration-include-describe
   semantic-debug-idle-function
   semantic-add-system-include
   semantic-reset-system-include
   semantic-remove-system-include
   semantic-idle-breadcrumbs-popup-menu
   semantic-idle-breadcrumbs--popup-menu
   semantic-idle-local-symbol-highlight-mode
   semantic-decoration-on-include-menu
   semantic-decoration-all-include-summary
   semantic-decoration-unknown-include-menu
   semantic-decoration-fileless-include-menu
   semantic-decoration-unparsed-include-menu
   semantic-decoration-unknown-include-describe
   semantic-decoration-fileless-include-describe
   semantic-decoration-unparsed-include-describe
   semantic-debug-idle-work-function
   semanticdb-find-adebug-lost-includes
   semanticdb-find-adebug-scanned-includes
   semantic-decoration-unparsed-include-parse-include
   global-semantic-idle-scheduler-mode
   semantic-decoration-on-unknown-include-menu
   semantic-decoration-on-fileless-include-menu
   semantic-decoration-on-unparsed-include-menu
   semantic-decoration-unparsed-include-parse-all-includes
   global-semantic-idle-breadcrumbs-mode
   semantic-stickyfunc-menu
   semantic-stickyfunc-mode
   global-semantic-idle-local-symbol-highlight-mode
   semantic-stickyfunc-popup-menu
   global-semantic-stickyfunc-mode
   ;; senator
   senator-copy-tag
   senator-kill-tag
   senator-next-tag
   senator-yank-tag
   senator-previous-tag
   senator-transpose-tags-up
   senator-transpose-tags-down
   senator-go-to-up-reference
   senator-copy-tag-to-register)

  :config
  ;; include semantic
  (require 'semantic)

  ;; add extra c++ sources
  (cond ((eq system-type 'darwin)
		 (let* ((sk/boost-location "/usr/local/include/boost")
				(sk/armadillo-location "/usr/local/include/armadillo")
				(sk/armadillo-bits-location "/usr/local/include/armadillo_bits")
				(sk/gsl-location "/usr/local/include/gsl"))
		   (if (file-exists-p sk/boost-location)
			   (semantic-add-system-include sk/boost-location 'c++-mode)
			 (message "boost not found"))
		   (if (file-exists-p sk/armadillo-location)
			   (semantic-add-system-include sk/armadillo-location 'c++-mode)
			 (message "armadillo not found"))
		   (if (file-exists-p sk/armadillo-bits-location)
			   (semantic-add-system-include sk/armadillo-bits-location 'c++-mode)
			 (message "armadillo-bits not found"))
		   (if (file-exists-p sk/gsl-location)
			   (semantic-add-system-include sk/gsl-location 'c++-mode)
			 (message "gsl not found"))))
		((eq system-type 'gnu/linux)
		 (let* ((sk/boost-location "/usr/include/boost")
				(sk/armadillo-location "/usr/include/armadillo")
				(sk/armadillo-bits-location "/usr/include/armadillo_bits")
				(sk/gsl-location "/usr/include/gsl"))
		   (if (file-exists-p sk/boost-location)
			   (semantic-add-system-include sk/boost-location 'c++-mode)
			 (message "boost not found"))
		   (if (file-exists-p sk/armadillo-location)
			   (semantic-add-system-include sk/armadillo-location 'c++-mode)
			 (message "armadillo not found"))
		   (if (file-exists-p sk/armadillo-bits-location)
			   (semantic-add-system-include sk/armadillo-bits-location 'c++-mode)
			 (message "armadillo-bits not found"))
		   (if (file-exists-p sk/gsl-location)
			   (semantic-add-system-include sk/gsl-location 'c++-mode)
			 (message "gsl not found")))))

  ;; add extra c sources
  (cond ((eq system-type 'darwin)
		 (let* ((sk/gsl-location "/usr/local/include/gsl"))
		   (if (file-exists-p sk/gsl-location)
			   (semantic-add-system-include sk/gsl-location 'c-mode)
			 (message "gsl not found"))))
		((eq system-type 'gnu/linux)
		 (let* ((sk/gsl-location "/usr/include/gsl"))
		   (if (file-exists-p sk/gsl-location)
			   (semantic-add-system-include sk/gsl-location 'c-mode)
			 (message "gsl not found")))))

  ;; activate semantic mode
  (global-semanticdb-minor-mode 1)
  (global-semantic-idle-scheduler-mode 1)
  (semantic-mode 1))

;; provide the config
(provide 'sk-semantic)
