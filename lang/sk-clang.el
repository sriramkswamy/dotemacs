;; better syntax highlighting for modern c++
(use-package modern-cpp-font-lock
  :ensure t
  :diminish modern-c++-font-lock-mode
  :hook (c++-mode . modern-c++-font-lock-mode))

;; indexer based on cmake and compile_commands.json
(use-package rtags
  :ensure t
  :ensure-system-package (cmake)

  :init
  (setq rtags-completions-enabled t)
  :hook ((c++-mode . rtags-start-process-unless-running)
		 (c-mode . rtags-start-process-unless-running))

  :commands
  (rtags-mode
   rtags-fixit
   rtags-imenu
   rtags-tokens
   rtags-install
   rtags-taglist
   rtags-diagnostics
   rtags-quit-rdm
   rtags-find-file
   rtags-next-diag
   rtags-is-running
   rtags-next-match
   rtags-make-member
   rtags-compile-file
   rtags-include-file
   rtags-list-results
   rtags-reparse-file
   rtags-taglist-mode
   rtags-close-taglist
   rtags-previous-diag
   rtags-check-includes
   rtags-previous-match
   rtags-recompile-file
   rtags-unsuspend-file
   rtags-dependency-tree
   rtags-find-references
   rtags-preprocess-file
   rtags-preprocess-mode
   rtags-references-tree
   rtags-restart-process
   rtags-diagnostics-mode
   rtags-clear-diagnostics
   rtags-compilation-flags
   rtags-print-dependencies
   rtags-bury-or-delete
   rtags-post-command-hook
   rtags-maybe-reparse-file
   rtags-update-buffer-list
   rtags-dependency-tree-all
   rtags-remove-other-window
   rtags-dependency-tree-mode
   rtags-references-tree-mode
   rtags-print-class-hierarchy
   rtags-create-doxygen-comment
   rtags-print-current-location
   rtags-restart-tracking-timer
   rtags-update-current-project
   rtags-display-tooltip-function
   rtags-clear-diagnostics-overlays
   rtags-fix-fixit-at-point
   rtags-call-bury-or-delete
   rtags-apply-fixit-at-point
   rtags-find-virtuals-at-point
   rtags-find-references-at-point
   rtags-dependency-tree-find-path
   rtags-dependency-tree-expand-all
   rtags-dependency-tree-next-level
   rtags-references-tree-expand-all
   rtags-references-tree-next-level
   rtags-find-references-current-dir
   rtags-dependency-tree-collapse-all
   rtags-find-references-current-file
   rtags-references-tree-collapse-all
   rtags-restart-find-container-timer
   rtags-clear-all-diagnostics-overlays
   rtags-dependency-tree-expand-current
   rtags-dependency-tree-previous-level
   rtags-references-tree-expand-current
   rtags-references-tree-previous-level
   rtags-dependency-tree-collapse-current
   rtags-references-tree-collapse-current
   rtags-select
   rtags-print-enum-value-at-point
   rtags-find-all-references-at-point
   rtags-copy-and-print-current-location
   rtags-restart-update-current-project-timer
   rtags-dependency-tree-toggle-current-expanded
   rtags-references-tree-toggle-current-expanded
   rtags-stack-cost
   rtags-symbol-info
   rtags-symbol-type
   rtags-suspend-file
   rtags-select-caller
   rtags-stop-diagnostics
   rtags-find-functions-called-by-this-function
   rtags-find-symbol
   rtags-rename-symbol
   rtags-display-summary
   rtags-show-rtags-buffer
   rtags-suspend-all-files
   rtags-select-other-window
   rtags-set-current-project
   rtags-start-process-maybe
   rtags-set-diagnostics-suspended
   rtags-after-save-hook
   rtags-print-symbol-info
   rtags-location-stack-back
   rtags-location-stack-jump
   rtags-list-suspended-files
   rtags-location-stack-reset
   rtags-clear-suspended-files
   rtags-location-stack-filter
   rtags-location-stack-forward
   rtags-print-source-arguments
   rtags-location-stack-visualize
   rtags-enable-standard-keybindings
   rtags-show-in-other-window
   rtags-select-caller-other-window
   rtags-set-periodic-reparse-timeout
   rtags-start-process-unless-running
   rtags-toggle-file-suspended
   rtags-toggle-diagnostics-suspended
   rtags-find-symbol-at-point
   rtags-find-symbol-current-dir
   rtags-find-symbol-current-file
   rtags-display-summary-as-message
   rtags-location-stack-visualize-mode
   rtags-show-target-in-other-window
   rtags-select-and-remove-rtags-buffer
   rtags-cycle-overlays-on-screen
   rtags-get-include-file-for-symbol
   rtags-goto-offset
   rtags-guess-function-at-point))

;; Shell auto completion
(use-package company-rtags
  :ensure t
  :hook ((c++-mode . sk/company-clang)
		 (c-mode . sk/company-clang))
  :bind* (("C-j r" . company-rtags))
  :bind (:map c++-mode-map
			  ("C-d" . company-rtags)))

;; provide configuration for all c based languages
(provide 'sk-clang)
