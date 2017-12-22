;; Microsoft Language Server Protocol (LSP)
(use-package lsp-mode
  :ensure t
  :config
  ;; bindings
  (ryo-modal-key "m ?" 'lsp-capabilities :mode 'lsp-mode :name "lsp capability"))

;; provide lsp configuration
(provide 'sk-lsp)
