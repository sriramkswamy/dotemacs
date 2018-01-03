;; better syntax highlighting for modern c++
(use-package modern-cpp-font-lock
  :ensure t
  :diminish modern-c++-font-lock-mode
  :hook (c++-mode . modern-c++-font-lock-mode))

;; provide configuration for all c based languages
(provide 'sk-clang)
