(use-package poly-markdown
  :ensure t
  :mode ("\\.md$" . poly-markdown-mode))

(use-package poly-R
  :ensure t
  :mode ("\\.Rmd$" . poly-markdown+R-mode)

  :config
  (ryo-modal-major-mode-keys
   'markdown-mode
   ;; polymode
   ("m a j" polymode-next-chunk :name "next chunk")
   ("m a J" polymode-next-chunk-same-type :name "next chunk of type")
   ("m a k" polymode-previous-chunk :name "prev chunk")
   ("m a K" polymode-previous-chunk-same-type :name "prev chunk same type")
   ("m a e" polymode-export :name "export")
   ("m a E" polymode-set-exporter :name "set exporter")
   ("m a w" markdown-cleanup-list-numbers :name "weave")
   ("m a W" markdown-complete-buffer :name "set weaver")
   ("m a c" polymode-toggle-chunk-narrowing :name "narrow chunk")
   ("m a x" polymode-kill-chunk :name "kill chunk")

   ;; eval
   ("m a a" polymode-eval-buffer :name "eval buffer")
   ("m a r" polymode-eval-region-or-chunk :name "eval region or chunk")))

;; provide the configuration
(provide 'sk-poly)
