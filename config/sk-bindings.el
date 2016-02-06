;;; Just some bindings using bind key
(sk/require-package 'bind-key)

;; Top level changes
(bind-keys*
 ("C-=" . avy-goto-line)                   ; Jump to any line on screen
 ("C-q" . avy-goto-char)                   ; Jump to any char on screen
 ("M-y" . counsel-yank-pop)                ; Browse and search the clipboard
 ("M-;" . comment-dwim-2)                  ; Comment and uncomment toggles
 )

;; Custom bindings
(bind-keys*
 ("C-c /" . counsel-locate)                ; Use the locate command
 ("C-c =" . indent-region)                 ; Indent based on language
 ("C-c [" . widen)                         ; Get complete context back
 ("C-c ]" . narrow-to-region)              ; Hide context and narrow
 ("C-c `" . sk/hydra-of-hydras/body)       ; Home menu
 ("C-c a" . sk/hydra-of-activate/body)     ; Activate menu
 ("C-c e" . sk/hydra-wgrep/body)           ; Wgrep for refactoring
 ("C-c f" . sk/hydra-of-navigation/body)   ; Files/Buffers/Bookmarks
 ("C-c g" . sk/hydra-of-git/body)          ; Git menu
 ("C-c h" . sk/hydra-of-help/body)         ; Help menu
 ("C-c i" . sk/hydra-of-edits/body)        ; Quick selection/copy/paste
 ("C-c j" . sk/hydra-of-jump/body)         ; Arbitrary jumps based on context
 ("C-c l" . sk/hydra-of-langs/body)        ; Language specific menu
 ("C-c m" . sk/hydra-of-macros/body)       ; Macro execution
 ("C-c o" . sk/hydra-of-org/body)          ; Org mode menu
 ("C-c p" . clipboard-yank)                ; Paste from Mac system clipboard
 ("C-c r" . vr/query-replace)              ; Find/replace
 ("C-c s" . sk/hydra-of-search/body)       ; Search anything menu
 ("C-c t" . sk/hydra-tags/body)            ; CTags for coding
 ("C-c w" . sk/hydra-of-windows/body)      ; Window configuration menu
 ("C-c x" . counsel-M-x)                   ; M-x
 ("C-c y" . clipboard-kill-ring-save)      ; Save to Mac system clipboard
 )

;; Rebind the same C-c keys with the same mnemonic keys to be used in God-mode
(bind-keys*
 ("C-z C-/" . counsel-locate)              ; Use the locate command
 ("C-z C-=" . indent-region)               ; Indent based on language
 ("C-z C-[" . widen)                       ; Get complete context back
 ("C-z C-]" . narrow-to-region)            ; Hide context and narrow
 ("C-z C-`" . sk/hydra-of-hydras/body)     ; Home menu
 ("C-z C-a" . sk/hydra-of-activate/body)   ; Activate menu
 ("C-z C-e" . sk/hydra-wgrep/body)         ; Wgrep for refactoring
 ("C-z C-f" . sk/hydra-of-navigation/body) ; Files/Buffers/Bookmarks
 ("C-z C-g" . sk/hydra-of-git/body)        ; Git menu
 ("C-z C-h" . sk/hydra-of-help/body)       ; Help menu
 ("C-z C-i" . sk/hydra-of-edits/body)      ; Quick selection/copy/paste
 ("C-z C-j" . sk/hydra-of-jump/body)       ; Arbitrary jumps based on context
 ("C-z C-l" . sk/hydra-of-langs/body)      ; Language specific menu
 ("C-z C-m" . sk/hydra-of-macros/body)     ; Macro execution
 ("C-z C-o" . sk/hydra-of-org/body)        ; Org mode menu
 ("C-z C-p" . clipboard-yank)              ; Paste from Mac system clipboard
 ("C-z C-r" . vr/query-replace)            ; Find/replace
 ("C-z C-s" . sk/hydra-of-search/body)     ; Search anything menu
 ("C-z C-t" . sk/hydra-tags/body)          ; CTags for coding
 ("C-z C-w" . sk/hydra-of-windows/body)    ; Window configuration menu
 ("C-z C-x" . counsel-M-x)                 ; M-x
 ("C-z C-y" . clipboard-kill-ring-save)    ; Save to Mac system clipboard
 )

(provide 'sk-bindings)

;;; sk-bindings.el ends here
