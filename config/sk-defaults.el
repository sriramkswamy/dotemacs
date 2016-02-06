;; Mac stuff
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta))

;; Enable winner-mode
(add-hook 'prog-mode-hook #'winner-mode)
(add-hook 'text-mode-hook #'winner-mode)

;; GUI changes
(defun sk/gui-defaults ()
  (interactive)
  (load-theme 'leuven t)
  (tool-bar-mode -1)
  (recentf-mode)
  (scroll-bar-mode -1))
(add-hook 'after-init-hook 'sk/gui-defaults)

;; Swiper like regex when using isearch
(setq search-whitespace-regexp ".*?")

;; No welcome screen - opens directly in scratch buffer
(setq inhibit-startup-message t
      initial-scratch-message ";; Scratch
;; Press C-x C-f to open a file. It opens in home(~) directory by default
;; To open init file, C-x C-f and then '~/.emacs.d/init.el'
;; Open '~/.emacs.d/config/sk-bindings.el' to view/modify custom bindings
;; Type 'C-h t' for Emacs tutorial"
      initial-major-mode 'fundamental-mode
      visible-bell nil
      inhibit-splash-screen t)

;; DocView Settings
(setq doc-view-continuous t
      doc-view-resolution 200)

;; Don't blink the cursor
(blink-cursor-mode -1)

;; GDB
(setq gdb-many-windows t
      gdb-show-main t)

;; Which function mode
(add-hook 'prog-mode-hook 'which-function-mode)

;; Backups at .saves folder in the current folder
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;; Ediff plain window and vertical
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;; Remote file navigation
(setq tramp-ssh-controlmaster-options "ssh")

;; Narrow to region
(put 'narrow-to-region 'disabled nil)

;; Column number mode
(column-number-mode)

;; Hide/Show mode
(defun sk/diminish-hs-minor ()
  (interactive)
  (diminish 'hs-minor-mode ""))
(add-hook 'hs-minor-mode-hook 'sk/diminish-hs-minor)
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; Diminish some stuff
(defun sk/diminish-abbrev ()
  (interactive)
  (diminish 'abbrev-mode ""))
(add-hook 'abbrev-mode-hook 'sk/diminish-abbrev)
(add-hook 'prog-mode-hook 'sk/diminish-abbrev)
(add-hook 'text-mode-hook 'sk/diminish-abbrev)

;; Diminish some stuff
(defun sk/diminish-auto-fill ()
  (interactive)
  (diminish 'auto-fill-mode ""))
(add-hook 'abbrev-mode-hook 'sk/diminish-auto-fill)
(add-hook 'prog-mode-hook 'sk/diminish-auto-fill)
(add-hook 'text-mode-hook 'sk/diminish-auto-fill)

;; Subword mode
(defun sk/diminish-subword ()
  (interactive)
  (diminish 'subword-mode ""))
(add-hook 'subword-mode-hook 'sk/diminish-subword)
(global-subword-mode)

;; Eldoc
(defun sk/diminish-eldoc ()
  (interactive)
  (diminish 'eldoc-mode ""))
(add-hook 'eldoc-mode-hook 'sk/diminish-eldoc)

(provide 'sk-defaults)
;;; sk-defaults.el ends here
