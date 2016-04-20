;;; sk-defaults.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; My custom maps for modal editing based on Modalka. This follows a Vi-style

;;; Code:

;; Enable winner-mode
(add-hook 'prog-mode-hook #'winner-mode)
(add-hook 'text-mode-hook #'winner-mode)

;; GUI changes
(defun sk/gui-defaults ()
  (interactive)
  (load-theme 'leuven t)
  (recentf-mode)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1))
(add-hook 'after-init-hook 'sk/gui-defaults)

;; Bar cursor
(setq-default cursor-type '(bar . 1))

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
      doc-view-resolution 300)

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

;; default global bindings
(require 'sk-defaults-bindings)

(provide 'sk-defaults)
;;; sk-defaults.el ends here
