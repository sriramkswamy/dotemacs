;;; sk-org-bindings.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Org mode bindings

;;; Code:

;; Default org bindings
(global-set-key (kbd "C-c O a") 'org-agenda)
(global-set-key (kbd "C-c O c") 'org-capture)
(global-set-key (kbd "C-c O i") 'org-insert-link)
(global-set-key (kbd "C-c O s") 'org-store-link)
(global-set-key (kbd "C-c O S") 'org-list-make-subtree)
(global-set-key (kbd "C-c O A") 'org-archive-subtree)
(global-set-key (kbd "C-c O g") 'org-goto)
(global-set-key (kbd "C-c O l") 'org-toggle-latex-fragment)
(global-set-key (kbd "C-c O L") 'org-toggle-link-display)
(global-set-key (kbd "C-c O I") 'org-toggle-inline-images)
(global-set-key (kbd "C-c O k") 'org-cut-subtree)
(global-set-key (kbd "C-c O V") 'org-reveal)
(global-set-key (kbd "C-c O R") 'org-refile)
(global-set-key (kbd "C-c O y") 'org-copy-subtree)
(global-set-key (kbd "C-c O h") 'org-toggle-heading)
(global-set-key (kbd "C-c O H") 'org-insert-heading-respect-content)
(global-set-key (kbd "C-c O e") 'org-export-dispatch)
(global-set-key (kbd "C-c O u") 'org-update-dblock)
(global-set-key (kbd "C-c O U") 'org-update-all-dblocks)
(global-set-key (kbd "C-c O F") 'org-footnote)
(global-set-key (kbd "C-c O ]") 'org-narrow-to-subtree)
(global-set-key (kbd "C-c O [") 'widen)
(global-set-key (kbd "C-c O N") 'org-note)
(global-set-key (kbd "C-c O O") 'org-open-at-point)
(global-set-key (kbd "C-c O F") 'org-attach)
(global-set-key (kbd "C-c O E") 'org-set-effort)
(global-set-key (kbd "C-c O B") 'org-table-blank-field)
(global-set-key (kbd "C-c O M") 'org-mark-subtree)
(global-set-key (kbd "C-c O <") 'org-date-from-calendar)
(global-set-key (kbd "C-c O >") 'org-goto-calendar)

;; Deft
(global-set-key (kbd "C-c O f") 'deft)

;; Interleave
(global-set-key (kbd "C-c O n") 'interleave)

;; Modal key bindings
(require 'sk-org-modalka)

;; which key explanations
(require 'sk-org-bindings-which-key)

(provide 'sk-org-bindings)
;;; sk-org-bindings.el ends here
