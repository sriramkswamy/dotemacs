;;; sk-navigation-diminish.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Diminish minor modes from showing up in the mode line

;;; Code:

;; Beacon
(defun sk/diminish-beacon ()
  (interactive)
  (diminish 'beacon-mode ""))
(add-hook 'beacon-mode-hook 'sk/diminish-beacon)

;; Undo tree
(defun sk/diminish-undo-tree ()
  (interactive)
  (diminish 'undo-tree-mode ""))
(add-hook 'undo-tree-mode-hook 'sk/diminish-undo-tree)

;; Helm
(defun sk/diminish-helm ()
  (interactive)
  (diminish 'helm-mode ""))
(add-hook 'helm-mode-hook 'sk/diminish-helm)

;; Ggtags
(defun sk/diminish-ggtags ()
  (interactive)
  (diminish 'ggtags-mode ""))
(add-hook 'ggtags-mode-hook 'sk/diminish-ggtags)
(add-hook 'prog-mode-hook 'sk/diminish-ggtags)
(add-hook 'text-mode-hook 'sk/diminish-ggtags)

;; Back button
(defun sk/diminish-back-button ()
  (interactive)
  (diminish 'back-button-mode ""))
(add-hook 'back-button-mode-hook 'sk/diminish-back-button)

(provide 'sk-navigation-diminish)
;;; sk-navigation-diminish.el ends here
