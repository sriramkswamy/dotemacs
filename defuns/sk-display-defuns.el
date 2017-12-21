;; Some convenience functions for changing fonts

;;;###autoload
(defun sk/courier-font ()
  "Change font to Courier"
  (interactive)
  (set-face-attribute 'default nil :font "Courier")
  (set-frame-width (selected-frame) 97))

;;;###autoload
(defun sk/georgia-font ()
  "Change font to Georgia"
  (interactive)
  (set-face-attribute 'default nil :font "Georgia" :height 160))

;;;###autoload
(defun sk/hack-font ()
  "Change font to Hack"
  (interactive)
  (set-face-attribute 'default nil :font "Hack"))

;;;###autoload
(defun sk/monaco-font ()
  "Change font to Monaco"
  (interactive)
  (set-face-attribute 'default nil :font "Monaco"))

;;;###autoload
(defun sk/consolas-font ()
  "Change font to Consolas"
  (interactive)
  (set-face-attribute 'default nil :font "Consolas"))

;;;###autoload
(defun sk/deja-vu-font ()
  "Change font to DejaVu Sans Mono"
  (interactive)
  (set-face-attribute 'default nil :font "DejaVu Sans Mono"))

;; Some easy functions for font types

;;;###autoload
(defun sk/tiny-type ()
  "Reduce the font size to tiny"
  (interactive)
  (set-face-attribute 'default nil  :height 150))

;;;###autoload
(defun sk/miniscule-type ()
  "Reduce the font size to miniscule"
  (interactive)
  (set-face-attribute 'default nil  :height 140))

;;;###autoload
(defun sk/small-type ()
  "Reduce the font size to small"
  (interactive)
  (set-face-attribute 'default nil  :height 190)
  (set-frame-width (selected-frame) 89))

;;;###autoload
(defun sk/medium-type ()
  "Reduce the font size to medium"
  (interactive)
  (set-face-attribute 'default nil  :height 215)
  (set-frame-width (selected-frame) 89))

;;;###autoload
(defun sk/large-type ()
  "Reduce the font size to large"
  (interactive)
  (set-face-attribute 'default nil  :height 350)
  (set-frame-width (selected-frame) 68))

;; provide these functions
(provide 'sk-display-defuns)
