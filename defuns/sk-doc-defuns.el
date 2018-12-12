;; document in other window
(defun sk/other-doc-down ()
  "Other document next page"
  (interactive)
  (other-window 1)
  (doc-view-next-page)
  (other-window 1))
(defun sk/other-doc-up ()
  "Other document prev page"
  (interactive)
  (other-window 1)
  (doc-view-previous-page)
  (other-window 1))

;; document fit to page in window
(defun sk/doc-fit ()
  "Document next page"
  (interactive)
  (doc-view-fit-page-to-window))

;; document fit to page in other window
(defun sk/other-pdf-fit ()
  "Other document next page"
  (interactive)
  (other-window 1)
  (pdf-view-fit-page-to-window)
  (other-window 1))

;; document revert
(defun sk/pdf-revert ()
  "Document revert"
  (interactive)
  (pdf-view-revert-buffer nil t))

;; document revert in the other window
(defun sk/pdf-revert-other-window ()
  "Document revert in the other window"
  (interactive)
  (other-window 1)
  (pdf-view-revert-buffer nil t)
  (other-window 1))

;; document in other window
(defun sk/other-pdf-down ()
  "Other document next page"
  (interactive)
  (other-window 1)
  (pdf-view-next-page)
  (other-window 1))
(defun sk/other-pdf-up ()
  "Other document prev page"
  (interactive)
  (other-window 1)
  (pdf-view-previous-page)
  (other-window 1))

;; document fit to page in window
(defun sk/pdf-fit ()
  "Document next page"
  (interactive)
  (pdf-view-fit-page-to-window))

;; document fit to page in other window
(defun sk/other-pdf-fit ()
  "Other document next page"
  (interactive)
  (other-window 1)
  (pdf-view-fit-page-to-window)
  (other-window 1))

;; document revert
(defun sk/pdf-revert ()
  "Document revert"
  (interactive)
  (pdf-view-revert-buffer nil t))

;; document revert in the other window
(defun sk/pdf-revert-other-window ()
  "Document revert in the other window"
  (interactive)
  (other-window 1)
  (pdf-view-revert-buffer nil t)
  (other-window 1))

;; add interleave note for the pdf in the other window
(defun sk/interleave-other-window-note ()
  "Add interleaved note for the page in other window"
  (interactive)
  (sk/enable-ryo-modal-mode)
  (other-window 1)
  (interleave-add-note)
  (sk/disable-ryo-modal-mode))

;; add interleave next page in other window
(defun sk/interleave-other-window-next ()
  "Move the next page in the other window when interleaving"
  (interactive)
  (sk/enable-ryo-modal-mode)
  (other-window 1)
  (interleave-go-to-next-page)
  (other-window 1))

;; add interleave previous page in other window
(defun sk/interleave-other-window-previous ()
  "Move the previous page in the other window when interleaving"
  (interactive)
  (sk/enable-ryo-modal-mode)
  (other-window 1)
  (interleave-go-to-previous-page)
  (other-window 1))

;; provide doc defuns
(provide 'sk-doc-defuns)
