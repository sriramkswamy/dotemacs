(defun sk/mark-inside-org-code ()
  "Select inside an Org code block without the org specific syntax"
  (interactive)
  (er/mark-org-code-block)
  (next-line 1)
  (exchange-point-and-mark)
  (previous-line 1)
  (end-of-line 1))

(defun sk/mark-around-word ()
  "Mark the word and the adjacent whitespace"
  (interactive)
  (if (looking-at-p "[[:space:]]")
	  (mark-word)
	(er/mark-word)
	(exchange-point-and-mark)
	(if (looking-at-p "[[:space:]]")
		(forward-char 1)
	  (exchange-point-and-mark)
	  (backward-char 1)
	  (if (looking-at-p "[[:space:]]")
		  (exchange-point-and-mark)
		(forward-char 1)))))

(defun sk/mark-inside-LaTeX-math ()
  "Mark inside the latex math"
  (interactive)
  (er/mark-LaTeX-math)
  (forward-char 1)
  (exchange-point-and-mark)
  (backward-char 1))

(defun sk/mark-inside-python-block ()
  "Mark inside a python block"
  (interactive)
  (er/mark-python-block)
  (next-line 1))

(defun sk/mark-inside-ruby-block ()
  "Mark inside a ruby/julia block"
  (interactive)
  (er/mark-ruby-block-up)
  (next-line 1)
  (exchange-point-and-mark)
  (previous-line 1))

(defun sk/mark-around-symbol ()
  "Mark around a symbol including the nearby whitespace"
  (interactive)
  (er/mark-symbol)
  (exchange-point-and-mark)
  (forward-char 1))

;; provide expand region wrapper functions
(provide 'sk-expand-defuns)
