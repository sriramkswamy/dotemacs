;;; -*- lexical-binding: t; -*-

(require 'flycheck)
(require 'matlab-server)
(require 's)

(defun flycheck-matlab--verify (checker)
  "Verify the matlab syntax checker."
  (list
   (flycheck-verification-result-new
    :label "mlint path checking"
    :message (if (and matlab-server-process
		      (eq (process-status matlab-server-process) 'run))
		 "matlab is on" "matlab process not found")
    :face (if t
	      'success '(bold error)) )))

(defun flycheck-matlab-get-error (filepath)
  (if (and matlab-server-process
	   (eq (process-status matlab-server-process) 'run))
      (let ((rawerrstr (matlab-send-request-sync
			(s-trim (concat "arrayfun(@(x) display(sprintf('%d\t%d\t%s', x.line, x.column(1), x.message)), checkcode('"
					filepath "'))")))))
	(delq nil
	      (mapcar (lambda (rrs)
			(let* ((cleanrs (s-replace ">>" "" rrs))
			       (cleanrs-split (s-split "\t" (s-trim cleanrs))))
			  (when (= (length cleanrs-split) 3)
			    (cl-multiple-value-bind (lpostart cpostart estr) cleanrs-split
			      `(,(string-to-int lpostart) ,(string-to-int lpostart) ,(string-to-int cpostart) ,(string-to-int cpostart) ,estr)))))
		      (s-split "\n" rawerrstr))))))


(defun flycheck-matlab--start (checker callback)
  (let* ((rawerror (flycheck-matlab-get-error (buffer-file-name)))
	 (errors (mapcar (lambda (rerr)
			   (cl-multiple-value-bind (lpostart lposend cpostart cposend estr) rerr
			     (flycheck-error-new-at lpostart cpostart 'warning estr :checker checker)))
			 rawerror)))
    (funcall callback 'finished errors)))


(flycheck-define-generic-checker 'flycheck-matlab
  "A syntax checker for matlab."
  :start #'flycheck-matlab--start
  :verify #'flycheck-matlab--verify
  :modes '(matlab-mode)
  ;; :error-filter flycheck-matlab-error-filter
  :predicate (lambda ()
	       (eq major-mode 'matlab-mode)))

(add-to-list 'flycheck-checkers 'flycheck-matlab)

(provide 'flycheck-matlab)
