;;;###autoload
;; Thanks to https://oremacs.com/2016/02/24/dired-rsync/
(defun sk/dired-rsync (dest)
  (interactive
   (list
    (expand-file-name
     (read-file-name
      "Rsync to:"
      (dired-dwim-target-directory)))))
  ;; store all selected files into "files" list
  (let ((files (dired-get-marked-files
                nil current-prefix-arg))
        ;; the rsync command
        (tmtxt/rsync-command
         "rsync -arvz --progress "))
    ;; add all selected file names as arguments
    ;; to the rsync command
    (dolist (file files)
      (setq tmtxt/rsync-command
            (concat tmtxt/rsync-command
                    (shell-quote-argument file)
                    " ")))
    ;; append the destination
    (setq tmtxt/rsync-command
          (concat tmtxt/rsync-command
                  (shell-quote-argument dest)))
    ;; run the async shell command
    (async-shell-command tmtxt/rsync-command "*rsync*")
    ;; finally, switch to that window
    (other-window 1)))

(defun sk/dired-rsync-map ()
  "rsync map to dired"
  (define-key dired-mode-map "Y" 'sk/dired-rsync))
(add-hook 'dired-mode-hook 'sk/dired-rsync-map)

;; https://fuco1.github.io/2017-05-01-Support-for-imenu-in-dired.html
;; imenu in dired
(defun sk/dired-imenu-prev-index-position (&optional arg)
  "Go to the header line of previous directory."
  (interactive "p")
  (unless (= (line-number-at-pos) 1)
    (call-interactively 'dired-prev-subdir)
    t))

(defun sk/dired-extract-index-name ()
  "Extract name of the current item for imenu."
  (save-excursion
    (back-to-indentation)
    (buffer-substring-no-properties
     (point)
     (1- (re-search-forward ":$")))))

(defun sk/dired-imenu-create-index ()
  "Create `imenu' index for dired."
  (let* ((alist (imenu-default-create-index-function))
         (uniquified (f-uniquify-alist (-map 'car alist))))
    (--remove
     (= 0 (length (car it)))
     (--map (cons (cdr (assoc (car it) uniquified)) (cdr it))
            alist))))

(defun sk/dired-imenu-init ()
  "Initialize `imenu' variables in current buffer."
  (setq-local imenu-prev-index-position-function
              'sk/dired-imenu-prev-index-position)
  (setq-local imenu-extract-index-name-function
              'sk/dired-extract-index-name)
  (setq-local imenu-create-index-function
              'sk/dired-imenu-create-index))
(add-hook 'dired-mode-hook 'sk/dired-imenu-init)

;; provide dired functions
(provide 'sk-dired-defuns)
