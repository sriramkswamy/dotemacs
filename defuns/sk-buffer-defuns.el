;; non-native fullscreen
(defun sk/toggle-frame-fullscreen-non-native ()
  "Toggle full screen non-natively. Uses the `fullboth' frame paramerter
   rather than `fullscreen'. Useful to fullscreen on OSX w/o animations."
  (interactive)
  (modify-frame-parameters
   nil
   `((maximized
      . ,(unless (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
		   (frame-parameter nil 'fullscreen)))
     (fullscreen
      . ,(if (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
			 (if (eq (frame-parameter nil 'maximized) 'maximized)
				 'maximized)
		   'fullboth)))))

;; browse the current HTML file in browser
(defun sk/browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
	  (browse-url (concat "file://" file-name)))))

;; rename buffer and file
(defun sk/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

;; delete buffer and file
(defun sk/delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

;; copy the file path
(defun sk/copy-current-file-path ()
  "Add current file path to kill ring. Limits the filename to project root if possible."
  (interactive)
  (kill-new buffer-file-name))

;; close compilation buffer automatically if no errors
(setq compilation-finish-functions
      (lambda (buf str)
        (if (null (string-match ".*exited abnormally.*" str))
            ;;no errors, make the compilation window go away in a few seconds
            (progn
              (run-at-time "0.4 sec" nil
                           (lambda ()
                             (select-window (get-buffer-window (get-buffer-create "*compilation*")))
                             (switch-to-buffer nil)))
              (message "No Compilation Errors!")))))

;; kill current buffer or prompt for buffer
(defun sk/kill-buffer (askp)
  "with no prefix, kill the current buffer without prompt
 with prefix, select which buffer to kill"
  (interactive "P")
  (if askp
	  (kill-buffer (read-buffer
					"Kill buffer: "
					(mapcar #'buffer-name (buffer-list))))
	(kill-this-buffer)))

;; Doc View autofit mode -
;; http://stackoverflow.com/questions/23236555/making-document-view-in-emacs-fit-to-width-of-page

;; set and remove breakpoints
(defun sk/breakpoint-icon-set ()
  "set breakpoint and icon"
  (interactive)
  (require 'gdb-mi)
  (gdb-put-breakpoint-icon t (concat (buffer-file-name) " "
									 (number-to-string (line-number-at-pos)))))

(defun sk/breakpoint-icon-remove ()
  "remove breakpoint and icon"
  (interactive)
  (require 'gdb-mi)
  (gdb-remove-breakpoint-icons (line-beginning-position) (line-end-position)))

;; ediff do what i mean
;; https://scripter.co/do-ediff-as-i-mean/
(defun sk/ediff-dwim ()
  "Do ediff as I mean.

If a region is active, call `ediff-regions-wordwise'.
Else if the frame has 2 windows with identical major modes,
  - Do `ediff-files' if the buffers are associated to files and the buffers
    have not been modified.
  - Do `ediff-buffers' otherwise.
Else if the current is a file buffer with a VC backend, call `vc-ediff'
Else call `ediff-buffers'."
  (interactive)
  (let* ((num-win (safe-length (window-list)))
         (bufa (get-buffer (buffer-name)))
         (filea (buffer-file-name bufa))
         (modea (with-current-buffer bufa major-mode))
         bufb fileb modeb)
    (save-excursion
      (other-window 1)
      (setq bufb (get-buffer (buffer-name)))
      (setq fileb (buffer-file-name bufb))
      (setq modeb (with-current-buffer bufb major-mode)))
    (cond
     ;; If a region is selected
     ((region-active-p)
      (call-interactively #'ediff-regions-wordwise))
     ;; Else if 2 windows with same major modes
     ((and (= 2 num-win)
           (eq modea modeb))
      (if ;; If either of the buffers is not associated to a file,
          ;; or if either of the buffers is modified
          (or (null filea)
              (null fileb)
              (buffer-modified-p bufa)
              (buffer-modified-p bufb))
          (progn
            (message "Running (ediff-buffers \"%s\" \"%s\") .." bufa bufb)
            (ediff-buffers bufa bufb))
        (progn
          (message "Running (ediff-files \"%s\" \"%s\") .." filea fileb)
          (ediff-files filea fileb))))
     ;; Else if file in current buffer has a vc backend
     ((and filea
           (vc-registered filea))
      (call-interactively #'vc-ediff))
     ;; Else call `ediff-buffers'
     (t
      (call-interactively #'ediff-buffers)))))

;; rclone sync
(defun sk/org-rclone-sync-to-dropbox ()
  (interactive)
  (shell-command
   (concat "rclone sync "
		   (getenv "HOME") "/Dropbox/org"
		   " drop:org")))
(defun sk/org-rclone-sync-from-dropbox ()
  (interactive)
  (shell-command
   (concat "rclone sync "
		   "drop:org "
		   (getenv "HOME") "/Dropbox/org")))

;; provide this configuration
(provide 'sk-buffer-defuns)
