;; easier way to make desktops
(defun sk/make-desktop ()
  "opens dired in the desktop directory to make a new folder"
  (interactive)
  (dired (concat user-emacs-directory "desktops")))

;; open dropbox folder
(defun sk/open-dropbox ()
  "opens dired in the dropbox directory to make a new folder"
  (interactive)
  (dired "~/Dropbox"))

;; provide these functions
(provide 'sk-navigation-defuns)
