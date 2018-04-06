;; getting "ESC" to work correctly in the terminal
(defvar sk/fast-keyseq-timeout 50)

(defun sk/tty-ESC-filter (map)
  (if (and (equal (this-single-command-keys) [?\e])
		   (sit-for (/ sk/fast-keyseq-timeout 1000.0)))
	  [escape] map))

(defun sk/lookup-key (map key)
  (catch 'found
	(map-keymap (lambda (k b) (if (equal key k) (throw 'found b))) map)))

(defun sk/catch-tty-ESC ()
  "Setup key mappings of current terminal to turn a tty's ESC into `escape'."
  (interactive)
  (when (memq (terminal-live-p (frame-terminal)) '(t pc))
	(let ((esc-binding (sk/lookup-key input-decode-map ?\e)))
	  (define-key input-decode-map
		[?\e] `(menu-item "" ,esc-binding :filter sk/tty-ESC-filter)))))

(add-hook 'after-init-hook 'sk/catch-tty-ESC)
(add-hook 'prog-mode-hook 'sk/catch-tty-ESC)
(add-hook 'text-mode-hook 'sk/catch-tty-ESC)

;; bind it explicity
(bind-key* "C-x c" 'sk/catch-tty-ESC)

;; wrapper for entering and exiting ryo modal mode
(defun sk/enable-ryo-modal-mode ()
  "Explicitly enables ryo-modal mode"
  (interactive)
  (ryo-modal-mode 1))
(defun sk/disable-ryo-modal-mode ()
  "Explicitly disables ryo-modal mode"
  (interactive)
  (ryo-modal-mode -1))

;; disable in magit commit buffer alone
(add-hook 'with-editor-mode-hook 'sk/disable-ryo-modal-mode)

;; open and edit line below
(defun sk/open-and-edit-line-below ()
  (interactive)
  (sk/open-line-below)
  (ryo-modal-mode -1))

;; open and edit line above
(defun sk/open-and-edit-line-above ()
  (interactive)
  (sk/open-line-above)
  (ryo-modal-mode -1))

;; use-package config
(use-package ryo-modal
  :ensure t
  :chords (("  " . sk/enable-ryo-modal-mode))
  :diminish (ryo-modal-mode . " μ")

  :hook ((text-mode        . sk/enable-ryo-modal-mode)
         (prog-mode        . sk/enable-ryo-modal-mode)
         (fundamental-mode . sk/enable-ryo-modal-mode)
         (doc-view-mode    . sk/enable-ryo-modal-mode))

  :bind (("<escape>" . sk/enable-ryo-modal-mode)
         :map ryo-modal-mode-map
         ("<escape>" . sk/remove-mark)
         ("."        . ryo-modal-repeat)
		 (">"        . mc/mark-next-like-this)
		 ("<"        . mc/mark-previous-like-this)
		 ("'"        . mc/edit-lines))

  :init
  ;; remove ryo key word from which-key
  (push '((nil . "ryo:\\w+:") . (nil . "")) which-key-replacement-alist)

  ;; default cursor shape
  (setq-default cursor-type '(bar . 2))
  ;; cursor for ryo modal mode
  (defvar ryo-modal-cursor-color "DarkGrey"
  	"The cursor color used in `ryo-modal-mode'.  If nil then use default color.")

  :config
  (sk/enable-ryo-modal-mode))

;; provide the modal configuration
(provide 'sk-ryo-modal)
