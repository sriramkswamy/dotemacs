;;; Packages
(require 'package)

;; Add packages
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; packages based on versions
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)
(setq package-enable-at-startup nil)

;; Add homebrew packages
(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; Define function for a required package
(defun require-package (package)
  (setq-default highlight-tabs t)
  "Install given PACKAGE."
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

;; Paradox for package
(require-package 'paradox)
(setq paradox-github-token t)

;; Get the proper path
(require-package 'exec-path-from-shell)
(exec-path-from-shell-copy-env "PYTHONPATH")
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;;; GUI
;; No welcome screen - opens directly in scratch buffer
(setq inhibit-startup-message t
      inhibit-splash-screen t)

;; No toolbar and scrollbar. Menubar only in GUI.
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(unless (display-graphic-p) (menu-bar-mode -1))

;; Non-native fullscreen
(setq ns-use-native-fullscreen nil)
(defun toggle-frame-fullscreen-non-native ()
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
(toggle-frame-maximized)

;; DocView Settings
(setq doc-view-continuous t
      doc-view-resolution 200)

;; Enable winner-mode
(add-hook 'after-init-hook #'winner-mode)

;; Don't blink the cursor
(blink-cursor-mode -1)

;; Improve dired
(require-package 'dired+)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;; Powerline and spaceline
(require-package 'powerline)
(require-package 'spaceline)
(require 'powerline)
(setq powerline-default-separator 'nil)
(require 'spaceline-config)
(setq ns-use-srgb-colorspace nil)
(spaceline-toggle-anzu-on)
(spaceline-spacemacs-theme)

;; Themes
(require-package 'solarized-theme)
(require-package 'zenburn-theme)
(require-package 'anti-zenburn-theme)
(require-package 'monokai-theme)
(load-theme 'zenburn t)

;; Vertical split eshell
(defun eshell-vertical ()
  "opens up a new shell in the directory associated with the current buffer's file."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (name (car (last (split-string parent "/" t)))))
    (split-window-right)
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))
    (eshell-send-input)))

;; Horizontal split eshell
(defun eshell-horizontal ()
  "opens up a new shell in the directory associated with the current buffer's file."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (name (car (last (split-string parent "/" t)))))
    (split-window-below)
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))
    (eshell-send-input)))

;;; God-mode - Absolutely necessary
(require-package 'god-mode)
(require 'god-mode)
(setq god-exempt-major-modes nil
      god-exempt-predicates nil)
(add-to-list 'god-exempt-major-modes 'dired-mode)
(global-set-key (kbd "<escape>") 'god-local-mode)
(global-set-key (kbd "C-c C-m") 'god-local-mode)
(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-2") 'split-window-below)
(global-set-key (kbd "C-x C-3") 'split-window-right)
(global-set-key (kbd "C-x C-0") 'delete-window)
(global-set-key (kbd "C-x C-r") 'switch-to-buffer)
(global-set-key (kbd "C-x C-k") 'kill-buffer)
(global-set-key (kbd "C-x C-t") 'eshell-here)

;;; Guide key
(require-package 'guide-key)
(setq guide-key/idle-delay 0.5
      guide-key/recursive-key-sequence-flag t
      guide-key/popup-window-position 'bottom
      guide-key/guide-key-sequence '("SPC" "\\" "g" "z" "[" "]"  "C" "C-x" "C-c" "C-h"))
(guide-key-mode 1)

;;; Evil - Vim emulation

;; Escape...escapes things
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
   In Delete Selection mode, if the mark is active, just deactivate it;
   then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(require-package 'evil)
(setq evil-want-C-u-scroll t)
(require 'evil)
(setq evil-default-cursor t
      evil-want-C-u-scroll t)
(evil-mode 1)
;; Specify evil initial states
(evil-set-initial-state 'git-commit-mode 'insert)
(evil-set-initial-state 'dired-mode 'emacs)
(evil-set-initial-state 'paradox-menu-mode 'emacs)
(evil-set-initial-state 'pdf-view-mode 'emacs)

;; Escape for everything
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key evil-emacs-state-map [escape] 'evil-exit-emacs-state)
;; Maps
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "w") 'split-window-horizontally)
(define-key evil-normal-state-map (kbd "Z") 'delete-other-windows)
(define-key evil-normal-state-map (kbd "Q") 'winner-undo)
(define-key evil-normal-state-map (kbd "gs") 'electric-newline-and-maybe-indent)
(define-key evil-normal-state-map (kbd "w") 'split-window-horizontally)
(define-key evil-normal-state-map (kbd "W") 'split-window-vertically)
(define-key evil-normal-state-map (kbd "U") 'undo-tree-visualize)
(define-key evil-normal-state-map (kbd "RET") 'evil-scroll-down)
(define-key evil-normal-state-map (kbd "BS") 'evil-scroll-up)
(define-key evil-normal-state-map (kbd "+") 'eshell-vertical)
(define-key evil-normal-state-map (kbd "-") 'eshell-horizontal)
(define-key evil-normal-state-map (kbd "]b") 'evil-next-buffer)
(define-key evil-normal-state-map (kbd "[b") 'evil-prev-buffer)
(define-key evil-normal-state-map (kbd "SPC q") 'evil-quit)
(define-key evil-normal-state-map (kbd "SPC w") 'save-buffer)
(define-key evil-normal-state-map (kbd "SPC k") 'kill-buffer)
(define-key evil-normal-state-map (kbd "SPC t") 'switch-to-buffer)
(define-key evil-normal-state-map (kbd "SPC u") 'universal-argument)
(define-key evil-normal-state-map (kbd "SPC z") 'toggle-frame-fullscreen-non-native)
(define-key evil-normal-state-map (kbd "SPC f") 'find-file)
(define-key evil-normal-state-map (kbd "SPC se") 'eval-buffer)
(define-key evil-normal-state-map (kbd "gos") 'flyspell-mode)
(define-key evil-normal-state-map (kbd "gol") 'whitespace-mode)
(define-key evil-normal-state-map (kbd "gon") 'linum-mode)
(define-key evil-normal-state-map (kbd "gow") 'toggle-truncate-lines)
(define-key evil-normal-state-map (kbd "goe") 'evil-show-marks)
(define-key evil-visual-state-map (kbd "SPC se") 'eval-region)
(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
(define-key evil-insert-state-map (kbd "C-u") 'universal-argument)

;; More useful arrow keys
(global-set-key (kbd "<left>") 'evil-window-left)
(global-set-key (kbd "<right>") 'evil-window-right)
(global-set-key (kbd "<up>") 'evil-window-up)
(global-set-key (kbd "<down>") 'evil-window-down)
(define-key evil-normal-state-map (kbd "<left>") 'evil-window-left)
(define-key evil-normal-state-map (kbd "<right>") 'evil-window-right)
(define-key evil-normal-state-map (kbd "<up>") 'evil-window-up)
(define-key evil-normal-state-map (kbd "<down>") 'evil-window-down)
(define-key evil-visual-state-map (kbd "<left>") 'evil-window-left)
(define-key evil-visual-state-map (kbd "<right>") 'evil-window-right)
(define-key evil-visual-state-map (kbd "<up>") 'evil-window-up)
(define-key evil-visual-state-map (kbd "<down>") 'evil-window-down)

;; Evil surround
(require-package 'evil-surround)
(global-evil-surround-mode 1)

;; '%' matching like vim
(require-package 'evil-matchit)
(define-key evil-normal-state-map "%" #'evilmi-jump-items)
(define-key evil-inner-text-objects-map "%" #'evilmi-text-object)
(define-key evil-outer-text-objects-map "%" #'evilmi-text-object)
(global-evil-matchit-mode 1)

;; Evil visual-star
(require-package 'evil-visualstar)
(global-evil-visualstar-mode)

;; Evil args
(require-package 'evil-args)
(define-key evil-inner-text-objects-map "," #'evil-inner-arg)
(define-key evil-outer-text-objects-map "," #'evil-outer-arg)
(define-key evil-normal-state-map "\C-j" #'evil-jump-out-args)

;; Play nice with god mode
(require-package 'evil-god-state)
(define-key evil-normal-state-map (kbd "\\") 'evil-execute-in-god-state)

;; Search count
(require-package 'evil-anzu)
(with-eval-after-load 'evil
  (require 'evil-anzu))

;; Jump lists like vim
(require-package 'evil-jumper)
(global-evil-jumper-mode 1)

;; Like vim-sneak
(require-package 'evil-snipe)
(require 'evil-snipe)
(evil-snipe-mode 1)
(setq evil-snipe-repeat-keys t
      evil-snipe-scope 'visible
      evil-snipe-repeat-scope 'whole-visible
      evil-snipe-enable-highlight t
      evil-snipe-enable-incremental-highlight t)

;; Evil commentary
(require-package 'evil-commentary)
(evil-commentary-mode)

;; Evil exchange
(require-package 'evil-exchange)
(evil-exchange-install)

;;; Evil text objects - Courtesy PythonNut
;; evil block indentation textobject for Python
(defun evil-indent--current-indentation ()
  "Return the indentation of the current line. Moves point."
  (buffer-substring-no-properties (point-at-bol)
    (progn (back-to-indentation)
      (point))))
(defun evil-indent--block-range (&optional point)
  "Return the point at the begin and end of the text block "
  ;; there are faster ways to mark the entire file
  ;; so assume the user wants a block and skip to there
  (while (and (string-empty-p
                (evil-indent--current-indentation))
           (not (eobp)))
    (forward-line))
  (cl-flet* ((empty-line-p ()
               (string-match "^[[:space:]]*$"
                 (buffer-substring-no-properties
                   (line-beginning-position)
                   (line-end-position))))
              (line-indent-ok (indent)
                (or (<= (length indent)
                      (length (evil-indent--current-indentation)))
                  (empty-line-p))))
    (let ((indent (evil-indent--current-indentation)) start begin end)
      ;; now skip ahead to the Nth block with this indentation
      (dotimes (index (or last-prefix-arg 0))
        (while (and (line-indent-ok) (not (eobp))) (forward-line))
        (while (or (line-indent-ok indent) (eobp)) (forward-line)))
      (save-excursion
        (setq start (goto-char (or point (point))))
        (while (and (line-indent-ok indent) (not (bobp)))
          (setq begin (point))
          (forward-line -1))
        (goto-char start)
        (while (and (line-indent-ok indent) (not (eobp)))
          (setq end (point))
          (forward-line))
        (goto-char end)
        (while (empty-line-p)
          (forward-line -1)
          (setq end (point)))
        (list begin end)))))
(evil-define-text-object evil-indent-i-block (&optional count beg end type)
  "Text object describing the block with the same indentation as the current line."
  (let ((range (evil-indent--block-range)))
    (evil-range (first range) (second range) 'line)))
(evil-define-text-object evil-indent-a-block (&optional count beg end type)
  "Text object describing the block with the same indentation as the current line and the line above."
  :type line
  (let ((range (evil-indent--block-range)))
    (evil-range (save-excursion
                  (goto-char (first (evil-indent--block-range)))
                  (forward-line -1)
                  (point-at-bol))
      (second range) 'line)))
(evil-define-text-object evil-indent-a-block-end (count &optional beg end type)
  "Text object describing the block with the same indentation as the current line and the lines above and below."
  :type line
  (let ((range (evil-indent--block-range)))
    (evil-range (save-excursion
                  (goto-char (first range))
                  (forward-line -1)
                  (point-at-bol))
      (save-excursion
        (goto-char (second range))
        (forward-line 1)
        (point-at-eol))
      'line)))
(define-key evil-inner-text-objects-map "i" #'evil-indent-i-block)
(define-key evil-outer-text-objects-map "i" #'evil-indent-a-block)
(define-key evil-outer-text-objects-map "I" #'evil-indent-a-block-end)

;; Sentence and range
(evil-define-text-object evil-i-line-range (count &optional beg end type)
  "Text object describing the block with the same indentation as the current line."
  :type line
  (save-excursion
    (let ((beg (evil-avy-jump-line-and-revert))
           (end (evil-avy-jump-line-and-revert)))
      (if (> beg end)
        (evil-range beg end #'line)
        (evil-range end beg #'line)))))
(define-key evil-inner-text-objects-map "r" #'evil-i-line-range)
(define-key evil-inner-text-objects-map "s" #'evil-inner-sentence)
(define-key evil-outer-text-objects-map "s" #'evil-a-sentence)

;; Entire buffer text object
(evil-define-text-object evil-i-entire-buffer (count &optional ben end type)
  "Text object describing the entire buffer excluding empty lines at the end"
  :type line
  (evil-range (point-min) (save-excursion
                            (goto-char (point-max))
                            (skip-chars-backward " \n\t")
                            (point)) 'line))

(evil-define-text-object evil-an-entire-buffer (count &optional beg end type)
  "Text object describing the entire buffer"
  :type line
  (evil-range (point-min) (point-max) 'line))
(define-key evil-inner-text-objects-map "a" #'evil-i-entire-buffer)
(define-key evil-outer-text-objects-map "a" #'evil-an-entire-buffer)

;;; Evil operators
;; Macros on all objects
(evil-define-operator evil-macro-on-all-lines (beg end &optional arg)
  (evil-with-state
    (evil-normal-state)
    (goto-char end)
    (evil-visual-state)
    (goto-char beg)
    (evil-ex-normal (region-beginning) (region-end)
      (concat "@"
        (single-key-description
          (read-char "What macro?"))))))
(define-key evil-operator-state-map "g@" #'evil-macro-on-all-lines)
(define-key evil-normal-state-map "g@" #'evil-macro-on-all-lines)

;; Evil text object proper sentence with abbreviation
(require-package 'sentence-navigation)
(define-key evil-normal-state-map ")" 'sentence-nav-evil-forward)
(define-key evil-normal-state-map "(" 'sentence-nav-evil-backward)
(define-key evil-normal-state-map "g)" 'sentence-nav-evil-forward-end)
(define-key evil-normal-state-map "g(" 'sentence-nav-evil-backward-end)
(define-key evil-outer-text-objects-map "s" 'sentence-nav-evil-outer-sentence)
(define-key evil-inner-text-objects-map "s" 'sentence-nav-evil-inner-sentence)

;;; Navigation

;; Flx
(require-package 'flx)

;; Flx with helm and company
(require-package 'helm-flx)
(helm-flx-mode +1)
(require-package 'company-flx)

;; No backups
(setq make-backup-files nil
      auto-save-default nil)

;; Ediff plain window and vertical
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;; Remote file navigation
(require-package 'tramp)
(setq tramp-ssh-controlmaster-options "ssh")

;; Very large file viewing
(require-package 'vlf)

;;; Avy
(require-package 'avy)
(define-key evil-normal-state-map (kbd "SPC h") 'avy-goto-line)
(define-key evil-visual-state-map (kbd "SPC h") 'avy-goto-line)

;; Smex
(require-package 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-x C-m") 'smex)
(define-key evil-normal-state-map (kbd "SPC d") 'smex)
(define-key evil-visual-state-map (kbd "SPC d") 'smex)

;; Swiper and Swiper helm - best I-Search I found
(require-package 'swiper)
(define-key evil-normal-state-map (kbd "SPC SPC") 'swiper)

;;; Helm
(require-package 'helm)
(setq helm-split-window-in-side-p t
      helm-autoresize-max-height 30
      helm-autoresize-min-height 30
      helm-M-x-fuzzy-match t
      helm-bookmark-show-location t
      helm-buffers-fuzzy-matching t
      helm-completion-in-region-fuzzy-match t
      helm-file-cache-fuzzy-match t
      helm-imenu-fuzzy-match t
      helm-mode-fuzzy-match t
      helm-locate-fuzzy-match t
      helm-quick-update t
      helm-recentf-fuzzy-match t
      helm-semantic-fuzzy-match t
      helm-echo-input-in-header-line t)
;; Search integration
(when (executable-find "ack-grep")
  (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
        helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))
(when (executable-find "ag-grep")
  (setq helm-grep-default-command "ag-grep -Hn --no-group --no-color %e %p %f"
        helm-grep-default-recurse-command "ag-grep -H --no-group --no-color %e %p %f"))
(helm-mode 1)
(helm-autoresize-mode 1)
;; Add helm sources
(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
;; Helm persistent action
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
;; Maps
(global-set-key (kbd "C-s") 'helm-occur)
(global-set-key (kbd "C-r") 'helm-occur)
(define-key evil-normal-state-map (kbd "SPC x") 'helm-apropos)
(define-key evil-normal-state-map (kbd "SPC r") 'helm-recentf)
(define-key evil-normal-state-map (kbd "SPC `") 'helm-bookmarks)
(define-key evil-normal-state-map (kbd "SPC .") 'helm-resume)
(define-key evil-normal-state-map (kbd "SPC y") 'helm-show-kill-ring)
(define-key evil-normal-state-map (kbd "t") 'helm-semantic-or-imenu)
(define-key evil-normal-state-map (kbd "K") 'helm-man-woman)
(define-key evil-normal-state-map (kbd "SPC 9") 'helm-google-suggest)
(define-key evil-normal-state-map (kbd "SPC 3") 'helm-calcul-expression)
(define-key evil-insert-state-map (kbd "C-l") 'helm-M-x)

;; Hide minibuffer when using helm input header line
(defun helm-hide-minibuffer-maybe ()
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                              `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))
(add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

;;; Ag
(require-package 'helm-ag)
(setq helm-ag-base-command "ag --nocolor --nogroup --ignore-case"
      helm-ag-command-option "--all-text"
      helm-ag-insert-at-point 'symbol)
(define-key evil-normal-state-map (kbd "SPC e") 'helm-do-ag-project-root)
(define-key evil-visual-state-map (kbd "SPC e") 'helm-do-ag-project-root)
(define-key evil-normal-state-map (kbd "SPC b") 'helm-do-ag-buffers)

;; ;; Helm swoop
(require-package 'helm-swoop)
(define-key evil-normal-state-map (kbd "SPC i") 'helm-swoop)
(define-key evil-visual-state-map (kbd "SPC i") 'helm-swoop)

;; Helm describe-bindings
(require-package 'helm-descbinds)

;; Helm to open colorschemes
(require-package 'helm-themes)
(define-key evil-normal-state-map (kbd "SPC v") 'helm-themes)

;; Helm for citations
(require-package 'helm-bibtex)

;; Helm unicode
(require-package 'helm-unicode)
(define-key evil-insert-state-map (kbd "C-k") 'helm-unicode)

;; Dictionary
(require-package 'helm-flyspell)
(define-key evil-insert-state-map (kbd "C-d") 'helm-flyspell-correct)

;; Browse offline documentation - code courtesy http://jwintz.me/blog/
(require-package 'helm-dash)
(setq helm-dash-browser-func 'eww
      helm-dash-docsets-path "~/.emacs.d/docsets"
      helm-dash-min-length 2)
(defun custom-dash-docset-path (docset)
  (if (string= docset "OpenGL_2")
      (concat (concat helm-dash-docsets-path "/") "OpenGL_2.docset")
    (if (string= docset "OpenGL_3")
        (concat (concat helm-dash-docsets-path "/") "OpenGL_3.docset")
      (if (string= docset "OpenGL_4")
          (concat (concat helm-dash-docsets-path "/") "OpenGL_4.docset")
        (if (string= docset "Emacs_Lisp")
            (concat (concat helm-dash-docsets-path "/") "Emacs_Lisp.docset")
          (concat
           (concat
            (concat
             (concat helm-dash-docsets-path "/")
             (nth 0 (split-string docset "_")))) ".docset"))))))
(defun custom-dash-install (docset)
  (unless (file-exists-p (custom-dash-docset-path docset))
    (helm-dash-install-docset docset)))
(custom-dash-install "C++")
(custom-dash-install "Boost")
(custom-dash-install "C")
(custom-dash-install "Python_2")
(custom-dash-install "NumPy")
(custom-dash-install "SciPy")
;; (custom-dash-install "MatPlotLib")
(custom-dash-install "Julia")
(custom-dash-install "R")
(custom-dash-install "LaTeX")
(custom-dash-install "Markdown")
(custom-dash-install "Java_SE8")
(custom-dash-install "HTML")
(custom-dash-install "Bootstrap_4")
(custom-dash-install "CSS")
(custom-dash-install "JavaScript")
(custom-dash-install "jQuery")
(custom-dash-install "Emacs_Lisp")
(define-key evil-normal-state-map (kbd "SPC 7") 'helm-dash-activate-docset)
(define-key evil-normal-state-map (kbd "SPC 1") 'helm-dash)

;; System processes
(require-package 'helm-proc)

;;; Manage external services
(require-package 'prodigy)

;;; Projectile
(require-package 'projectile)
(setq projectile-enable-caching t
      projectile-require-project-root nil
      projectile-mode-line '(:eval (format " [%s]" (projectile-project-name))))
(projectile-global-mode)

;; Helm-projectile
(require-package 'helm-projectile)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
;;Maps
(define-key evil-normal-state-map (kbd "SPC p") 'helm-projectile)
(define-key evil-normal-state-map (kbd "SPC 6") 'projectile-switch-project)
(define-key evil-normal-state-map (kbd "SPC aa") 'helm-projectile-find-other-file)

;; Project explorer
(require-package 'project-explorer)
(add-to-list 'evil-emacs-state-modes 'project-explorer-mode)
(define-key evil-normal-state-map (kbd "SPC n") 'project-explorer-toggle)

;;; Rtags - Awesome for C/C++
(require-package 'rtags)

;;; Cmake ide
(require-package 'cmake-ide)

;;; GTags
(require-package 'ggtags)
(ggtags-mode)
;; Add exec-path for Gtags
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/Cellar/global/6.5/bin"))
(setq exec-path (append exec-path '("/usr/local/Cellar/global/6.5/bin")))
(setq-local imenu-create-index-function #'ggtags-build-imenu-index)
(define-key evil-normal-state-map (kbd "SPC as") 'ggtags-create-tags)

;; Helm Gtags
(require-package 'helm-gtags)
(helm-gtags-mode 1)
;; Tags using appropriate methods
(define-key evil-normal-state-map (kbd "T") 'helm-gtags-select)
(define-key evil-normal-state-map (kbd "R") 'helm-gtags-find-rtag)
(define-key evil-normal-state-map (kbd "SPC jj") 'helm-gtags-dwim)

;;; Interact with OS services
;; Jabber
(require-package 'jabber)
(setq jabber-history-enabled t
      jabber-use-global-history nil
      jabber-backlog-number 40
      jabber-backlog-days 30)

;; Google under point
(require-package 'google-this)
(define-key evil-visual-state-map (kbd "SPC 9") 'google-this)

;; Helm itunes
(require-package 'helm-itunes)
(define-key evil-normal-state-map (kbd "SPC 2") 'helm-itunes)

;; Helm spotify
(require-package 'helm-spotify)
(define-key evil-normal-state-map (kbd "SPC 0") 'helm-spotify)

;; Evernote with geeknote
(require-package 'geeknote)

;; Stackexchange
(require-package 'sx)

;;; Text editing

;; Indenting
(defun my-generate-tab-stops (&optional width max)
  "Return a sequence suitable for `tab-stop-list'."
  (let* ((max-column (or max 200))
         (tab-width (or width tab-width))
         (count (/ max-column tab-width)))
    (number-sequence tab-width (* tab-width count) tab-width)))
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)
(setq tab-stop-list (my-generate-tab-stops))

;; Narrow to region
(put 'narrow-to-region 'disabled nil)

;; Spell check
(require 'flyspell)
(add-hook 'text-mode-hook 'flyspell-mode)
;; move point to previous error
;; based on code by hatschipuh at
;; http://emacs.stackexchange.com/a/14912/2017
(defun flyspell-goto-previous-error (arg)
  "Go to arg previous spelling error."
  (interactive "p")
  (while (not (= 0 arg))
    (let ((pos (point))
          (min (point-min)))
      (if (and (eq (current-buffer) flyspell-old-buffer-error)
               (eq pos flyspell-old-pos-error))
          (progn
            (if (= flyspell-old-pos-error min)
                ;; goto beginning of buffer
                (progn
                  (message "Restarting from end of buffer")
                  (goto-char (point-max)))
              (backward-word 1))
            (setq pos (point))))
      ;; seek the next error
      (while (and (> pos min)
                  (let ((ovs (overlays-at pos))
                        (r '()))
                    (while (and (not r) (consp ovs))
                      (if (flyspell-overlay-p (car ovs))
                          (setq r t)
                        (setq ovs (cdr ovs))))
                    (not r)))
        (backward-word 1)
        (setq pos (point)))
      ;; save the current location for next invocation
      (setq arg (1- arg))
      (setq flyspell-old-pos-error pos)
      (setq flyspell-old-buffer-error (current-buffer))
      (goto-char pos)
      (if (= pos min)
          (progn
            (message "No more miss-spelled word!")
            (setq arg 0))
        (forward-word)))))
(define-key evil-normal-state-map (kbd "]s") 'flyspell-goto-next-error)
(define-key evil-normal-state-map (kbd "[s") 'flyspell-goto-previous-error)

;; Highlight stuff
(require-package 'volatile-highlights)
(require 'volatile-highlights)
(volatile-highlights-mode t)

;; Aggressive indent
(require-package 'aggressive-indent)
(add-hook 'c-mode-hook 'aggressive-indent-mode)
(add-hook 'c++-mode-hook 'aggressive-indent-mode)

;; Smart tabs
(require-package 'smart-tab)
(global-smart-tab-mode)

;;; Multiple cursors
(require-package 'evil-mc)
(global-evil-mc-mode 1)

;;; Company
(require-package 'company)
(global-company-mode)
(with-eval-after-load 'company
  (company-flx-mode +1))
(add-to-list 'company-backends 'company-files)
(setq company-idle-delay 0
      company-minimum-prefix-length 1
      company-require-match 0
      company-selection-wrap-around t
      company-dabbrev-downcase nil)
;; Maps
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map [tab] 'company-complete-common-or-cycle)

;; Company C headers
(require-package 'company-c-headers)
(add-to-list 'company-backends 'company-c-headers)

;; Irony mode for C++
(require-package 'irony)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; Company irony
(require-package 'company-irony)
(require-package 'company-irony-c-headers)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

;; Play well with FCI mode
(defvar-local company-fci-mode-on-p nil)
(defun company-turn-off-fci (&rest ignore)
  (when (boundp 'fci-mode)
    (setq company-fci-mode-on-p fci-mode)
    (when fci-mode (fci-mode -1))))
(defun company-maybe-turn-on-fci (&rest ignore)
  (when company-fci-mode-on-p (fci-mode 1)))
(add-hook 'company-completion-started-hook 'company-turn-off-fci)
(add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
(add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)

;; Support auctex
(require-package 'company-auctex)

;; Support Go
(require-package 'company-go)

;; Support inf-ruby
(require-package 'company-inf-ruby)

;; Support tern
(require-package 'company-tern)

;; Support web mode
(require-package 'company-web)

;;; YASnippet
(require-package 'yasnippet)
;; Add yasnippet support for all company backends
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")
(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))
(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
;; Prompt using ido/normal in that order if helm-yasnippet is not there
(setq yas-prompt-functions '(yas-ido-prompt
                             yas-completing-prompt))
;; Disable in shell
(defun force-yasnippet-off ()
  (yas-minor-mode -1)
  (setq yas-dont-activate t))
(add-hook 'term-mode-hook 'force-yasnippet-off)
(add-hook 'shell-mode-hook 'force-yasnippet-off)
(yas-global-mode)

;; Helm yasnippet
(require-package 'helm-c-yasnippet)
(define-key evil-insert-state-map (kbd "C-j") 'helm-yas-complete)

;;; Language and Syntax

;; ESS - Emacs speaks statistics
(require-package 'ess)
(require 'ess-site)
(add-to-list 'auto-mode-alist '("\\.R$" . R-mode))
(define-key evil-normal-state-map (kbd "SPC sr") 'ess-eval-buffer)
(define-key evil-visual-state-map (kbd "SPC sr") 'ess-eval-region)

;; Lisp
(require-package 'paredit)

;; Slime
(require-package 'slime)

;; Cider
(require-package 'cider)

;; Geiser for scheme-mode
(require-package 'geiser)

;; Haskell
(require-package 'haskell-mode)

;; Markdown
(require-package 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Pandoc mode
(require-package 'pandoc-mode)
(add-hook 'markdown-mode-hook 'pandoc-mode)

;; Jedi+Company for Python mode
(require-package 'jedi)
(require-package 'company-jedi)
(defun my/python-mode-hook ()
  (jedi:setup)
  (add-to-list 'company-backends 'company-jedi))
(add-hook 'python-mode-hook 'my/python-mode-hook)
(define-key evil-normal-state-map (kbd "SPC jk") 'jedi:goto-definition)
(define-key evil-normal-state-map (kbd "SPC sp") 'python-shell-send-buffer)
(define-key evil-visual-state-map (kbd "SPC sp") 'python-shell-send-region)

;; Cython mode
(require-package 'cython-mode)

;; Virtualenv for python
(require-package 'virtualenvwrapper)
(setq venv-location "~/Py34/")

;; LaTeX-mode
(require-package 'auctex)
(require-package 'auctex-latexmk)

;; Web mode
(require-package 'web-mode)
(add-hook 'html-mode-hook 'web-mode)

;; JavaScript
(require-package 'js2-mode)
(require-package 'skewer-mode)

;; Applescript
(require-package 'applescript-mode)
(add-to-list 'auto-mode-alist '("\\.scpt\\'" . applescript-mode))

;; YAML mode
(require-package 'yaml-mode)

;; Ruby
(require-package 'inf-ruby)

;; Editing my gitconfig
(require-package 'gitconfig-mode)

;; MATLAB mode
(require-package 'matlab-mode)
(eval-after-load 'matlab
  '(add-to-list 'matlab-shell-command-switches "-nosplash"))
(setq matlab-shell-command "/Applications/MATLAB_R2014a.app/bin/matlab"
      matlab-indent-function t)
(define-key evil-normal-state-map (kbd "SPC sm") 'matlab-shell-run-cell)
(define-key evil-visual-state-map (kbd "SPC sm") 'matlab-shell-run-region)

;; Sage
(require-package 'sage-shell-mode)
(setq sage-shell:sage-executable "/Applications/Sage-6.8.app/Contents/Resources/sage/sage"
      sage-shell:input-history-cache-file "~/.emacs.d/.sage_shell_input_history"
      sage-shell-sagetex:auctex-command-name "LaTeX"
      sage-shell-sagetex:latex-command "latexmk")

;; SQL
(require-package 'emacsql)
(require-package 'emacsql-mysql)
(require-package 'emacsql-sqlite)

;; Go mode
(require-package 'go-mode)

;; Java
(require-package 'emacs-eclim)
(require 'eclimd)
(require 'company-emacs-eclim)
(setq eclim-executable (or (executable-find "eclim") "/Applications/Eclipse/Eclipse.app/Contents/Eclipse/eclim")
      eclimd-executable (or (executable-find "eclimd") "/Applications/Eclipse/Eclipse.app/Contents/Eclipse/eclimd")
      eclimd-wait-for-process nil
      eclimd-default-workspace "~/Documents/workspace/eclipse/")
(add-hook 'java-mode-hook 'eclim-mode)
(company-emacs-eclim-setup)

;;; Flycheck
(require-package 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(define-key evil-normal-state-map (kbd "]l") 'flycheck-next-error)
(define-key evil-normal-state-map (kbd "[l") 'flycheck-previous-error)
(define-key evil-normal-state-map (kbd "]L") 'flycheck-last-checker)
(define-key evil-normal-state-map (kbd "[L") 'flycheck-first-error)
(define-key evil-normal-state-map (kbd "SPC l") 'flycheck-list-errors)

;; Irony for flycheck
(require-package 'flycheck-irony)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;;; Org mode
(define-key evil-normal-state-map (kbd "SPC c") 'org-capture)
(define-key evil-normal-state-map (kbd "SPC o") 'org-agenda)
(define-key evil-visual-state-map (kbd "SPC c") 'org-capture)
(define-key evil-visual-state-map (kbd "SPC o") 'org-agenda)
(define-key evil-normal-state-map (kbd "SPC -") 'org-edit-src-code)
(define-key evil-normal-state-map (kbd "SPC =") 'org-edit-src-exit)
(define-key evil-normal-state-map (kbd "SPC 4") 'org-toggle-latex-fragment)
(define-key evil-normal-state-map (kbd "SPC 5") 'org-toggle-inline-images)
(define-key evil-normal-state-map (kbd "]h") 'org-metaright)
(define-key evil-normal-state-map (kbd "[h") 'org-metaleft)
(define-key evil-normal-state-map (kbd "]j") 'org-metadown)
(define-key evil-normal-state-map (kbd "[j") 'org-metaup)

;; Turns the next page in adjoining pdf-tools pdf
(defun other-pdf-next ()
  "Turns the next page in adjoining PDF file"
  (interactive)
  (other-window 1)
  (doc-view-next-page)
  (other-window 1))
;; Turns the previous page in adjoining pdf
(defun other-pdf-previous ()
  "Turns the previous page in adjoining PDF file"
  (interactive)
  (other-window 1)
  (doc-view-previous-page)
  (other-window 1))
(define-key evil-normal-state-map (kbd "]p") 'other-pdf-next)
(define-key evil-normal-state-map (kbd "[p") 'other-pdf-previous)

(setq org-directory "~/Dropbox/notes/"
      org-completion-use-ido t
      org-default-notes-file "~/Dropbox/notes/inbox.org"
      ;; Indent
      org-startup-indented t
      ;; Source code
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      ;; Quotes
      org-export-with-smart-quotes t
      ;; Citations
      org-latex-to-pdf-process '("pdflatex %f" "biber %b" "pdflatex %f" "pdflatex %f"))

;; Tags with fast selection keys
(setq org-tag-alist (quote ((:startgroup)
                            ("@errand" . ?e)
                            ("@office" . ?o)
                            ("@home" . ?H)
                            ("@farm" . ?f)
                            (:endgroup)
                            ("WAITING" . ?w)
                            ("HOLD" . ?h)
                            ("PERSONAL" . ?P)
                            ("WORK" . ?W)
                            ("FARM" . ?F)
                            ("ORG" . ?O)
                            ("NORANG" . ?N)
                            ("crypt" . ?E)
                            ("NOTE" . ?n)
                            ("CANCELLED" . ?c)
                            ("FLAGGED" . ??))))

;; Better bullets
(require-package 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Pomodoro
(require-package 'org-pomodoro)

;; Babel for languages
(require-package 'babel)
(setq org-confirm-babel-evaluate nil)
;; Org load languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (dot . t)
   (ditaa . t)
   (latex . t)
   (gnuplot . t)
   (sh . t)
   (C . t)
   (R . t)
   (octave . t)
   (matlab . t)
   (python . t)))
(require-package 'ob-ipython)

;; Export using reveal and impress-js
(require-package 'ox-reveal)
(require-package 'ox-impress-js)

;; Restructred text and pandoc
(require-package 'ox-rst)
(require-package 'ox-pandoc)

;; Trello
(require-package 'org-trello)

;;; Version control

;; Magit
(require-package 'magit)
(add-to-list 'god-exempt-major-modes 'magit-mode)
(define-key evil-normal-state-map (kbd "SPC g") 'magit-status)
(define-key evil-normal-state-map (kbd "gb") 'magit-blame)
(define-key evil-normal-state-map (kbd "gz") 'magit-blame-quit)
(define-key evil-normal-state-map (kbd "zj") 'magit-blame-next-chunk)
(define-key evil-normal-state-map (kbd "zn") 'magit-blame-next-chunk-same-commit)
(define-key evil-normal-state-map (kbd "zk") 'magit-blame-previous-chunk)
(define-key evil-normal-state-map (kbd "zp") 'magit-blame-previous-chunk-same-commit)
(define-key evil-normal-state-map (kbd "gy") 'magit-blame-copy-hash)
(define-key evil-normal-state-map (kbd "gt") 'magit-blame-toggle-headings)

;; Diff-hl
(require-package 'diff-hl)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
(add-hook 'prog-mode-hook 'diff-hl-mode)
(add-hook 'html-mode-hook 'diff-hl-mode)
(add-hook 'text-mode-hook 'diff-hl-mode)
(add-hook 'org-mode-hook 'diff-hl-mode)
(diff-hl-margin-mode)
(diff-hl-flydiff-mode)
(define-key evil-normal-state-map (kbd "]c") 'diff-hl-next-hunk)
(define-key evil-normal-state-map (kbd "[c") 'diff-hl-previous-hunk)
(define-key evil-normal-state-map (kbd "gh") 'diff-hl-revert-hunk)

;; Git time-machine
(require-package 'git-timemachine)
(define-key evil-normal-state-map (kbd "gl") 'git-timemachine)
(define-key evil-normal-state-map (kbd "gr") 'git-timemachine-kill-revision)
(define-key evil-normal-state-map (kbd "]q") 'git-timemachine-show-next-revision)
(define-key evil-normal-state-map (kbd "[q") 'git-timemachine-show-previous-revision)
(define-key evil-normal-state-map (kbd "]Q") 'git-timemachine-show-nth-revision)
(define-key evil-normal-state-map (kbd "[Q") 'git-timemachine-show-current-revision)

;;; REPL

;; Interact with Tmux
(require-package 'emamux)
(setq emamux:completing-read-type 'helm)
(define-key evil-normal-state-map (kbd "SPC sc") 'emamux:send-command)

;; Make the compilation window automatically disapper from enberg on #emacs
(setq compilation-finish-functions
      (lambda (buf str)
        (if (null (string-match ".*exited abnormally.*" str))
            ;;no errors, make the compilation window go away in a few seconds
            (progn
              (run-at-time
               "2 sec" nil 'delete-windows-on
               (get-buffer-create "*compilation*"))
              (message "No Compilation Errors!")))))

;; Helm make
(require-package 'helm-make)
(define-key evil-normal-state-map (kbd "SPC m") 'helm-make)

;; Quickrun
(require-package 'quickrun)
(define-key evil-normal-state-map (kbd "SPC 8") 'helm-quickrun)

;;; Eshell
(setq eshell-glob-case-insensitive t
      eshell-scroll-to-bottom-on-input 'this
      eshell-buffer-shorthand t
      eshell-history-size 1024
      eshell-cmpl-ignore-case t
      eshell-last-dir-ring-size 512)

;; Aliases
(setq eshell-aliases-file (concat user-emacs-directory ".eshell-aliases"))

;; Plan9
(require 'eshell)
(require 'em-smart)
(setq eshell-where-to-jump 'begin
      eshell-review-quick-commands nil
      eshell-smart-space-goes-to-end t)

;;; Project buffer management - Perspective
(require-package 'perspective)
(global-set-key (kbd "C-c C-p C-s") 'persp-switch)
(global-set-key (kbd "C-c C-p C-k") 'persp-remove-buffer)
(global-set-key (kbd "C-c C-p C-c") 'persp-kill)
(global-set-key (kbd "C-c C-p C-r") 'persp-rename)
(global-set-key (kbd "C-c C-p C-a") 'persp-add-buffer)
(global-set-key (kbd "C-c C-p C-A") 'persp-set-buffer)
(global-set-key (kbd "C-c C-p C-i") 'persp-import)
(global-set-key (kbd "C-c C-p C-n") 'persp-next)
(global-set-key (kbd "C-c C-p C-p") 'persp-prev)

;;; Wrap up

;; Highlight the cursor line
(global-hl-line-mode 1)

;; GDB
(setq gdb-many-windows t
      gdb-show-main t)

;; Fill column indicator
(require-package 'fill-column-indicator)
(setq fci-rule-width 5
      fci-rule-column 99)
(define-key evil-normal-state-map (kbd "gox") 'fci-mode)

;; Column enforce column that highlights if I go over 100 characters
;; I try to stick within 80 characters but, frankly, I prefer 100.
;; Hence a compromise
(require-package 'column-enforce-mode)
(require 'column-enforce-mode)
(setq column-enforce-column 79)
(global-column-enforce-mode)
(define-key evil-normal-state-map (kbd "goc") 'column-enforce-mode)

;; Which function mode
(which-function-mode 1)

;; Enable subword mode
(global-subword-mode)

;; Enable smartparens-mode
(require-package 'smartparens)
(smartparens-global-mode)

;; Hide/Show mode
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; Column number mode
(column-number-mode)

;; Display time
(define-key evil-normal-state-map (kbd "got") 'display-time-mode)

;; Display battery
(define-key evil-normal-state-map (kbd "gob") 'display-battery-mode)

;; Nice writing environment
(require-package 'olivetti)

;; Hyperfocus
(require-package 'focus)

;; Restart Emacs from Emacs
(require-package 'restart-emacs)

;;; Clean mode-line
(defvar mode-line-cleaner-alist
  `((smartparens-mode . "")
    (guide-key-mode . "")
    (god-local-mode . " ψ")
    (evil-snipe-mode . "")
    (evil-snipe-local-mode . "")
    (evil-mc-mode . "")
    (evil-commentary-mode . "")
    (helm-mode . "")
    (helm-gtags-mode . "")
    (volatile-highlights-mode . "")
    (flycheck-mode . "")
    (flyspell-mode . " $")
    (aspell-mode . " $")
    (ispell-mode . " $")
    (ggtags-mode . "")
    (aggressive-indent-mode . "")
    (column-enforce-mode . "")
    (smart-tab-mode . "")
    (company-mode . " ς")
    (irony-mode . " Γ")
    (yas-minor-mode . " γ")
    (eldoc-mode . "")
    (org-indent-mode . "")
    (hs-minor-mode . "")
    (auto-complete-mode . "")
    (abbrev-mode . "")
    (undo-tree-mode . "")
    ;; Major modes
    (lisp-interaction-mode . ":λ:")
    (hi-lock-mode . "")
    (markdown-mode . ":MD:")
    (python-mode . ":PY:")
    (emacs-lisp-mode . ":EL:")
    (org-mode . ":ORG:")
    (nxhtml-mode . ":NX:"))
  "Alist for `clean-mode-line'.
When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")
(defun clean-mode-line ()
  (interactive)
  (loop for cleaner in mode-line-cleaner-alist
        do (let* ((mode (car cleaner))
                  (mode-str (cdr cleaner))
                  (old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
               (setcar old-mode-str mode-str))
             ;; major mode
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))
(add-hook 'after-change-major-mode-hook 'clean-mode-line)

(server-start)
;;; .emacs ends here
