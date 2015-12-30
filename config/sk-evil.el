;;; Evil configuration
(sk/require-package 'evil)
(setq evil-default-cursor t
      evil-want-C-u-scroll t
      evil-want-Y-yank-to-eol t)
(evil-mode 1)

;; Specify evil initial states
(evil-set-initial-state 'dired-mode 'emacs)
(evil-set-initial-state 'paradox-menu-mode 'emacs)
(evil-set-initial-state 'calendar-mode 'emacs)
(evil-set-initial-state 'diff-mode 'emacs)
(evil-set-initial-state 'ag-mode 'normal)

;; Escape for everything
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)

;;; Maps

;; Evil maps
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "SPC q") 'evil-quit)
(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

;; Emacs functionality maps
(define-key evil-normal-state-map (kbd "t") 'imenu)
(define-key evil-normal-state-map (kbd "Z") 'delete-other-windows)
(define-key evil-normal-state-map (kbd "K") 'man)
(define-key evil-normal-state-map (kbd "\\") 'universal-argument)
(define-key evil-normal-state-map (kbd "gs") 'electric-newline-and-maybe-indent)
(define-key evil-normal-state-map (kbd "gl") 'browse-url-at-point)
(define-key evil-normal-state-map (kbd "SPC w") 'save-buffer)
(define-key evil-normal-state-map (kbd "SPC k") 'kill-buffer)
(define-key evil-normal-state-map (kbd "SPC f") 'find-file)
(define-key evil-normal-state-map (kbd "SPC [") 'widen)
(define-key evil-normal-state-map (kbd "SPC DEL") 'whitespace-cleanup)
(define-key evil-normal-state-map (kbd "SPC ,") 'describe-bindings)
(define-key evil-normal-state-map (kbd "SPC \\") 'toggle-input-method)
(define-key evil-visual-state-map (kbd "SPC ]") 'narrow-to-region)

;; Macros
(define-key evil-normal-state-map (kbd "H") 'kmacro-start-macro)
(define-key evil-normal-state-map (kbd "L") 'kmacro-end-macro)
(define-key evil-normal-state-map (kbd "M") 'sk/hydra-of-macros/body)

;;; Evil packages

;; Evil surround
(sk/require-package 'evil-surround)
(global-evil-surround-mode 1)

;; Visual selection '*'
(sk/require-package 'evil-visualstar)
(global-evil-visualstar-mode 1)

;; '%' matching like vim
(sk/require-package 'evil-matchit)
(define-key evil-normal-state-map "%" #'evilmi-jump-items)
(define-key evil-inner-text-objects-map "%" #'evilmi-text-object)
(define-key evil-outer-text-objects-map "%" #'evilmi-text-object)
(global-evil-matchit-mode 1)

;; Evil args
(sk/require-package 'evil-args)
(define-key evil-inner-text-objects-map "," #'evil-inner-arg)
(define-key evil-outer-text-objects-map "," #'evil-outer-arg)
(define-key evil-normal-state-map "\C-j" #'evil-jump-out-args)

;; Jump lists like vim
(sk/require-package 'evil-jumper)
(global-evil-jumper-mode 1)

;; Evil commentary
(sk/require-package 'evil-commentary)
(defun sk/diminish-evil-commentary ()
  (interactive)
  (diminish 'evil-commentary-mode ""))
(add-hook 'evil-commentary-mode-hook 'sk/diminish-evil-commentary)
(evil-commentary-mode)

;; Evil exchange
(sk/require-package 'evil-exchange)
(evil-exchange-install)

;; Increment and decrement numbers like vim
(sk/require-package 'evil-numbers)
(define-key evil-normal-state-map (kbd "C-k") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-j") 'evil-numbers/dec-at-pt)

;; Evil text object proper sentence with abbreviation
(sk/require-package 'sentence-navigation)
(define-key evil-normal-state-map ")" 'sentence-nav-evil-forward)
(define-key evil-normal-state-map "(" 'sentence-nav-evil-backward)
(define-key evil-normal-state-map "g)" 'sentence-nav-evil-forward-end)
(define-key evil-normal-state-map "g(" 'sentence-nav-evil-backward-end)
(define-key evil-outer-text-objects-map "s" 'sentence-nav-evil-outer-sentence)
(define-key evil-inner-text-objects-map "s" 'sentence-nav-evil-inner-sentence)

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

;;; Evil operators - PythonNut

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

;; Avy bindings
(define-key evil-normal-state-map (kbd "SPC l") 'avy-goto-line)
(define-key evil-visual-state-map (kbd "SPC l") 'avy-goto-line)
(define-key evil-normal-state-map (kbd "s") 'avy-goto-char-2)
(define-key evil-visual-state-map (kbd "s") 'avy-goto-char-2)

;; Swiper (with Ivy and counsel)
(define-key evil-normal-state-map (kbd "SPC d") 'counsel-M-x)
(define-key evil-normal-state-map (kbd "SPC SPC") 'swiper)
(define-key evil-normal-state-map (kbd "SPC r") 'ivy-recentf)
(define-key evil-normal-state-map (kbd "SPC u") 'ivy-switch-buffer)
(define-key evil-normal-state-map (kbd "SPC y") 'counsel-yank-pop)
(define-key evil-normal-state-map (kbd "SPC .") 'ivy-resume)
(define-key evil-normal-state-map (kbd "SPC /") 'counsel-locate)
(define-key evil-normal-state-map (kbd "SPC h") 'counsel-ag)
(define-key evil-visual-state-map (kbd "SPC h") 'counsel-ag)
(define-key evil-visual-state-map (kbd "SPC d") 'counsel-M-x)
(define-key evil-insert-state-map (kbd "C-k") 'counsel-unicode-char)
(define-key evil-insert-state-map (kbd "C-l") 'counsel-M-x)

;; ag and wgrep
(define-key evil-normal-state-map (kbd "W") 'sk/hydra-wgrep/body)
(define-key evil-normal-state-map (kbd "SPC e") 'sk/hydra-of-search/body)
(define-key evil-visual-state-map (kbd "SPC e") 'sk/hydra-of-search/body)

;; Swoop
(define-key evil-normal-state-map (kbd "n") 'swoop-pcre-regexp)
(define-key evil-normal-state-map (kbd "N") 'swoop-pcre-regexp)
(define-key evil-visual-state-map (kbd "n") 'swoop-pcre-regexp)
(define-key evil-visual-state-map (kbd "N") 'swoop-pcre-regexp)

;; Expand regions
(define-key evil-normal-state-map (kbd "SPC b") 'sk/hydra-of-edits/body)

;; Multiple cursors
(define-key evil-normal-state-map (kbd "SPC m") 'sk/hydra-of-multiple-cursors/body)

;; Search anything
(define-key evil-normal-state-map (kbd "SPC g") 'sk/hydra-of-search/body)
(define-key evil-visual-state-map (kbd "SPC g") 'sk/hydra-of-search/body)

;;; Visual regexp
(define-key evil-normal-state-map (kbd "SPC v") 'vr/query-replace)
(define-key evil-visual-state-map (kbd "SPC v") 'vr/query-replace)

;; Winner mode
(define-key evil-normal-state-map (kbd "Q") 'winner-undo)

;; Shell
(define-key evil-normal-state-map (kbd "-") 'sk/multi-term-horizontal)
(define-key evil-normal-state-map (kbd "+") 'sk/eshell-vertical)

;; Move lines
(define-key evil-normal-state-map (kbd "]x") 'sk/move-text-down)
(define-key evil-normal-state-map (kbd "[x") 'sk/move-text-up)

;; Blank lines
(define-key evil-normal-state-map (kbd "]n") 'sk/blank-line-down)
(define-key evil-normal-state-map (kbd "[n") 'sk/blank-line-up)

;; Window management
(define-key evil-normal-state-map (kbd "w") 'sk/split-right-and-move)
(define-key evil-normal-state-map (kbd "S") 'sk/split-below-and-move)
(define-key evil-normal-state-map (kbd "SPC i") 'sk/hydra-of-windows/body)

;; Flyspell
(define-key evil-normal-state-map (kbd "]s") 'flyspell-goto-next-error)
(define-key evil-normal-state-map (kbd "[s") 'flyspell-goto-previous-error)

;; PDF
(define-key evil-normal-state-map (kbd "]p") 'sk/other-pdf-next)
(define-key evil-normal-state-map (kbd "[p") 'sk/other-pdf-previous)

;; Neotree
(add-hook 'neotree-mode-hook
            (lambda ()
              (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "o") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "h") 'neotree-select-up-node)
              (define-key evil-normal-state-local-map (kbd "l") 'neotree-change-root)
              (define-key evil-normal-state-local-map (kbd ".") 'neotree-hidden-file-toggle)
              (define-key evil-normal-state-local-map (kbd "r") 'neotree-refresh)
              (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
              (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))
(define-key evil-normal-state-map (kbd "SPC n") 'neotree-toggle)

;; Reveal in OS X finder
(define-key evil-normal-state-map (kbd "gF") 'reveal-in-osx-finder)

;; Find file in project
(define-key evil-normal-state-map (kbd "SPC p") 'find-file-in-project)
(define-key evil-normal-state-map (kbd "SPC TAB") 'ff-find-other-file)

;; Undo tree
(define-key evil-normal-state-map (kbd "U") 'sk/hydra-of-undo/body)

;; Dash at point
(define-key evil-normal-state-map (kbd "SPC c") 'dash-at-point-with-docset)

;; Tags
(define-key evil-normal-state-map (kbd "T") 'ggtags-find-tag-regexp)
(define-key evil-normal-state-map (kbd "SPC t") 'sk/hydra-tags/body)

;; Help Bookmarks
(define-key evil-normal-state-map (kbd "SPC `") 'sk/hydra-bookmarks/body)

;; Help hydra
(define-key evil-normal-state-map (kbd "SPC x") 'sk/hydra-of-help/body)

;; Git
(define-key evil-normal-state-map (kbd "SPC g") 'sk/hydra-of-git/body)

;; Snippets
(define-key evil-insert-state-map (kbd "C-j") 'yas-insert-snippet)

;; Better folding
(define-key evil-normal-state-map (kbd "SPC z") 'sk/hydra-origami/body)

;; Org mode
(define-key evil-normal-state-map (kbd "SPC o") 'sk/hydra-of-org/body)
(define-key evil-normal-state-map (kbd "SPC -") 'org-edit-src-code)
(define-key evil-normal-state-map (kbd "SPC =") 'org-edit-src-exit)
(define-key evil-normal-state-map (kbd "[u") 'org-up-element)
(define-key evil-normal-state-map (kbd "]u") 'org-down-element)
(define-key evil-normal-state-map (kbd "[o") 'org-previous-visible-heading)
(define-key evil-normal-state-map (kbd "]o") 'org-next-visible-heading)
(define-key evil-normal-state-map (kbd "[i") 'org-previous-item)
(define-key evil-normal-state-map (kbd "]i") 'org-next-item)
(define-key evil-normal-state-map (kbd "[h") 'org-backward-heading-same-level)
(define-key evil-normal-state-map (kbd "]h") 'org-forward-heading-same-level)
(define-key evil-normal-state-map (kbd "[b") 'org-previous-block)
(define-key evil-normal-state-map (kbd "]b") 'org-next-block)
(define-key evil-normal-state-map (kbd "[l") 'org-previous-link)
(define-key evil-normal-state-map (kbd "]l") 'org-next-link)
(define-key evil-normal-state-map (kbd "[f") 'org-table-previous-field)
(define-key evil-normal-state-map (kbd "]f") 'org-table-next-field)
(define-key evil-normal-state-map (kbd "gob") 'org-table-blank-field)
(define-key evil-normal-state-map (kbd "gox") 'org-preview-latex-fragment)
(define-key evil-normal-state-map (kbd "goi") 'org-toggle-inline-images)
(define-key evil-normal-state-map (kbd "gor") 'org-reveal)
(define-key evil-normal-state-map (kbd "gof") 'org-refile)
(define-key evil-normal-state-map (kbd "goa") 'org-attach)
(define-key evil-normal-state-map (kbd "gon") 'org-add-note)
(define-key evil-normal-state-map (kbd "goe") 'org-export-dispatch)
(define-key evil-normal-state-map (kbd "gol") 'org-insert-link)
(define-key evil-normal-state-map (kbd "god") 'org-toggle-link-display)
(define-key evil-normal-state-map (kbd "gou") 'org-store-link)
(define-key evil-normal-state-map (kbd "goy") 'org-copy-subtree)
(define-key evil-normal-state-map (kbd "gok") 'org-cut-subtree)
(define-key evil-normal-state-map (kbd "goh") 'org-toggle-heading)
(define-key evil-normal-state-map (kbd "go>") 'org-goto-calendar)
(define-key evil-normal-state-map (kbd "go<") 'org-date-from-calendar)
(define-key evil-normal-state-map (kbd "gos") 'org-sort)
(define-key evil-normal-state-map (kbd "goc") 'sk/hydra-org-clock/body)
(define-key evil-normal-state-map (kbd "got") 'sk/hydra-org-tag-todo/body)
(define-key evil-normal-state-map (kbd "gow") 'sk/hydra-org-drawer/body)
(define-key evil-normal-state-map (kbd "gom") 'sk/hydra-org-manipulate/body)
(define-key evil-normal-state-map (kbd "go|") 'sk/hydra-org-tables/body)
(define-key evil-visual-state-map (kbd "SPC o") 'sk/hydra-of-org/body)

;; Jumps
(define-key evil-normal-state-map (kbd "SPC j") 'sk/hydra-of-jump/body)
(define-key evil-visual-state-map (kbd "SPC j") 'sk/hydra-of-jump/body)

;; Flycheck
(define-key evil-normal-state-map (kbd "]l") 'flycheck-next-error)
(define-key evil-normal-state-map (kbd "[l") 'flycheck-previous-error)
(define-key evil-normal-state-map (kbd "]L") 'flycheck-last-checker)
(define-key evil-normal-state-map (kbd "[L") 'flycheck-first-error)

;; Diff hl
(define-key evil-normal-state-map (kbd "]c") 'diff-hl-next-hunk)
(define-key evil-normal-state-map (kbd "[c") 'diff-hl-previous-hunk)

;; Hydra of activate
(define-key evil-normal-state-map (kbd "SPC a") 'sk/hydra-of-activate/body)

;; Hydra of langs
(define-key evil-normal-state-map (kbd "SPC s") 'sk/hydra-of-langs/body)
(define-key evil-visual-state-map (kbd "SPC s") 'sk/hydra-of-langs/body)

(provide 'sk-evil)

;;; sk-evil.el ends here
