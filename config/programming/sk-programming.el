;;; sk-programming.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; For all kinds of programming

;;; Code:

;; Indentation and stuff
(use-package editorconfig
  :ensure t
  :demand t
  :config
  (editorconfig-mode 1))

;; Arduino mode
(use-package arduino-mode
  :ensure t
  :mode "\\.pde$")

;; YAML editing
(use-package yaml-mode
  :ensure t
  :mode "\\.yaml$")

;; C++
(require 'sk-cpp)

;; Python
(require 'sk-python)

;; ESS
(require 'sk-ess)

;; MATLAB
(require 'sk-matlab)

;; JS/HTML/CSS
(require 'sk-web)

;; Error checking
(require 'sk-flycheck)

;; Auto completion
(require 'sk-company)

;; REPL stuff
(require 'sk-repl)

;; start services easily
(use-package prodigy
  :ensure t
  :commands (prodigy)
  :bind (
	 ("C-c B" . prodigy)
	 )
  :init
  (prodigy-define-tag
    :name 'blog
    :ready-message "Serving blog. Ctrl-C to shutdown server")
  (prodigy-define-service
    :name "Nikola build"
    :command "nikola"
    :args '("build")
    :cwd "/Users/sriramkswamy/Dropbox/org/blogposts"
    :tags '(blog)
    :kill-signal 'sigkill)
  (prodigy-define-service
    :name "Nikola serve"
    :command "nikola"
    :args '("serve" "--browser")
    :cwd "/Users/sriramkswamy/Dropbox/org/blogposts"
    :tags '(blog)
    :kill-signal 'sigkill
    :kill-process-buffer-on-stop t))
;; Modal bindings and explanation
(modalka-define-kbd "c b" "C-c B")
(which-key-add-key-based-replacements "c b" "background process")

;; Better debugging
(use-package realgud
  :ensure t
  :commands (realgud:gdb
	     realgud:ipdb
	     realgud:pdb))

;; aux requirements
(require 'sk-programming-hydra)

(provide 'sk-programming)
;;; sk-programming.el ends here
