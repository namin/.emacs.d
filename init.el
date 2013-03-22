(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit starter-kit-lisp starter-kit-bindings
                      starter-kit-eshell
                      clojure-mode clojure-test-mode nrepl
                      scala-mode
                      markdown-mode
                      rainbow-delimiters
                      undo-tree)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(show-paren-mode t)
(setq show-paren-style 'parenthesis)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)
(remove-hook 'prog-mode-hook 'esk-turn-on-idle-highlight-mode)

(setq-default tab-width 2)
(setq visible-bell nil)

(global-undo-tree-mode t)

(set-default-font "Droid Sans Mono 14")

(custom-set-variables '(scheme-program-name "petite"))

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
