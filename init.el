(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit starter-kit-lisp starter-kit-bindings
                      starter-kit-eshell
                      exec-path-from-shell
                      clojure-mode clojure-test-mode nrepl
                      scheme-complete
                      scala-mode
                      markdown-mode
                      rainbow-delimiters
                      undo-tree browse-kill-ring)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(menu-bar-mode t)

(show-paren-mode t)
(setq show-paren-style 'parenthesis)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)
(remove-hook 'prog-mode-hook 'esk-turn-on-idle-highlight-mode)

(setq-default tab-width 2)
;(setq-default c-basic-indent 2)
;(setq-default lisp-indent-offset 2)
(setq visible-bell nil)

(global-undo-tree-mode t)

(set-default-font "Inconsolata 17")
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode))
(add-to-list 'auto-mode-alist '("\\.dot\\'" . scala-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(scheme-program-name "petite"))

(setq twelf-root "~/code/plt/twelf/") (load (concat twelf-root "emacs/twelf-init.el"))

(defun run-mechanics-scheme ()
  "Runs scmutils"
  (interactive)
  (run-scheme "~/.emacs.d/mechanics-emacs")
  (process-kill-without-query (get-process "scheme")))
