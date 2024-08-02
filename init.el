(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(slime yaml-mode jinja2-mode typescript-mode markdown-mode conda highlight-parentheses company rainbow-delimiters paredit undo-tree)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(setq-default frame-title-format '("%b"))

(setq undo-tree-auto-save-history nil)

(setq-default indent-tabs-mode nil)
(setq tab-width 4)

(global-undo-tree-mode)

(add-to-list 'auto-mode-alist '("\\.dcj\\'" . java-mode))

(add-to-list 'auto-mode-alist '("\\.rkt\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.lurk\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.smt\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.sc\\'" . scheme-mode))

(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code."
  t)
(add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)
(add-hook 'lisp-mode-hook             'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'scheme-mode-hook           'enable-paredit-mode)

(setq inferior-lisp-program "sbcl") ;;  --control-stack-size 10000
