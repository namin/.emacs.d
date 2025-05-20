(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ignored-local-variable-values '((global-visual-line-mode)))
 '(package-selected-packages
   '(go-mode haskell-mode gptel ## merlin tuareg chatgpt-shell proof-general cider scala-mode slime yaml-mode jinja2-mode typescript-mode markdown-mode conda highlight-parentheses company rainbow-delimiters paredit undo-tree)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-to-list 'default-frame-alist '(font . "Monaco 19"))

(setq-default frame-title-format '("%b"))

(setq undo-tree-auto-save-history nil)

(setq-default indent-tabs-mode nil)
(setq tab-width 4)

(add-hook 'js-mode-hook
          (lambda ()
            (setq js-indent-level 2)))

(global-undo-tree-mode)

(add-to-list 'auto-mode-alist '("\\.dcj\\'" . java-mode))

(add-to-list 'auto-mode-alist '("\\.rkt\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.lurk\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.smt\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.sc\\'" . scheme-mode))

(add-to-list 'auto-mode-alist '("\\.bl\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.blk\\'" . scheme-mode))

(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code."
  t)
(add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)
(add-hook 'lisp-mode-hook             'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'scheme-mode-hook           'enable-paredit-mode)

(add-hook 'inferior-scheme-mode-hook
          (lambda ()
            (paredit-mode 1)
            (define-key paredit-mode-map (kbd "RET") nil)
            (define-key paredit-mode-map (kbd "C-d") 'comint-delchar-or-maybe-eof)))

(setq inferior-lisp-program "sbcl")

;; work-around mac os x key bindings using karabiner
(global-set-key (kbd "A-M-<right>") 'paredit-forward-slurp-sexp)
(global-set-key (kbd "A-M-<left>") 'paredit-forward-barf-sexp)

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))
(global-set-key (kbd "C-x a") 'revert-buffer-no-confirm)


(add-to-list 'auto-mode-alist '("\\.f\\'" . text-mode))

(defun my-pretty-lambda ()
  "make some word or string show as pretty Unicode symbols"
  (setq prettify-symbols-alist
        '(
          ("lambda" . 955)              ; λ
          )))

(add-hook 'text-mode-hook 'my-pretty-lambda)
(add-hook 'shell-mode-hook 'my-pretty-lambda)
(global-prettify-symbols-mode 1)

(add-to-list 'load-path "~/.emacs.d/lisp/boogie-friends/emacs/")
(require 'dafny-mode)
(require 'boogie-mode)

(add-hook 'after-change-major-mode-hook (lambda() (electric-indent-mode -1)))

(setq twelf-root "/Users/namin/code/152/twelf/")
(load (concat twelf-root "emacs/twelf-init.el"))
