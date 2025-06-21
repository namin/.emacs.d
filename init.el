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

(setq scheme-program-name "chez")
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

(global-set-key (kbd "A-<left>") 'backward-word)
(global-set-key (kbd "A-<right>") 'forward-word)
(with-eval-after-load 'paredit
  (define-key paredit-mode-map (kbd "A-<left>") 'paredit-backward)
  (define-key paredit-mode-map (kbd "A-<right>") 'paredit-forward))

(setq twelf-root "/Users/namin/code/152/twelf/")
(load (concat twelf-root "emacs/twelf-init.el"))

(put 'generate-verify 'scheme-indent-function 1)
(put 'tabled 'scheme-indent-function 1)
(put 'with-mutex 'scheme-indent-function 1)
(put 'trace-lambda 'scheme-indent-function 1)
(put 'lambdae 'scheme-indent-function 1)
(put 'lambdaw 'scheme-indent-function 1)
(put 'lambda-w 'scheme-indent-function 1)
(put 'timed-lambda 'scheme-indent-function 1)
(put 'ulambda 'scheme-indent-function 1)
(put 'lambda-e 'scheme-indent-function 1)
(put 'lambda-a 'scheme-indent-function 1)
(put 'lambda-u 'scheme-indent-function 1)
(put 'tlambda 'scheme-indent-function 1)
(put 'lambdaf@ 'scheme-indent-function 1)
(put 'lambdag@ 'scheme-indent-function 1)
(put 'fresh 'scheme-indent-function 1)
(put 'eigen 'scheme-indent-function 1)
(put 'exists 'scheme-indent-function 1)
(put 'exist 'scheme-indent-function 1)
(put 'nom 'scheme-indent-function 1)
(put 'run 'scheme-indent-function 2)
(put 'run-unique 'scheme-indent-function 2)
(put 'case-inf 'scheme-indent-function 1)
(put 'letcc 'scheme-indent-function 1)
(put 'let-tie 'scheme-indent-function 1)
(put 'conde 'scheme-indent-function 0)
(put 'condp 'scheme-indent-function 0)
(put 'condq 'scheme-indent-function 0)
(put 'conda 'scheme-indent-function 0)
(put 'condu 'scheme-indent-function 0)
(put 'test 'scheme-indent-function 1)
(put 'test-genny 'scheme-indent-function 1)
(put 'test-unify 'scheme-indent-function 1)
(put 'test-check 'scheme-indent-function 1)
(put 'test-divergence 'scheme-indent-function 1)
(put 'make-engine 'scheme-indent-function 0)
(put 'run-prob* 'scheme-indent-function 1)
(put 'run-prob 'scheme-indent-function 1)
(put 'run* 'scheme-indent-function 1)
(put 'run1 'scheme-indent-function 1)
(put 'run2 'scheme-indent-function 1)
(put 'run3 'scheme-indent-function 1)
(put 'run4 'scheme-indent-function 1)
(put 'run5 'scheme-indent-function 1)
(put 'run6 'scheme-indent-function 1)
(put 'run7 'scheme-indent-function 1)
(put 'run8 'scheme-indent-function 1)
(put 'run9 'scheme-indent-function 1)
(put 'run10 'scheme-indent-function 1)
(put 'run11 'scheme-indent-function 1)
(put 'run12 'scheme-indent-function 1)
(put 'run13 'scheme-indent-function 1)
(put 'run15 'scheme-indent-function 1)
(put 'run22 'scheme-indent-function 1)
(put 'run34 'scheme-indent-function 1)
(put 'project 'scheme-indent-function 1)
(put 'match 'scheme-indent-function 1)
(put 'pmatch 'scheme-indent-function 1)
(put 'umatch 'scheme-indent-function 1)
(put 'matche 'scheme-indent-function 1)
(put 'match-e 'scheme-indent-function 1)
(put 'match-a 'scheme-indent-function 1)
(put 'match-u 'scheme-indent-function 1)
(put 'union-case 'scheme-indent-function 2)
(put 'cases 'scheme-indent-function 1)
(put 'let-values 'scheme-indent-function 1)
(put 'mv-let 'scheme-indent-function 1)
(put 'call-with-values 'scheme-indent-function 2)
(put 'syntax-case 'scheme-indent-function 2)
(put 'syntax-rules+ 'scheme-indent-function 2)
(put 'extend-syntax 'scheme-indent-function 1)
(put 'curry 'scheme-indent-function 1)
(put 'for-each 'scheme-indent-function 0)
(put 'cond 'scheme-indent-function 0)
(put 'def+ 'scheme-indent-function 1)

(setq mac-option-modifier 'none)
(setq mac-command-modifier 'meta)
