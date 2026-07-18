;; init.el

(unless window-system           ; only in terminal (emacs -nw)
  (set-face-attribute 'default nil
                      :background "white"
                      :foreground "black"))

;; Keep Custom out of the way
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file 'noerror))

;; Package archives
(require 'package)
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("nongnu". "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; use-package as config macro, not package manager
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; General behavior
(setopt use-short-answers t)
(global-auto-revert-mode 1)
(save-place-mode 1)
(recentf-mode 1)

(setq-default indent-tabs-mode nil)
(setq tab-width 4)

(setq undo-tree-auto-save-history nil)
(use-package undo-tree
  :init
  (global-undo-tree-mode 1))

;; JS indentation
(add-hook 'js-mode-hook
          (lambda ()
            (setq js-indent-level 2)))

;; File associations
(add-to-list 'auto-mode-alist '("\\.dcj\\'"  . java-mode))
(add-to-list 'auto-mode-alist '("\\.rkt\\'"  . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.lurk\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.smt\\'"  . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.sc\\'"   . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.bl\\'"   . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.blk\\'"  . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.f\\'"    . text-mode))

;; Paredit
(use-package paredit
  :hook ((emacs-lisp-mode lisp-mode lisp-interaction-mode
          scheme-mode clojure-mode) . enable-paredit-mode)
  :config
  ;; paredit.el notes incompatibility with electric-indent-mode. [page:0]
  (add-hook 'paredit-mode-hook
            (lambda () (electric-indent-local-mode -1)))

  ;; Scheme REPL behavior you had before
  (add-hook 'inferior-scheme-mode-hook
            (lambda ()
              (paredit-mode 1)
              (define-key paredit-mode-map (kbd "RET") nil)
              (define-key paredit-mode-map (kbd "C-d")
                #'comint-delchar-or-maybe-eof)))

  ;; Tweaks to avoid conflicts with your global word motion keys
  (with-eval-after-load 'paredit
    (define-key paredit-mode-map (kbd "A-<left>")  #'paredit-backward)
    (define-key paredit-mode-map (kbd "A-<right>") #'paredit-forward)

    ;; Optional: classic RET behavior in Lisp buffers
    (define-key paredit-mode-map (kbd "RET") #'paredit-newline)
    (define-key paredit-mode-map (kbd "C-j") nil)))

(show-paren-mode 1)

;; Pretty lambdas
(defun my-pretty-lambda ()
  (setq prettify-symbols-alist '(("lambda" . 955))))  ;; λ [web:27][web:21]
(add-hook 'text-mode-hook 'my-pretty-lambda)
(add-hook 'shell-mode-hook 'my-pretty-lambda)
(global-prettify-symbols-mode 1)  ;; enables prettify-symbols everywhere [web:23][web:25]

;; Undo-tree-friendly visual undo exists as vundo in modern Emacs if you ever want it. [web:19]

;; ;; Boogie/Dafny
;; (add-to-list 'load-path "~/.emacs.d/lisp/boogie-friends/emacs/")
;; (require 'dafny-mode)
;; (require 'boogie-mode)

;; Turn off electric-indent globally (you had an after-change-major-mode hook)
(add-hook 'after-change-major-mode-hook
          (lambda () (electric-indent-mode -1)))

;; Twelf
;;(setq twelf-root "~/code/152/twelf/")
;;(load (concat twelf-root "emacs/twelf-init.el"))

;; Scheme indentation rules (miniKanren + friends)
(put 'generate-verify   'scheme-indent-function 1)
(put 'tabled            'scheme-indent-function 1)
(put 'with-mutex        'scheme-indent-function 1)
(put 'trace-lambda      'scheme-indent-function 1)
(put 'lambdae           'scheme-indent-function 1)
(put 'lambdaw           'scheme-indent-function 1)
(put 'lambda-w          'scheme-indent-function 1)
(put 'timed-lambda      'scheme-indent-function 1)
(put 'ulambda           'scheme-indent-function 1)
(put 'lambda-e          'scheme-indent-function 1)
(put 'lambda-a          'scheme-indent-function 1)
(put 'lambda-u          'scheme-indent-function 1)
(put 'tlambda           'scheme-indent-function 1)
(put 'lambdaf@          'scheme-indent-function 1)
(put 'lambdag@          'scheme-indent-function 1)
(put 'fresh             'scheme-indent-function 1)
(put 'eigen             'scheme-indent-function 1)
(put 'exists            'scheme-indent-function 1)
(put 'exist             'scheme-indent-function 1)
(put 'nom               'scheme-indent-function 1)
(put 'run               'scheme-indent-function 2)
(put 'run-unique        'scheme-indent-function 2)
(put 'case-inf          'scheme-indent-function 1)
(put 'letcc             'scheme-indent-function 1)
(put 'let-tie           'scheme-indent-function 1)
(put 'conde             'scheme-indent-function 0)
(put 'condp             'scheme-indent-function 0)
(put 'condq             'scheme-indent-function 0)
(put 'conda             'scheme-indent-function 0)
(put 'condu             'scheme-indent-function 0)
(put 'test              'scheme-indent-function 1)
(put 'test-genny        'scheme-indent-function 1)
(put 'test-unify        'scheme-indent-function 1)
(put 'test-check        'scheme-indent-function 1)
(put 'test-divergence   'scheme-indent-function 1)
(put 'make-engine       'scheme-indent-function 0)
(put 'run-prob*         'scheme-indent-function 1)
(put 'run-prob          'scheme-indent-function 1)
(put 'run*              'scheme-indent-function 1)
(put 'run1              'scheme-indent-function 1)
(put 'run2              'scheme-indent-function 1)
(put 'run3              'scheme-indent-function 1)
(put 'run4              'scheme-indent-function 1)
(put 'run5              'scheme-indent-function 1)
(put 'run6              'scheme-indent-function 1)
(put 'run7              'scheme-indent-function 1)
(put 'run8              'scheme-indent-function 1)
(put 'run9              'scheme-indent-function 1)
(put 'run10             'scheme-indent-function 1)
(put 'run11             'scheme-indent-function 1)
(put 'run12             'scheme-indent-function 1)
(put 'run13             'scheme-indent-function 1)
(put 'run15             'scheme-indent-function 1)
(put 'run22             'scheme-indent-function 1)
(put 'run34             'scheme-indent-function 1)
(put 'project           'scheme-indent-function 1)
(put 'match             'scheme-indent-function 1)
(put 'pmatch            'scheme-indent-function 1)
(put 'umatch            'scheme-indent-function 1)
(put 'matche            'scheme-indent-function 1)
(put 'match-e           'scheme-indent-function 1)
(put 'match-a           'scheme-indent-function 1)
(put 'match-u           'scheme-indent-function 1)
(put 'union-case        'scheme-indent-function 2)
(put 'cases             'scheme-indent-function 1)
(put 'let-values        'scheme-indent-function 1)
(put 'mv-let            'scheme-indent-function 1)
(put 'call-with-values  'scheme-indent-function 2)
(put 'syntax-case       'scheme-indent-function 2)
(put 'syntax-rules+     'scheme-indent-function 2)
(put 'extend-syntax     'scheme-indent-function 1)
(put 'curry             'scheme-indent-function 1)
(put 'for-each          'scheme-indent-function 0)
(put 'cond              'scheme-indent-function 0)
(put 'def+              'scheme-indent-function 1)

;; Global keybindings
(global-set-key (kbd "C-x a") #'revert-buffer-no-confirm)

(global-set-key (kbd "A-<left>")  #'backward-word)
(global-set-key (kbd "A-<right>") #'forward-word)

(global-set-key (kbd "A-M-<right>") #'paredit-forward-slurp-sexp)
(global-set-key (kbd "A-M-<left>")  #'paredit-forward-barf-sexp)

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer t t))

;; macOS modifiers
(setq mac-option-modifier 'none)
(setq mac-command-modifier 'meta)

;; Language/tooling packages (pulled from your package-selected-packages)
(use-package go-mode)
(use-package haskell-mode)
(use-package gptel)
(use-package merlin)
(use-package tuareg)
(use-package chatgpt-shell)
(use-package proof-general)
(use-package cider)
(use-package scala-mode)
(use-package slime)
(use-package yaml-mode)
(use-package jinja2-mode)
(use-package typescript-mode)
(use-package markdown-mode)
(use-package conda)
(use-package highlight-parentheses)
(use-package company)
(use-package rainbow-delimiters)
