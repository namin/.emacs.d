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
                      sml-mode
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

(global-set-key (kbd "<C-tab>") 'indent-relative-maybe)
(setq-default tab-width 2)
;(setq-default c-basic-indent 2)
;(setq-default lisp-indent-offset 2)
(setq visible-bell nil)

(global-undo-tree-mode t)

(set-default-font "Inconsolata 13")
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode))
(add-to-list 'auto-mode-alist '("\\.dot\\'" . scala-mode))
(add-to-list 'auto-mode-alist '("\\.fun\\'" . sml-mode))
(add-to-list 'auto-mode-alist '("\\.sig\\'" . sml-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(quack-programs (quote ("petite" "bigloo" "csi" "csi -hygienic" "gosh" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "mred -z" "mzscheme" "mzscheme -il r6rs" "mzscheme -il typed-scheme" "mzscheme -M errortrace" "mzscheme3m" "mzschemecgc" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi")))
 '(scheme-program-name "petite"))

(setq twelf-root "~/code/plt/twelf/") (load (concat twelf-root "emacs/twelf-init.el"))

(defun run-mechanics-scheme ()
  "Runs scmutils"
  (interactive)
  (run-scheme "~/.emacs.d/mechanics-emacs")
  (process-kill-without-query (get-process "scheme")))

(remove-hook 'prog-mode-hook 'esk-local-comment-auto-fill)
(remove-hook 'text-mode-hook 'turn-on-auto-fill)

;(add-to-list 'load-path "~/code/zic/Tidal")
;(require 'tidal)

(load-file "~/.emacs.d/chuck-mode.el")
(require 'chuck-mode)

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
(put 'for-each 'scheme-indent-function 0)
(put 'cond 'scheme-indent-function 0)
