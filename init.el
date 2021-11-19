(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives
               (cons "melpa" (concat proto "://melpa.org/packages/")) t))
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inferior-lisp-program "alisp" t)
 '(package-selected-packages
   '(tabbar ## raku-mode go-mode elixir-mode boogie-friends rust-mode tuareg company ess highlight-parentheses julia-mode pyvenv pyenv-mode jupyter slime sml-mode cider clojure-mode smartparens haskell-mode exec-path-from-shell rainbow-mode paredit-everywhere racket-mode markdown-mode scala-mode undo-tree rainbow-delimiters paredit proof-general))
 '(safe-local-variable-values '((Syntax . Common-lisp)))
 '(scheme-program-name "racket")
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-to-list 'default-frame-alist '(font . "Monaco 17"))

;;(setq exec-path-from-shell-check-startup-files nil)
;; (when (memq window-system '(mac ns x))
;;   (exec-path-from-shell-initialize))

(defun my-exec-path-from-shell-initialize ()
     (when (memq window-system '(mac ns x))
       (exec-path-from-shell-initialize)))

(use-package exec-path-from-shell
  :init
  (add-hook 'after-init-hook 'my-exec-path-from-shell-initialize))

(setq-default indent-tabs-mode nil)
(setq tab-width 4)

(global-undo-tree-mode)

(add-to-list 'auto-mode-alist '("\\.rkt\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.lurk\\'" . scheme-mode))

(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code."
  t)
(add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)
(add-hook 'lisp-mode-hook             'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'scheme-mode-hook           'enable-paredit-mode)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;(require 'smartparens-config)
;;(show-smartparens-global-mode +1)
;;(smartparens-global-mode t)

(setq company-idle-delay 0)
(setq company-minimum-prefix-length 2)
(global-company-mode t)

(progn
  (highlight-parentheses-mode)
  (global-highlight-parentheses-mode))

(put 'downcase-region 'disabled nil)

(show-paren-mode 1)

;; slime

;; update this path to the correct location.
(setq *slime-path* "~/.emacs.d/elpa/slime-20210512.1220")
(add-to-list 'load-path *slime-path*)
(require 'slime-autoloads)

(eval-after-load "slime"
  '(progn
    (add-to-list 'load-path *slime-path*)
    (slime-setup '(slime-fancy slime-banner))
    (setq slime-complete-symbol*-fancy t)
    (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)))
;;(setq inferior-lisp-program "alisp")
;;(setq inferior-lisp-program "/Applications/AllegroCL64express.app/Contents/Resources/allegro-express")

(use-package slime
  :init
  (global-set-key (kbd "C-c z") 'slime-repl)
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (setq inferior-lisp-program "/usr/local/bin/sbcl")
  (add-to-list 'slime-contribs 'slime-repl))

(setenv "WORKON_HOME" "/Users/namin/opt/anaconda3/envs")
(pyvenv-mode 1)
(custom-set-variables '(pyvenv-workon ".."))

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))
(global-set-key (kbd "C-x a") 'revert-buffer-no-confirm)

(defun run-chez-scheme ()
  "Runs Chez Scheme"
  (interactive)
  (run-scheme "chez"))

(add-to-list 'auto-mode-alist '("\\.f\\'" . text-mode))
(setq auto-mode-alist
        (append '(("\\.ml[ily]?$" . tuareg-mode)
                  ("\\.topml$" . tuareg-mode))
                auto-mode-alist))
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
;; (require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

(defun my-pretty-lambda ()
  "make some word or string show as pretty Unicode symbols"
  (setq prettify-symbols-alist
        '(
          ("lambda" . 955) ; λ
          )))

(add-hook 'text-mode-hook 'my-pretty-lambda)
(global-prettify-symbols-mode 1)

(add-to-list 'auto-mode-alist '("\\.blk\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.bl\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.brn\\'" . scheme-mode))
(put 'run-staged 'scheme-indent-function 2)
(put 'run-staged* 'scheme-indent-function 1)
(put 'meaning 'scheme-indent-function 1)
(put 'set! 'scheme-indent-function 1)
(put 'delta 'scheme-indent-function 1)
(put 'lambda-reflect 'scheme-indent-function 1)
(put 'common-define 'scheme-indent-function 1)
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
(put 'extend-syntax 'scheme-indent-function 1)
(put 'curry 'scheme-indent-function 1)
(put 'for-each 'scheme-indent-function 0)
(put 'cond 'scheme-indent-function 0)
(put 'def+ 'scheme-indent-function 1)

(set-frame-parameter (selected-frame) 'alpha '(100 100))
(add-to-list 'default-frame-alist '(alpha 100 100))
;;(eval-when-compile (require 'cl))
(defun toggle-transparency ()
  (interactive)
  (if (/=
       (cadr (frame-parameter nil 'alpha))
       100)
      (set-frame-parameter nil 'alpha '(100 100))
    (set-frame-parameter nil 'alpha '(70 40))))
(global-set-key (kbd "C-c t") 'toggle-transparency)

(load "~/.emacs.d/scribble.el")

;;(setq twelf-root "/Users/namin/code/152/twelf/")
;;(load (concat twelf-root "emacs/twelf-init.el"))

(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save)
            (setq tab-width 4)
            (setq indent-tabs-mode 1)))
(put 'upcase-region 'disabled nil)

(setq proof-splash-seen t)
(setq proof-three-window-mode-policy 'hybrid)

(autoload 'asm86-mode "/Users/namin/.emacs.d/asm86-mode.elc")
(setq auto-mode-alist
    (append '(("\\.asm\\'" . asm86-mode) ("\\.inc\\'" . asm86-mode))
    auto-mode-alist))

(setq ido-enable-flex-matching t
      ido-auto-merge-work-directories-length -1
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-everywhere t
      ido-default-buffer-method 'selected-window)
(ido-mode 1)
(put 'ido-exit-minibuffer 'disabled nil)
(when (require 'ido-ubiquitous nil t)
  (ido-ubiquitous-mode 1))

(setq bluespec-home "/Users/namin/code/blu/bsc/")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for bsv mode 
(add-to-list 'load-path (concat bluespec-home "/util/emacs"))
(load "bsv-mode/bsv-mode-23")

;; for bsv snippets
(setq bsv-snippets-path (concat bluespec-home "/util/emacs/bsv-snippets"))
(add-to-list 'load-path bsv-snippets-path)
(load "bsv-snippets")

;;(setq bluespec-root "/Users/namin/code/blu/bsc/util/emacs/")
;;(load (concat bluespec-root "bsv-mode/bsv-mode-23.el"))
;;(load (concat bluespec-root "emacs20-extras.el"))
;;(load (concat bluespec-root "mark.el"))
(autoload 'bsv-mode "bsv-mode" "BSV mode" t )
(setq auto-mode-alist (cons  '("\\.bsv\\'" . bsv-mode) auto-mode-alist))
