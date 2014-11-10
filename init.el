(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit starter-kit-lisp starter-kit-bindings
                      starter-kit-eshell
                      exec-path-from-shell
                      clojure-mode clojure-test-mode cider
                      lua-mode
                      sass-mode
                      scheme-complete
                      scala-mode2
                      sml-mode
                      haskell-mode
                      tuareg
                      racket-mode
                      markdown-mode
                      web-mode
                      rainbow-delimiters
                      undo-tree browse-kill-ring
                      multi-web-mode
                      misc-cmds)
  "A list of packages to ensure are installed at launch.")

(when window-system
  (add-to-list 'my-packages 'ensime))

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
;(remove-hook 'prog-mode-hook 'esk-turn-on-idle-highlight-mode)

(global-set-key (kbd "<C-tab>") 'indent-relative-maybe)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default js-indent-level 2)
(setq-default css-indent-offset 2)
(setq-default c-basic-indent 2)
;(setq-default lisp-indent-offset 2)
(setq visible-bell t)
(setq ring-bell-function 'ignore)

(global-undo-tree-mode t)

(set-default-font "Monaco 15")
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . sass-mode))
(add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode))
(add-to-list 'auto-mode-alist '("\\.dot\\'" . scala-mode))
(add-to-list 'auto-mode-alist '("\\.sbt\\'" . scala-mode))
(add-to-list 'auto-mode-alist '("\\.fun\\'" . sml-mode))
(add-to-list 'auto-mode-alist '("\\.sig\\'" . sml-mode))
(add-to-list 'auto-mode-alist '("\\.sml\\'" . sml-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-mode-hook (quote (turn-on-haskell-indentation)))
 '(safe-local-variable-values (quote ((coq-prog-args "-emacs-U" "-I" ".") (coq-prog-args "-emacs-U" "-I" "./ln/tlc") (coq-prog-args "-emacs-U" "-I" "metalib" "-I" "lib") (coq-prog-args "-emacs-U" "-I" "./tlc") (whitespace-line-column . 80) (lexical-binding . t))))
 '(scheme-program-name "petite"))

(setq twelf-root "~/local/plt/twelf/")
(let ((twelf-file (concat twelf-root "emacs/twelf-init.el")))
  (when (file-exists-p twelf-file)
    (load twelf-file)))
(setq dafny-root "~/local/plt/dafny/")
(let ((dafny-file (concat dafny-root "Util/Emacs/dafny-mode.el")))
  (when (file-exists-p dafny-file)
    (load dafny-file)))

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
(put 'def+ 'scheme-indent-function 1)

(set-frame-parameter (selected-frame) 'alpha '(100 100))
(add-to-list 'default-frame-alist '(alpha 100 100))
(eval-when-compile (require 'cl))
(defun toggle-transparency ()
  (interactive)
  (if (/=
       (cadr (frame-parameter nil 'alpha))
       100)
      (set-frame-parameter nil 'alpha '(100 100))
    (set-frame-parameter nil 'alpha '(85 50))))
(global-set-key (kbd "C-c t") 'toggle-transparency)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "dark blue"))))
 '(font-lock-string-face ((t (:foreground "black"))))
 '(idle-highlight ((t (:background "ghost white"))))
 '(region ((t (:background "yellow")))))

(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((js-mode "<script>" "</script>")
                  (css-mode "<style" "</style>")))
(setq mweb-filename-extensions '("html"))
(multi-web-global-mode 1)

(defadvice yes-or-no-p (around prevent-dialog activate)
  "Prevent yes-or-no-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))
(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  "Prevent y-or-n-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))

(let
    (
     ;; mac os x via homebrew
     (proof-general-loc1 "/usr/local/share/emacs/site-lisp/ProofGeneral/generic/proof-site.el")
     ;; linux via mint 17
     (proof-general-loc2 "/usr/share/emacs/site-lisp/proofgeneral/generic/proof-site.el")
     )
  (when (or (file-exists-p proof-general-loc1) (file-exists-p proof-general-loc2)) 
    (load-file (if (file-exists-p proof-general-loc1)
                   proof-general-loc1
                 proof-general-loc2))
    (setq auto-mode-alist (cons '("\\.v$" . coq-mode) auto-mode-alist))
    (autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t)
    ))

;; (load-file (let ((coding-system-for-read 'utf-8))
;;              (shell-command-to-string "agda-mode locate")))

(global-set-key (kbd "C-x a") 'revert-buffer-no-confirm)

;; OCaml

;; Add opam emacs directory to the load-path
(when (executable-find "opam")
(setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))

;; Load merlin-mode
(require 'merlin)

;; Start merlin on ocaml files
(add-hook 'tuareg-mode-hook 'merlin-mode t)
(add-hook 'caml-mode-hook 'merlin-mode t)

;; Enable auto-complete
(setq merlin-use-auto-complete-mode 'easy)

;; Use opam switch to lookup ocamlmerlin binary
(setq merlin-command 'opam)

(require 'ocp-indent)
)

;; quick hack to run a command on each save of a file
;; adapted from http://rtime.felk.cvut.cz/~sojka/blog/compile-on-save/

(defun compile-on-save-cmd ()
  (if (local-variable-p 'on-save-cmd)
      (progn
        (shell-command (buffer-local-value 'on-save-cmd (current-buffer)))
        (message ""))
    (let ((user-cmd (read-from-minibuffer "command? ")))
      (if (eq 0 (shell-command user-cmd))
          (progn
            (setq-local on-save-cmd user-cmd)
            (message "ok :)"))
        (message "failed :(")))))

(defun compile-on-save-start ()
  (compile-on-save-cmd))

(define-minor-mode compile-on-save-mode
  "Minor mode to automatically call `recompile' whenever the
current buffer is saved. When there is ongoing compilation,
nothing happens."
  :lighter " CoS"
    (if compile-on-save-mode
    (progn  (make-local-variable 'after-save-hook)
            (add-hook 'after-save-hook 'compile-on-save-start nil t))
    (progn
      (kill-local-variable 'after-save-hook)
      (kill-local-variable 'on-save-cmd))))

;; ensime for Scala
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
