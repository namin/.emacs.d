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
   '(julia-mode pyvenv pyenv-mode jupyter slime sml-mode cider clojure-mode smartparens haskell-mode exec-path-from-shell rainbow-mode paredit-everywhere racket-mode markdown-mode scala-mode undo-tree rainbow-delimiters paredit proof-general))
 '(safe-local-variable-values '((Syntax . Common-lisp)))
 '(scheme-program-name "racket"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(setq-default indent-tabs-mode nil)

(global-undo-tree-mode)

;;(set-default-font "Monaco 17")
(add-to-list 'default-frame-alist '(font . "Monaco 17"))

(add-to-list 'auto-mode-alist '("\\.rkt\\'" . scheme-mode))

(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code."
  t)
(add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)
(add-hook 'lisp-mode-hook             'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'scheme-mode-hook           'enable-paredit-mode)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(require 'smartparens-config)
(show-smartparens-global-mode +1)
(smartparens-global-mode t)

(put 'downcase-region 'disabled nil)

;; slime

;; update this path to the correct location.
(setq *slime-path* "~/.emacs.d/elpa/slime-20200810.224")
(add-to-list 'load-path *slime-path*)
(require 'slime-autoloads)

(eval-after-load "slime"
  '(progn
    (add-to-list 'load-path *slime-path*)
    (slime-setup '(slime-fancy slime-banner))
    (setq slime-complete-symbol*-fancy t)
    (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)))
(setq inferior-lisp-program "alisp")
;;(setq inferior-lisp-program "/Applications/AllegroCL64express.app/Contents/Resources/allegro-express")


(setenv "WORKON_HOME" "/Users/namin/opt/anaconda3/envs")
(pyvenv-mode 1)

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))
(global-set-key (kbd "C-x a") 'revert-buffer-no-confirm)

(defun run-chez-scheme ()
  "Runs Chez Scheme"
  (interactive)
  (run-scheme "chez"))
