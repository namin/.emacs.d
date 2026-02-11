(setq package-enable-at-startup nil)

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(setq native-comp-async-report-warnings-errors 'silent)

;; Frame defaults
(add-to-list 'default-frame-alist '(font . "Monaco 19"))
(setq-default frame-title-format '("%b"))
