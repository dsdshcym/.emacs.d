;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook #'(lambda ()
                               (setq gc-cons-threshold 4000000)))

(add-to-list 'load-path "~/.emacs.d/lisp/")

(setq private/cache-directory "~/.emacs.d/.cache/")

(require 'init-package)
(require 'init-ui)
(require 'init-keybinding)
(require 'init-ivy)
(require 'init-git)
(require 'init-projectile)
(require 'init-company)
(global-auto-revert-mode)
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq backup-directory-alist `(("." . "~/.emacs.d/.cache/backups")))
(setq vc-follow-symlinks t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/.cache/auto-save-list/" t)))
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)
(setq sentence-end-double-space nil)
(setq fill-column 80)
(setq initial-scratch-message nil)
