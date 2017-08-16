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
(setq private/system-is-mac (eq system-type 'darwin))
(setq private/system-is-linux (eq system-type 'gnu/linux))
(setq private/system-is-linux (eq system-type 'windows-nt))

(require 'init-package)
(require 'init-keybinding)
(require 'init-ui)
(require 'init-ivy)
(require 'init-version-control)
(require 'init-projectile)
(require 'init-auto-completion)
(require 'init-org)
(require 'init-chinese)
(require 'init-editing)
(require 'init-syntax-checking)
(require 'init-spell-checking)
(require 'init-dired)
(require 'init-ruby)
(require 'init-yaml)
(require 'init-html)
(require 'init-coffee)
(require 'init-lua)
(require 'init-mac)
(require 'init-elfeed)
(require 'init-mu4e)
(require 'init-ledger)
(require 'init-pdf)
(require 'init-doc)
(require 'init-server)

(defun private/push-to-kindle (url)
  (browse-url (concat "http://fivefilters.org/kindle-it/send.php?url=" url)))

(defun private/notification (title msg &optional subtitle group-id sound)
  (interactive)
  (if private/system-is-mac
      (call-process-shell-command
       (concat "terminal-notifier"
               " -title \"" title
               "\" -message \"" msg
               (if subtitle (concat "\" -subtitle \"" subtitle))
               (if sound (concat "\" -sound \"" sound))
               (if group-id (concat "\" -group \"" group-id))
               "\" -activate " "org.gnu.Emacs"
               " -sender " "org.gnu.Emacs"
               " -timeout " "3"
               "&")))
  (if private/system-is-linux
      (call-process-shell-command
       (concat "notify-send" " \"" title "\" \"" msg "\""))))

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
