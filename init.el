;;; Package --- Summary

;;; Commentary:
;; Emacs init file responsible for either loading a pre-compiled configuration file
;; or tangling and loading a literate org configuration file.

;;; Code:

(setq +emacs-directory (file-truename user-emacs-directory))
(setq +local-directory (concat +emacs-directory ".local/"))
(setq +packages-directory (concat +local-directory "packages/"))
(setq +is-emacs-mac-port (eq window-system 'mac))
(setq +is-emacs-ns (eq window-system 'ns))

(let ((file-name-handler-alist nil))
  ;; If config is pre-compiled, then load that
  (if (file-exists-p (expand-file-name "config.elc" user-emacs-directory))
      (load-file (expand-file-name "config.elc" user-emacs-directory))
    ;; Otherwise use org-babel to tangle and load the configuration
    (setq load-prefer-newer t
          package-user-dir (expand-file-name "elpa" +packages-directory)
          package--init-file-ensured t
          package-enable-at-startup nil)

    (unless (file-directory-p package-user-dir)
      (make-directory package-user-dir t))

    (require 'package)
    (unless (assoc-default "melpa" package-archives)
      (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))
    (unless (assoc-default "org" package-archives)
      (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t))

    (package-initialize)
    (unless (package-installed-p 'use-package)
      (package-refresh-contents)
      (package-install 'use-package))
    (require 'use-package)

    ;; (require 'cl)
    ;; (setq load-path (remove-if (lambda (x) (string-match-p "org$" x)) load-path))

    (use-package org
      :ensure org-plus-contrib)
    (org-babel-load-file (expand-file-name "config.org" user-emacs-directory))))

;;; init.el ends here
