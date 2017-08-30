(use-package avy
  :defer t
  :init
  (progn
    (setq avy-all-windows nil)
    (setq avy-background t))
  :general
  (private/set-leader-keys
   "jb" 'avy-pop-mark
   "jj" 'evil-avy-goto-char
   "jJ" 'evil-avy-goto-char-2
   "jl" 'evil-avy-goto-line
   "jw" 'evil-avy-goto-word-or-subword-1))

(use-package ace-pinyin
  :delight
  :defer t
  :init
  (progn
    (setq ace-pinyin-use-avy t)
    (ace-pinyin-global-mode t)))

(use-package undo-tree
  :delight
  :defer t
  :init
  (global-undo-tree-mode)
  :config
  (progn
    (setq undo-tree-history-directory-alist `(("." . ,(concat private/cache-directory "undo-tree-history"))))
    (setq undo-tree-auto-save-history t)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

(use-package saveplace
  :init
  (progn
    (setq save-place-file (concat private/cache-directory "places"))
    (save-place-mode)))

(use-package autorevert
  :defer t
  :delight auto-revert-mode
  :init
  (global-auto-revert-mode))

(use-package recentf
  :defer t
  :init
  (progn
    ;; lazy load recentf
    (add-hook 'find-file-hook (lambda () (unless recentf-mode
                                           (recentf-mode)
                                           (recentf-track-opened-file))))
    (setq recentf-save-file (concat private/cache-directory "recentf")
          recentf-max-saved-items 1000
          recentf-auto-cleanup 'never
          recentf-auto-save-timer (run-with-idle-timer 600 t
                                                       'recentf-save-list)))
  :config
  (progn
    (add-to-list 'recentf-exclude (file-truename private/cache-directory))
    (add-to-list 'recentf-exclude (file-truename package-user-dir))
    (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")))

(use-package savehist
  :init
  (progn
    ;; Minibuffer history
    (setq savehist-file (concat private/cache-directory "savehist")
          enable-recursive-minibuffers t ; Allow commands in minibuffers
          history-length 1000
          savehist-additional-variables '(mark-ring
                                          global-mark-ring
                                          search-ring
                                          regexp-search-ring
                                          extended-command-history)
          savehist-autosave-interval 60)
    (savehist-mode t)))

(use-package ws-butler
  :delight
  :init
  (ws-butler-global-mode))

(use-package wgrep
  :defer t)

(use-package string-inflection
  :commands (string-inflection-camelcase
             string-inflection-lower-camelcase
             string-inflection-kebab-case
             string-inflection-underscore
             string-inflection-upcase)
  :general
  (general-mmap
   "gsc" 'string-inflection-lower-camelcase
   "gsC" 'string-inflection-camelcase
   "gs-" 'string-inflection-kebab-case
   "gs_" 'string-inflection-underscore
   "gsU" 'string-inflection-upcase))

(use-package files
  :ensure nil
  :config
  (progn
    (setq backup-directory-alist `(("." . "~/.emacs.d/.cache/backups")))
    (setq auto-save-list-file-prefix "~/.emacs.d/.cache/auto-save-list/.saves-")
    (setq auto-save-file-name-transforms '((".*" "~/.emacs.d/.cache/auto-save-list/" t)))))

(provide 'init-editing)
