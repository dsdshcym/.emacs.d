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

(use-package wgrep
  :defer t)

(provide 'init-editing)
