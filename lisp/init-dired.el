(use-package dired
  :ensure nil
  :defer t
  :config
  (setq dired-dwim-target t)
  :general
  (private/set-leader-keys
   "fj" 'dired-jump
   "fJ" 'dired-jump-other-window)
  (private/enable-leader-key-for-mode
   :keymaps 'dired-mode-map))

(use-package dired-x
  :ensure nil
  :commands (dired-jump
             dired-jump-other-window))

(use-package dired-quick-sort
  :defer t
  :init
  (dired-quick-sort-setup))

(provide 'init-dired)
