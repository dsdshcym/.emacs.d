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

(use-package wdired
  :ensure nil
  :general
  (private/set-leader-keys-for-mode
   :keymaps 'dired-mode-map
   "w" 'wdired-change-to-wdired-mode))

(provide 'init-dired)
