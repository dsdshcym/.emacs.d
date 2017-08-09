(use-package dired
  :ensure nil
  :defer t
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

(provide 'init-dired)
