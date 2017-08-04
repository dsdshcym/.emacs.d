(use-package dired
  :ensure nil
  :defer t
  :general
  (private/set-leader-keys
   "fj" 'dired-jump
   "fJ" 'dired-jump-other-window)
  (general-define-key
   :keymaps 'dired-mode-map
   "SPC" nil))

(use-package dired-x
  :ensure nil
  :commands (dired-jump
             dired-jump-other-window))

(provide 'init-dired)
