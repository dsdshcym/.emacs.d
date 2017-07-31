(use-package dired
  :ensure nil
  :defer t
  :general
  (general-define-key
   :prefix "SPC"
   :non-normal-prefix "M-m"
   :keymaps '(motion insert emacs)
   "fj" 'dired-jump
   "fJ" 'dired-jump-other-window)
  (general-define-key
   :keymaps 'dired-mode-map
   "SPC" nil))

(provide 'init-dired)
