(use-package Info-mode
  :ensure nil
  :general
  (general-define-key
   :keymaps 'Info-mode-map
   [override-state] nil
   [motion-state] nil)
  (private/enable-leader-key-for-mode
   :keymaps 'Info-mode-map)
  (general-mmap
   :keymaps 'Info-mode-map
   "q"   'Info-exit
   "RET" 'Info-follow-nearest-node
   "C-n" 'Info-next
   "C-p" 'Info-prev))

(provide 'init-doc)
