(use-package ivy
  :config (progn
            (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
            (setq ivy-initial-inputs-alist nil)
            (setq ivy-re-builders-alist
                  '((t . ivy--regex-ignore-order)))
            (ivy-mode))
  :delight
  :general
  (private/set-leader-keys
   ;; Buffer
   "bb" 'ivy-switch-buffer))

(use-package counsel
  :delight
  :after ivy
  :config (counsel-mode)
  :general
  (private/set-leader-keys
   ;; Insert
   "iu" 'counsel-unicode-char
   ;; Register/Ring
   "ry" 'counsel-yank-pop
   "rm" 'counsel-mark-ring))

(use-package swiper
  :after ivy
  :general
  (private/set-leader-keys
   "ss" 'swiper))

(provide 'init-ivy)
