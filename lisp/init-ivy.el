(use-package ivy
  :delight
  :defer t
  :init
  (ivy-mode t)
  :config
  (progn
    (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-initial-inputs-alist nil)
    (setq ivy-re-builders-alist
          '((t . ivy--regex-ignore-order))))
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

(use-package ivy-hydra
  :defer t)

(provide 'init-ivy)
