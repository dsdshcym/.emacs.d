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

    (setq ivy-truncate-lines nil)

    (setq ivy-re-builders-alist
          '((t . ivy--regex-ignore-order))))
  :general
  (private/set-leader-keys
   ;; Buffer
   "bb" 'ivy-switch-buffer)
  :general
  (private/set-leader-keys-for-mode
   :keymaps 'ivy-occur-grep-mode-map
   "w" 'ivy-wgrep-change-to-wgrep-mode)
  (private/enable-leader-key-for-mode
   :keymaps 'ivy-occur-grep-mode-map))

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

(use-package smex
  :defer t
  :init
  (setq-default smex-history-length 32
                smex-save-file (concat private/cache-directory ".smex-items")))

(provide 'init-ivy)
