(use-package ivy
  :config (progn
            (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
            (setq ivy-initial-inputs-alist nil)
            (setq ivy-re-builders-alist
                  '((t . ivy--regex-ignore-order)))
            (ivy-mode))
  :delight
  :general (general-define-key :prefix "SPC"
                               :non-normal-prefix "M-m"
                               :keymaps '(motion insert emacs)
                               ;; Buffer
                               "bb" 'ivy-switch-buffer))

(use-package counsel
  :delight
  :after ivy
  :config (counsel-mode)
  :general (general-define-key :prefix "SPC"
                               :non-normal-prefix "M-m"
                               :keymaps '(motion insert emacs)
                               ;; Insert
                               "iu" 'counsel-unicode-char
                               ;; Register/Ring
                               "ry" 'counsel-yank-pop
                               "rm" 'counsel-mark-ring))

(use-package swiper
  :after ivy
  :general (general-define-key :prefix "SPC"
                               :non-normal-prefix "M-m"
                               :keymaps '(motion insert emacs)
                               "ss" 'swiper))

(provide 'init-ivy)
