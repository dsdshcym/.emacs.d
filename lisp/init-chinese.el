(use-package fcitx
  :init
  (fcitx-evil-turn-on))

(use-package pangu-spacing
  :delight
  :defer t
  :init
  (progn
    (setq pangu-spacing-real-insert-separtor t)
    (global-pangu-spacing-mode 1)))

(provide 'init-chinese)
