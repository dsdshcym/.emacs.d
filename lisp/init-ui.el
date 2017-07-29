(blink-cursor-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)

(use-package solarized-theme
  :ensure t
  :init (progn
          (setq solarized-use-variable-pitch nil)
          (setq solarized-scale-org-headlines nil)
          (load-theme 'solarized-dark t nil)))

(provide 'init-ui)
