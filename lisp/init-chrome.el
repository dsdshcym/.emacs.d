(use-package atomic-chrome
  :defer t
  :init
  (atomic-chrome-start-server)
  :config
  (progn
    (setq atomic-chrome-default-major-mode 'org-mode)
    (setq atomic-chrome-buffer-open-style 'frame)
    (add-hook 'atomic-chrome-edit-done-hook 'delete-frame)))

(provide 'init-chrome)
