(use-package yaml-mode
  :defer t
  :mode (("\\.\\(yml\\|yaml\\)\\'" . yaml-mode)
         ("Procfile\\'" . yaml-mode)))

(provide 'init-yaml)
