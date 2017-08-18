(use-package elixir-mode
  :defer t)

(use-package ob-elixir
  :after org
  :init
  (add-to-list 'org-babel-load-languages '(elixir . t)))

(provide 'init-elixir)
