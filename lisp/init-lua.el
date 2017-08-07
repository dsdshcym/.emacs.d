(use-package lua-mode
  :defer t
  :mode ("\\.lua$" . lua-mode)
  :interpreter "lua"
  :init
  (setq lua-indent-level 2))

(use-package company-lua
  :defer t
  :init
  (add-hook 'lua-mode-hook
            (lambda ()
              (make-local-variable 'company-backends)
              (add-to-list 'company-backends 'company-lua))))

(provide 'init-lua)
