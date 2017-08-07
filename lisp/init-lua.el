(use-package lua-mode
  :defer t
  :mode ("\\.lua$" . lua-mode)
  :interpreter "lua"
  :init
  (setq lua-indent-level 2))

(provide 'init-lua)
