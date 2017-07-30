(use-package projectile
  :commands (projectile-ack
             projectile-ag
             projectile-compile-project
             projectile-dired
             projectile-find-dir
             projectile-find-file
             projectile-find-tag
             projectile-test-project
             projectile-grep
             projectile-invalidate-cache
             projectile-kill-buffers
             projectile-multi-occur
             projectile-project-p
             projectile-project-root
             projectile-recentf
             projectile-regenerate-tags
             projectile-replace
             projectile-replace-regexp
             projectile-run-async-shell-command-in-root
             projectile-run-shell-command-in-root
             projectile-switch-project
             projectile-switch-to-buffer
             projectile-vc)
  :init (progn
          (setq projectile-sort-order 'recentf
                projectile-cache-file (concat private/cache-directory
                                              "projectile.cache")
                projectile-known-projects-file (concat private/cache-directory
                                                       "projectile-bookmarks.eld")))
  :config (progn
            (setq projectile-completion-system 'ivy)
            (projectile-global-mode))
  :general (general-define-key 
            :keymaps '(motion insert emacs)
            :prefix "SPC"
            :non-normal-prefix "M-m"
            "p!" 'projectile-run-shell-command-in-root
            "p&" 'projectile-run-async-shell-command-in-root
            "p%" 'projectile-replace-regexp
            "pa" 'projectile-toggle-between-implementation-and-test
            "pb" 'projectile-switch-to-buffer
            "pc" 'projectile-compile-project
            "pd" 'projectile-find-dir
            "pD" 'projectile-dired
            "pf" 'projectile-find-file
            "pF" 'projectile-find-file-dwim
            "pg" 'projectile-find-tag
            "pG" 'projectile-regenerate-tags
            "pI" 'projectile-invalidate-cache
            "pk" 'projectile-kill-buffers
            "pp" 'projectile-switch-project
            "pr" 'projectile-recentf
            "pR" 'projectile-replace
            "pT" 'projectile-test-project
            "pv" 'projectile-vc))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-on))

(provide 'init-projectile)
