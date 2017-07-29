(use-package magit
  :defer t
  :config (progn
            (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))
            (setq magit-repository-directories '(("~/Projects/" . 2))))
  :general (general-define-key :prefix "SPC"
                               :non-normal-prefix "M-m"
                               :states '(normal visual insert emacs)
                               "gs" 'magit-status
                               "gi" 'magit-init
                               "gl" 'magit-log-buffer-file))

(use-package evil-magit
  :after magit)

(use-package git-commit
  :defer t)

(use-package git-commit
  :defer t)

(use-package gitattributes-mode
  :defer t)

(use-package gitconfig-mode
  :defer t)

(use-package gitignore-mode
  :defer t)

(use-package git-timemachine
  :defer t)

(provide 'init-git)
