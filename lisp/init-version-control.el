(use-package magit
  :defer t
  :config (progn
            (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))
            (setq magit-repository-directories '(("~/Projects/" . 2))))
  :general (general-define-key :prefix "SPC"
                               :non-normal-prefix "M-m"
                               :keymaps '(motion insert emacs)
                               "gs" 'magit-status
                               "gi" 'magit-init
                               "gl" 'magit-log-buffer-file)
  (general-define-key
   :keymaps '(magit-status-mode-map magit-diff-mode-map magit-log-mode-map)
   "SPC" nil))

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

(use-package git-link
  :defer t
  :config
  (setq git-link-open-in-browser t)
  :general
  (general-define-key
   :prefix "SPC"
   :non-normal-prefix "M-m"
   :keymaps '(motion insert emacs)
   "gl" nil
   "gll" 'git-link
   "glc" 'git-link-commit
   "glh" 'git-link-homepage))

(use-package diff-mode
  :defer t)

(provide 'init-version-control)
