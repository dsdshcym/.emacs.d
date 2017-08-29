(use-package magit
  :defer t
  :config (progn
            (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))
            (setq magit-repository-directories '(("~/Projects/" . 2))))
  :general
  (private/set-leader-keys
   "gs" 'magit-status
   "gi" 'magit-init
   "gl" 'magit-log-buffer-file)
  (private/enable-leader-key-for-mode
   :keymaps '(magit-status-mode-map magit-diff-mode-map magit-process-mode-map magit-blame-mode-map magit-log-mode-map)))

(use-package magit-blame
  :ensure magit
  :defer t
  :config
  (progn
    (defun private/magit-blame-quit-completely ()
      (interactive)
      (while magit-blame-mode
        (magit-blame-quit)))

    (defhydra private/magit-blame-hydra (:body-pre (call-interactively 'magit-blame))
      ("b" magit-blame-popup "magit blame popup" :exit t)
      ("p" magit-blame "blame previous version")
      ("n" magit-blame-quit "blame next version")
      ("q" private/magit-blame-quit-completely "quit magit blame completely" :exit t)))
  :general
  (private/set-leader-keys
   "gb" 'private/magit-blame-hydra/body))

(use-package smerge
  :ensure nil
  :defer t
  :config
  (defhydra private/smerge-mode-hydra (:foreign-keys run)
    ("n" smerge-next "next conflict")
    ("p" smerge-prev "previous conflict")
    ("a" smerge-keep-all "keep all")
    ("b" smerge-keep-base "keep base")
    ("m" smerge-keep-mine "keep mine")
    ("o" smerge-keep-other "keep other")
    ("c" smerge-keep-current "keep current")
    ("C" smerge-combine-with-next "combine with next")
    ("r" smerge-refine "refine")
    ("q" nil :exit t))
  :general
  (private/set-leader-keys-for-mode
   :keymaps 'smerge-mode-map
   "'" 'private/smerge-mode-hydra/body))

(use-package orgit
  :defer t)

(use-package evil-magit
  :after magit)

(use-package git-commit
  :defer t)

(use-package git-rebase
  :ensure magit
  :defer t
  :general
  (private/enable-leader-key-for-mode
   :keymaps 'git-rebase-mode-map))

(use-package gitattributes-mode
  :defer t)

(use-package gitconfig-mode
  :defer t)

(use-package gitignore-mode
  :defer t)

(use-package git-timemachine
  :defer t
  :config
  (defhydra private/git-timemachine-hydra (:body-pre (git-timemachine))
    ("p" git-timemachine-show-previous-revision "previous revision")
    ("n" git-timemachine-show-next-revision "next revision")
    ("g" git-timemachine-show-nth-revision "goto revision")
    ("q" git-timemachine-quit "quit timemachine"))
  :general
  (private/set-leader-keys
   "gt" 'private/git-timemachine-hydra/body))

(use-package git-link
  :defer t
  :config
  (setq git-link-open-in-browser t)
  :general
  (private/set-leader-keys
   "gl" nil
   "gll" 'git-link
   "glc" 'git-link-commit
   "glh" 'git-link-homepage))

(use-package diff-mode
  :defer t)

(use-package git-gutter+
  :delight
  :init
  (global-git-gutter+-mode t)
  :config
  (setq git-gutter+-modified-sign " "
        git-gutter+-added-sign "+"
        git-gutter+-deleted-sign "-"
        git-gutter+-diff-option "-w"
        git-gutter+-hide-gutter t))

(use-package git-gutter-fringe+
  :after git-gutter+
  :init
  (setq git-gutter-fr+-side 'right-fringe)
  :config
  (progn
    ;; custom graphics that works nice with half-width fringes
    (fringe-helper-define 'git-gutter-fr+-added nil
      "..X...."
      "..X...."
      "XXXXX.."
      "..X...."
      "..X...."
      )
    (fringe-helper-define 'git-gutter-fr+-deleted nil
      "......."
      "......."
      "XXXXX.."
      "......."
      "......."
      )
    (fringe-helper-define 'git-gutter-fr+-modified nil
      "..X...."
      ".XXX..."
      "XX.XX.."
      ".XXX..."
      "..X....")))

(provide 'init-version-control)
