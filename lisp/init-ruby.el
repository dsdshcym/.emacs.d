(use-package ruby-mode
  :delight
  :defer t
  :mode (("Appraisals\\'" . ruby-mode)
         ("Puppetfile" . ruby-mode))
  :general
  (general-define-key
   :prefix "SPC m"
   :non-normal-prefix "M-m"
   :states '(motion insert emacs)
   :keymaps 'ruby-mode-map
   "'" 'ruby-toggle-string-quotes
   "{" 'ruby-toggle-block))

(use-package rbenv
  :defer t
  :init
  (global-rbenv-mode)
  :config
  (add-hook 'ruby-mode-hook 'rbenv-use-corresponding))

(use-package inf-ruby
  :defer t
  :general
  (general-define-key
   :prefix "SPC m"
   :non-normal-prefix "M-m"
   :states '(motion insert emacs)
   :keymaps 'ruby-mode-map
   "sf" 'ruby-send-definition
   "sF" 'ruby-send-definition-and-go
   "sr" 'ruby-send-region
   "sR" 'ruby-send-region-and-go))

(use-package robe
  :delight
  :defer t
  :init
  (add-hook 'ruby-mode-hook 'robe-mode))

(use-package bundler
  :defer t
  :general
  (general-define-key
   :prefix "SPC m"
   :non-normal-prefix "M-m"
   :states '(motion insert emacs)
   :keymaps 'ruby-mode-map
   "bc" 'bundle-check
   "bi" 'bundle-install
   "bs" 'bundle-console
   "bu" 'bundle-update
   "bx" 'bundle-exec
   "bo" 'bundle-open))

(use-package rspec-mode
  :delight
  :defer t
  :config
  (progn
    (defun ruby/rspec-verify-directory (dir)
      "Launch tests in DIR directory.
Called interactively it prompts for a directory."
      (interactive "Drspec directory: ")
      (rspec-run-single-file dir (rspec-core-options))))
  :general
  (general-define-key
   :prefix "SPC m"
   :non-normal-prefix "M-m"
   :states '(motion insert emacs)
   :keymaps 'ruby-mode-map
   "ta"    'rspec-verify-all
   "tb"    'rspec-verify
   "tc"    'rspec-verify-continue
   "td"    'ruby/rspec-verify-directory
   "te"    'rspec-toggle-example-pendingness
   "tf"    'rspec-verify-method
   "tl"    'rspec-run-last-failed
   "tm"    'rspec-verify-matching
   "tr"    'rspec-rerun
   "tt"    'rspec-verify-single
   "t~"    'rspec-toggle-spec-and-target-find-example
   "t TAB" 'rspec-toggle-spec-and-target))

(use-package rubocop
  :delight
  :defer t
  :init
  (add-hook 'ruby-mode-hook 'rubocop-mode)
  :general
  (general-define-key
   :prefix "SPC m"
   :non-normal-prefix "M-m"
   :states '(motion insert emacs)
   :keymaps 'ruby-mode-map
   "rrd" 'rubocop-check-directory
   "rrD" 'rubocop-autocorrect-directory
   "rrf" 'rubocop-check-current-file
   "rrF" 'rubocop-autocorrect-current-file
   "rrp" 'rubocop-check-project
   "rrP" 'rubocop-autocorrect-project))

(use-package projectile-rails
  :delight
  :after projectile
  :init
  (projectile-rails-global-mode))

(use-package ruby-tools
  :delight
  :defer t
  :init
  (add-hook 'ruby-mode-hook 'ruby-tools-mode)
  :general
  (general-define-key
   :prefix "SPC m"
   :non-normal-prefix "M-m"
   :states '(motion insert emacs)
   :keymaps 'ruby-mode-map
   "x\'" 'ruby-tools-to-single-quote-string
   "x\"" 'ruby-tools-to-double-quote-string
   "x:" 'ruby-tools-to-symbol))

(use-package rake
  :defer t
  :init
  (progn
    (setq rake-cache-file (concat private/cache-directory "rake.cache"))
    (setq rake-completion-system 'default))
  :general
  (general-define-key
   :prefix "SPC m"
   :non-normal-prefix "M-m"
   :states '(motion insert emacs)
   :keymaps 'ruby-mode-map
   "kk"    'rake
   "kr"    'rake-rerun
   "kR"    'rake-regenerate-cache
   "kf"    'rake-find-task))

(provide 'init-ruby)
