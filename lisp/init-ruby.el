(use-package ruby-mode
  :delight
  :defer t
  :mode (("Appraisals\\'" . ruby-mode)
         ("Puppetfile" . ruby-mode))
  :general
  (private/set-leader-keys-for-mode
   :keymaps 'ruby-mode-map
   "'" 'ruby-toggle-string-quotes
   "{" 'ruby-toggle-block))

(use-package rbenv
  :defer t
  :init
  (global-rbenv-mode)
  :config
  (add-hook 'ruby-mode-hook 'rbenv-use-corresponding))

;; (use-package xmpfilter)

(use-package inf-ruby
  :defer t
  :general
  (private/set-leader-keys-for-mode
   :keymaps 'ruby-mode-map
   "sf" 'ruby-send-definition
   "sF" 'ruby-send-definition-and-go
   "sr" 'ruby-send-region
   "sR" 'ruby-send-region-and-go))

(use-package robe
  :delight
  :defer t
  :init
  (progn
    (add-hook 'ruby-mode-hook 'robe-mode)
    (add-hook 'robe-mode-hook
              (lambda ()
                (make-local-variable 'company-backends)
                (add-to-list 'company-backends 'company-robe))))
  :general
  (private/set-leader-keys-for-mode
   :keymaps 'ruby-mode-map
   "hh" 'robe-doc
   "rsr" 'robe-rails-refresh
   "gg" 'robe-jump
   "si" 'robe-start
   "ss" 'ruby-switch-to-inf))

(use-package bundler
  :defer t
  :general
  (private/set-leader-keys-for-mode
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
      (rspec-run-single-file dir (rspec-core-options)))

    (push '(rspec-compilation-mode :dedicated t :position bottom :stick t :noselect t :height 0.4)
          popwin:special-display-config))
  :general
  (private/set-leader-keys-for-mode
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
  (private/set-leader-keys-for-mode
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
  (projectile-rails-global-mode)
  :config
  (push '(projectile-rails-compilation-mode :dedicated t :position bottom :stick t :noselect t :height 0.4)
        popwin:special-display-config)
  :general
  (private/set-leader-keys-for-mode
   :keymaps 'projectile-rails-mode-map
   "rfa" 'projectile-rails-find-locale
   "rfc" 'projectile-rails-find-controller
   "rfe" 'projectile-rails-find-environment
   "rff" 'projectile-rails-find-feature
   "rfh" 'projectile-rails-find-helper
   "rfi" 'projectile-rails-find-initializer
   "rfj" 'projectile-rails-find-javascript
   "rfl" 'projectile-rails-find-lib
   "rfm" 'projectile-rails-find-model
   "rfn" 'projectile-rails-find-migration
   "rfo" 'projectile-rails-find-log
   "rfp" 'projectile-rails-find-spec
   "rfr" 'projectile-rails-find-rake-task
   "rfs" 'projectile-rails-find-stylesheet
   "rft" 'projectile-rails-find-test
   "rfu" 'projectile-rails-find-fixture
   "rfv" 'projectile-rails-find-view
   "rfy" 'projectile-rails-find-layout
   "rf@" 'projectile-rails-find-mailer
   ;; Goto file
   "rgc" 'projectile-rails-find-current-controller
   "rgd" 'projectile-rails-goto-schema
   "rge" 'projectile-rails-goto-seeds
   "rgh" 'projectile-rails-find-current-helper
   "rgj" 'projectile-rails-find-current-javascript
   "rgg" 'projectile-rails-goto-gemfile
   "rgm" 'projectile-rails-find-current-model
   "rgn" 'projectile-rails-find-current-migration
   "rgp" 'projectile-rails-find-current-spec
   "rgr" 'projectile-rails-goto-routes
   "rgs" 'projectile-rails-find-current-stylesheet
   "rgt" 'projectile-rails-find-current-test
   "rgu" 'projectile-rails-find-current-fixture
   "rgv" 'projectile-rails-find-current-view
   "rgz" 'projectile-rails-goto-spec-helper
   "rg." 'projectile-rails-goto-file-at-point
   ;; Rails external commands
   "r:" 'projectile-rails-rake
   "rc" 'projectile-rails-generate
   "ri" 'projectile-rails-console
   "rs" 'projectile-rails-server
   ;; Refactoring 'projectile-rails-mode
   "rRx" 'projectile-rails-extract-region))

(use-package ruby-tools
  :delight
  :defer t
  :init
  (add-hook 'ruby-mode-hook 'ruby-tools-mode)
  :general
  (private/set-leader-keys-for-mode
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
  :config
  (push '(rake-compilation-mode :dedicated t :position bottom :stick t :noselect t :height 0.4)
        popwin:special-display-config)
  :general
  (private/set-leader-keys-for-mode
   :keymaps 'ruby-mode-map
   "kk"    'rake
   "kr"    'rake-rerun
   "kR"    'rake-regenerate-cache
   "kf"    'rake-find-task))

(provide 'init-ruby)
