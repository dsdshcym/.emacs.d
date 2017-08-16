(use-package ruby-mode
  :delight
  :defer t
  :mode (("Appraisals\\'" . ruby-mode)
         ("Puppetfile" . ruby-mode))
  :general
  (private/set-leader-keys-for-mode
   :keymaps 'ruby-mode-map
   "r'" 'ruby-toggle-string-quotes
   "r{" 'ruby-toggle-block))

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
  :init
  (defadvice rspec-enable-appropriate-mode (around enable-rspec-when-it-is-current-test-framework)
    (if (eq (private/projectile-ruby-test-framework) 'rspec)
        ad-do-it))
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
   :keymaps '(rspec-mode-map rspec-verifiable-mode-map)
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

(use-package minitest
  :defer t
  :init
  (defadvice minitest-enable-appropriate-mode (around enable-minitest-when-it-is-current-test-framework)
    (if (eq (private/projectile-ruby-test-framework) 'minitest)
        ad-do-it))
  :config
  (push '(minitest-compilation-mode :dedicated t :position bottom :stick t :noselect t :height 0.4)
        popwin:special-display-config)
  :general
  (private/set-leader-keys-for-mode
   :keymaps 'minitest-mode-map
   "ta" 'minitest-verify-all
   "tb" 'minitest-verify
   "tr" 'minitest-rerun
   "tt" 'minitest-verify-single))

(use-package ruby-test-mode
  :defer t
  :init
  (add-hook 'ruby-mode
            (lambda ()
              (if (eq (private/projectile-ruby-test-framework) 'ruby-test)
                  (ruby-test-mode))))
  :general
  (private/set-leader-keys-for-mode
   :keymaps 'ruby-test-mode-map
   "tb" 'ruby-test-run
   "tt" 'ruby-test-run-at-point
   "t TAB" 'ruby-test-toggle-implementation-and-specification))

(defun private/projectile-ruby-test-framework ()
  "Return current test framework. Either 'minitest or 'rspec"
  (interactive)
  (cond
   ((projectile-file-exists-p (concat (projectile-project-root) "spec")) 'rspec)
   ((projectile-file-exists-p (concat (projectile-project-root) "test")) 'minitest)
   (t 'ruby-test)))

(use-package rubocop
  :delight
  :defer t
  :init
  (add-hook 'ruby-mode-hook 'rubocop-mode)
  :general
  (private/set-leader-keys-for-mode
   :keymaps 'ruby-mode-map
   "cd" 'rubocop-check-directory
   "cD" 'rubocop-autocorrect-directory
   "cf" 'rubocop-check-current-file
   "cF" 'rubocop-autocorrect-current-file
   "cp" 'rubocop-check-project
   "cP" 'rubocop-autocorrect-project))

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
   "fa" 'projectile-rails-find-locale
   "fc" 'projectile-rails-find-controller
   "fe" 'projectile-rails-find-environment
   "ff" 'projectile-rails-find-feature
   "fh" 'projectile-rails-find-helper
   "fi" 'projectile-rails-find-initializer
   "fj" 'projectile-rails-find-javascript
   "fl" 'projectile-rails-find-lib
   "fm" 'projectile-rails-find-model
   "fn" 'projectile-rails-find-migration
   "fo" 'projectile-rails-find-log
   "fp" 'projectile-rails-find-spec
   "fr" 'projectile-rails-find-rake-task
   "fs" 'projectile-rails-find-stylesheet
   "ft" 'projectile-rails-find-test
   "fu" 'projectile-rails-find-fixture
   "fv" 'projectile-rails-find-view
   "fy" 'projectile-rails-find-layout
   "f@" 'projectile-rails-find-mailer
   ;; Goto file
   "Fc" 'projectile-rails-find-current-controller
   "Fd" 'projectile-rails-goto-schema
   "Fe" 'projectile-rails-goto-seeds
   "Fh" 'projectile-rails-find-current-helper
   "Fj" 'projectile-rails-find-current-javascript
   "Fg" 'projectile-rails-goto-gemfile
   "Fm" 'projectile-rails-find-current-model
   "Fn" 'projectile-rails-find-current-migration
   "Fp" 'projectile-rails-find-current-spec
   "Fr" 'projectile-rails-goto-routes
   "Fs" 'projectile-rails-find-current-stylesheet
   "Ft" 'projectile-rails-find-current-test
   "Fu" 'projectile-rails-find-current-fixture
   "Fv" 'projectile-rails-find-current-view
   "Fz" 'projectile-rails-goto-spec-helper
   "F." 'projectile-rails-goto-file-at-point
   ;; Rails external commands
   "pk" 'projectile-rails-rake
   "pg" 'projectile-rails-generate
   "pi" 'projectile-rails-console
   "ps" 'projectile-rails-server
   ;; Refactoring 'projectile-rails-mode
   "rx" 'projectile-rails-extract-region))

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

(use-package ruby-refactor
  :defer t
  :pin manual
  :init
  (add-hook 'ruby-mode-hook 'ruby-refactor-mode-launch)
  :config
  (setq ruby-refactor-add-parens t)
  :general
  (private/set-leader-keys-for-mode
   :keymaps 'ruby-mode-map
   "rm" 'ruby-refactor-extract-to-method
   "rv" 'ruby-refactor-extract-local-variable
   "rc" 'ruby-refactor-convert-post-conditional
   "rC" 'ruby-refactor-extract-constant
   "rp" 'ruby-refactor-add-parameter
   "rl" 'ruby-refactor-extract-to-let))

(provide 'init-ruby)
