(use-package company
  :delight
  :defer t
  :init
  (progn
    (setq company-idle-delay 0.2
          company-minimum-prefix-length 2
          company-require-match nil
          company-dabbrev-ignore-case nil
          company-dabbrev-downcase nil)
    (global-company-mode))
  :general
  (general-define-key
   :keymaps 'company-active-map
   "C-n" 'company-select-next
   "C-p" 'company-select-previous))

(use-package company-statistics
  :defer t
  :init
  (progn
    (setq company-statistics-file (concat private/cache-directory
                                          "company-statistics-cache.el"))
    (add-hook 'company-mode-hook 'company-statistics-mode)))

(use-package smartparens
  :delight
  :defer t
  :init (smartparens-global-mode)
  :commands (sp-split-sexp sp-newline sp-up-sexp))

(use-package smartparens-config
  :ensure smartparens)

(provide 'init-auto-completion)
