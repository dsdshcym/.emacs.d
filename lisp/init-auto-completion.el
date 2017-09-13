(use-package company
  :delight
  :defer t
  :init
  (progn
    (setq company-idle-delay nil
          company-minimum-prefix-length 2
          company-require-match nil
          company-tooltip-align-annotations t
          company-dabbrev-ignore-case nil
          company-dabbrev-downcase nil)
    (global-company-mode))
  :config
  (progn
    (setq company-backends
          '(company-keywords
            company-files
            company-capf
            company-dabbrev-code
            company-dabbrev)))
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
  :config
  (show-smartparens-mode)
  :commands (sp-split-sexp sp-newline sp-up-sexp)
  :general
  (general-imap
   "C-s" 'sp-forward-slurp-sexp
   "S-C-s" 'sp-forward-barf-sexp
   "M-s" 'sp-backward-slurp-sexp
   "S-M-s" 'sp-backward-barf-sexp))

(use-package smartparens-config
  :ensure smartparens)

(use-package yasnippet
  :commands (yas-reload-all yas-global-mode yas-minor-mode)
  :init
  (progn
    (yas-reload-all)
    (add-hook 'prog-mode-hook #'yas-minor-mode)))

(use-package hippie-expand
  :ensure nil
  :defer t
  :init
  (progn
    (defun private/evil-complete (arg)
      "Expand wih company mode if available, otherwise hippie-expand."
      (interactive)
      (unless (company-complete)
        (hippie-expand arg)))

    (defun private/hippie-expand-previous (arg)
      (interactive)
      (when he-tried-table
        (let ((l (length (car he-tried-table))))
          (when (string= (buffer-substring (- (point) l) (point))
                         (car he-tried-table))
            (delete-region (- (point) l) (point))
            (when (cadr he-tried-table)
              (insert (cadr he-tried-table)))
            (setq he-tried-table (cdr he-tried-table))))))

    (setq evil-complete-next-func 'private/evil-complete)
    (setq evil-complete-previous-func 'private/hippie-expand-previous)))

(provide 'init-auto-completion)
