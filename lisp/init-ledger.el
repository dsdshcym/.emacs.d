(use-package ledger-mode
  :mode ("\\.\\(ledger\\|ldg\\)\\'" . ledger-mode)
  :defer t
  :init
  (progn
    (setq ledger-post-amount-alignment-column 62))
  :config
  (with-eval-after-load company-mode
    (add-to-list 'company-backends 'company-capf))
  :general
  (general-define-key
   :prefix "SPC m"
   :non-normal-prefix "M-m"
   :states '(motion insert emacs)
   :keymaps 'ledger-mode-map
   "a" 'ledger-add-transaction
   "b" 'ledger-post-edit-amount
   "c" 'ledger-mode-clean-buffer
   "C" 'ledger-toggle-current
   "d" 'ledger-delete-current-transaction
   "l" 'ledger-display-ledger-stats
   "m" 'ledger-set-month
   "p" 'ledger-display-balance-at-point
   "q" 'ledger-post-align-xact
   "r" 'ledger-report
   "R" 'ledger-reconcile
   "t" 'ledger-insert-effective-date
   "y" 'ledger-set-year)
  (general-define-key
   :prefix "SPC m"
   :non-normal-prefix "M-m"
   :states '(motion insert emacs)
   :keymaps 'ledger-reconcile-mode-map
   "'" 'ledger-reconcile-toggle
   "a" 'ledger-reconcile-add
   "q" 'ledger-reconcile-quit
   "t" 'ledger-reconcile-change-target
   "RET" 'ledger-reconcile-finish))

(use-package flycheck-ledger
  :after ledger-mode)

(provide 'init-ledger)
