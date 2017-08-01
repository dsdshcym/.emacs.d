(use-package flycheck
  :delight
  :defer t
  :init
  (progn
    (setq flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc))
    (global-flycheck-mode)))

(provide 'init-syntax-checking)
