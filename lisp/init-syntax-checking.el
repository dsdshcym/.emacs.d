(use-package flycheck
  :defer t
  :init
  (global-flycheck-mode)
  :config
  (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc))

(provide 'init-syntax-checking)
