(use-package slim-mode
  :defer t
  :mode ("\\.slim\\'" . slim-mode))

(use-package company-css
  :ensure company
  :defer t
  :init
  (add-hook 'css-mode-hook
              (lambda ()
                (make-local-variable 'company-backends)
                (add-to-list 'company-backends company-css))))

(use-package company-web
  :defer t
  :init
  (progn
    (add-hook 'slim-mode-hook
              (lambda ()
                (make-local-variable 'company-backends)
                (add-to-list 'company-backends 'company-web-slim)))
    (add-hook 'html-mode-hook
              (lambda ()
                (make-local-variable 'company-backends)
                (add-to-list 'company-backends 'company-web-html)))))

(provide 'init-html)
