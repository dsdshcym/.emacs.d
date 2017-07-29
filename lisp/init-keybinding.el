(use-package evil
  :ensure t
  :init (progn
            (setq evil-want-C-u-scroll t)
            (evil-mode))
  :config (progn
            ;; esc should always quit: http://stackoverflow.com/a/10166400/61435
            (define-key evil-normal-state-map [escape] 'keyboard-quit)
            (define-key evil-visual-state-map [escape] 'keyboard-quit)
            (define-key minibuffer-local-map [escape] 'abort-recursive-edit)
            (define-key minibuffer-local-ns-map [escape] 'abort-recursive-edit)
            (define-key minibuffer-local-completion-map [escape] 'abort-recursive-edit)
            (define-key minibuffer-local-must-match-map [escape] 'abort-recursive-edit)
            (define-key minibuffer-local-isearch-map [escape] 'abort-recursive-edit)
            ;; Escape from isearch-mode("/" and "?" in evil-mode) like vim
            (define-key isearch-mode-map (kbd "<escape>") 'isearch-cancel)))

(provide 'init-keybinding)
