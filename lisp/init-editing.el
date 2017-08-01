(use-package avy
  :defer t
  :init
  (progn
    (setq avy-all-windows nil)
    (setq avy-background t))
  :general
  (general-define-key
   :prefix "SPC"
   :non-normal-prefix "M-m"
   :keymaps '(motion insert emacs)
   "jb" 'avy-pop-mark
   "jj" 'evil-avy-goto-char
   "jJ" 'evil-avy-goto-char-2
   "jl" 'evil-avy-goto-line
   "jw" 'evil-avy-goto-word-or-subword-1))

(use-package undo-tree
  :delight
  :defer t
  :init
  (global-undo-tree-mode)
  :config
  (progn
    (setq undo-tree-history-directory-alist `(("." . ,(concat private/cache-directory "undo-tree-history"))))
    (setq undo-tree-auto-save-history t)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

(use-package saveplace
  :init
  (progn
    (setq save-place-file (concat private/cache-directory "places"))
    (save-place-mode)
    ))

(provide 'init-editing)
