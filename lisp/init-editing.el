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

(provide 'init-editing)
