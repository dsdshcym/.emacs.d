(use-package dash-at-point
  :defer t
  :general
  (general-define-key
   :prefix "SPC"
   :non-normal-prefix "M-m"
   :keymaps '(motion insert emacs)
   "dd" 'dash-at-point
   "dD" 'dash-at-point-with-docset))

(provide 'init-mac)
