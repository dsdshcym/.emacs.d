(use-package dash-at-point
  :defer t
  :general
  (general-define-key
   :prefix "SPC"
   :non-normal-prefix "M-m"
   :keymaps '(motion insert emacs)
   "dd" 'dash-at-point
   "dD" 'dash-at-point-with-docset))

(use-package osx-dictionary
  :commands (osx-dictionary-search-pointer
             osx-dictionary-search-input
             osx-dictionary-cli-find-or-recompile)
  :general
  (general-define-key
   :prefix "SPC"
   :non-normal-prefix "M-m"
   :keymaps '(motion insert emacs)
   "xd" 'osx-dictionary-search-pointer
   "xD" 'osx-dictionary-search-input)
  (general-evil-define-key 'normal 'osx-dictionary-mode-map
   "q" 'osx-dictionary-quit
   "r" 'osx-dictionary-read-word
   "s" 'osx-dictionary-search-input
   "o" 'osx-dictionary-open-dictionary.app))

(provide 'init-mac)
