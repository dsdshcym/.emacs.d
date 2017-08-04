(use-package dash-at-point
  :defer t
  :general
  (private/set-leader-keys
   "dd" 'dash-at-point
   "dD" 'dash-at-point-with-docset))

(use-package osx-dictionary
  :commands (osx-dictionary-search-pointer
             osx-dictionary-search-input
             osx-dictionary-cli-find-or-recompile)
  :general
  (private/set-leader-keys
   "xd" 'osx-dictionary-search-pointer
   "xD" 'osx-dictionary-search-input)
  (general-evil-define-key 'normal 'osx-dictionary-mode-map
   "q" 'osx-dictionary-quit
   "r" 'osx-dictionary-read-word
   "s" 'osx-dictionary-search-input
   "o" 'osx-dictionary-open-dictionary.app))

(use-package osx-browse
  :pin manual
  :commands 'osx-browse-url
  :init
  (progn
    (setq osx-browse-prefer-background t)
    (setq browse-url-browser-function 'osx-browse-url)))

(provide 'init-mac)
