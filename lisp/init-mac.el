(setq mac-pass-command-to-system nil)
(mac-auto-operator-composition-mode t)

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
   "dw" 'osx-dictionary-search-pointer
   "dW" 'osx-dictionary-search-input)
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

(use-package mac-win
  :ensure nil
  :config
  (progn
    (setq mac-frame-tabbing nil)

    (defun private/mac-new-tab ()
      "Create a new tab on mac"
      (interactive)
      (let ((mac-frame-tabbing t))
        (new-frame)))

    (defun private/mac-kill-tab ()
      "Kill a tab (Just an alias for delete-frame since a tab on
mac is actually just a frame)"
      (interactive)
      (delete-frame)))
  :general
  (general-nmap
   "gt" 'mac-next-tab
   "gT" 'mac-previous-tab)

  (private/set-leader-keys
   "wt" 'private/mac-new-tab
   "qt" 'private/mac-kill-tab
   "td" 'mac-move-tab-to-new-frame))

(provide 'init-mac)
