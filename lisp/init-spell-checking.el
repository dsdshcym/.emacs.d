(use-package ispell
  :ensure nil
  :init
  (progn
    (setq ispell-program-name "hunspell"
          ispell-dictionary "en_US")))

(use-package flyspell
  :defer t
  :init
  (progn
    (setq flyspell-issue-message-flag nil)
    (setq flyspell-prog-text-faces
          '(font-lock-comment-face font-lock-doc-face))
    (add-hook 'prog-mode-hook 'flyspell-prog-mode)
    (add-hook 'text-mode-hook 'flyspell-mode)
    (add-hook 'org-mode-hook 'flyspell-mode))
  :general
  (private/set-leader-keys
   "Sb" 'flyspell-buffer
   "Sn" 'flyspell-goto-next-error))

(use-package flyspell-correct
  :commands (flyspell-correct-word-generic
             flyspell-correct-previous-word-generic)
  :general
  (general-define-key
   :keymaps 'flyspell-mode-map
   "C-;" 'flyspell-correct-previous-word-generic)
  (private/set-leader-keys
   "Sc" 'flyspell-correct-previous-word-generic))

(use-package flyspell-correct-ivy
  :commands (flyspell-correct-ivy)
  :init
  (setq flyspell-correct-interface #'flyspell-correct-ivy))

(provide 'init-spell-checking)
