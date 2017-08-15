(use-package doc-view
  :defer t
  :ensure nil
  :config
  (progn
    (evil-set-initial-state 'doc-view-mode 'normal)

    (setq doc-view-resolution 300)

    (defun private/doc-view-search-new-query ()
      "Initiate a new query."
      (interactive)
      (doc-view-search 'newquery))

    (defun private/doc-view-search-new-query-backward ()
      "Initiate a new query."
      (interactive)
      (doc-view-search 'newquery t))

    (defun private/doc-view-goto-page (&optional count)
      (interactive (list
                    (when current-prefix-arg
                      (prefix-numeric-value current-prefix-arg))))
      (if (null count)
          (doc-view-last-page)
        (doc-view-goto-page count))))
  :general
  (private/enable-leader-key-for-mode
   :keymaps 'doc-view-mode)
  (general-define-key
   :states 'normal
   :keymaps 'doc-view-mode-map
   "/"  'private/doc-view-search-new-query
   "?"  'private/doc-view-search-new-query-backward
   "gg" 'doc-view-first-page
   "G"  'private/doc-view-goto-page
   "gt" 'doc-view-goto-page
   "h"  'image-backward-hscroll
   "j"  'doc-view-next-line-or-next-page
   "k"  'doc-view-previous-line-or-previous-page
   "K"  'doc-view-kill-proc
   "l"  'image-forward-hscroll
   "p"  'doc-view-previous-page
   "n"  'doc-view-next-page
   "N"  'doc-view-search-backward
   "+"  'doc-view-enlarge
   "-"  'doc-view-shrink
   "0"  'doc-view-scale-reset
   "="  'doc-view-enlarge
   "H"  'doc-view-fit-height-to-window
   "K"  'doc-view-kill-proc
   "P"  'doc-view-fit-page-to-window
   "W"  'doc-view-fit-width-to-window
   "g"  'doc-view-revert-buffer
   "q"  'image-kill-buffer
   "r"  'doc-view-revert-buffer
   (kbd "C-d") 'doc-view-scroll-up-or-next-page
   (kbd "C-k") 'doc-view-kill-proc
   (kbd "C-u") 'doc-view-scroll-down-or-previous-page))

(use-package interleave
  :commands 'interleave)

(provide 'init-pdf)
