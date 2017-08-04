(use-package elfeed
  :defer t
  :config
  (progn
    (defun private/org-elfeed-entry-store-link ()
      (interactive)
      (when (and (boundp 'elfeed-show-entry) elfeed-show-entry)
        (let* ((link (elfeed-entry-link elfeed-show-entry))
               (title (elfeed-entry-title elfeed-show-entry)))
          (message title)
          (org-store-link-props
           :link link
           :description title)
          )))
    (add-hook 'org-store-link-functions 'private/org-elfeed-entry-store-link))
  :general
  (general-define-key
   :states 'normal
   :keymaps 'elfeed-search-mode-map
   "RET" 'elfeed-search-show-entry
   "s"   'elfeed-search-live-filter
   "b"   'elfeed-search-browse-url
   "c"   'elfeed-db-compact
   "r"   'elfeed-search-untag-all-unread
   "u"   'elfeed-search-tag-all-unread
   "y"   'elfeed-search-yank
   "gr"  'elfeed-update
   "gR"  'elfeed-search-update--force
   "gu"  'elfeed-unjam
   "q"   'quit-window)
  (general-define-key
   :states 'normal
   :keymaps 'elfeed-show-mode-map
   "C-n" 'elfeed-show-next
   "C-p" 'elfeed-show-prev
   "y"   'elfeed-show-yank
   "b"   'elfeed-show-visit
   "q"   'quit-window)
  (general-define-key
   :prefix "SPC"
   :non-normal-prefix "M-m"
   :keymaps '(motion insert emacs)
   "af"  'elfeed))

(use-package elfeed-goodies
  :after elfeed
  :init
  (progn
    (setq elfeed-goodies/entry-pane-position 'bottom)
    (elfeed-goodies/setup)))

(use-package elfeed-org
  :defer t
  :init
  (progn
    (setq rmh-elfeed-org-files '("~/Org/rss_feed.org"))
    (elfeed-org)))

(provide 'init-elfeed)
