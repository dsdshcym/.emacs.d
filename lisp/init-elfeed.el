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
  (general-nmap
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
  (general-nmap
   :keymaps 'elfeed-show-mode-map
   "C-n" 'elfeed-show-next
   "C-p" 'elfeed-show-prev
   "y"   'elfeed-show-yank
   "b"   'elfeed-show-visit
   "q"   'quit-window)
  (private/set-leader-keys
   "af"  'elfeed))

(use-package elfeed-org
  :defer t
  :init
  (progn
    (setq rmh-elfeed-org-files '("~/Org/rss_feed.org"))
    (elfeed-org))
  :config
  (progn
    (defun rmh-elfeed-org-convert-tree-to-headlines (parsed-org)
      (org-element-map parsed-org 'headline
        (lambda (h)
          (let* ((heading (org-element-property :raw-value h))
                 (tags (mapcar 'intern (org-element-property :tags h))))
            (-concat (list heading) tags)))))

    (defun rmh-elfeed-org-filter-relevant (list)
      "Filter relevant entries from the LIST."
      (-filter
       (lambda (entry)
         (and
          (string-match "\\(http\\|entry-title\\)" (car entry))
          (member (intern rmh-elfeed-org-tree-id) entry)
          (not (member (intern rmh-elfeed-org-ignore-tag) entry))))
       list))))

(provide 'init-elfeed)
