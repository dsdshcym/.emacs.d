(use-package mu4e
  :ensure nil
  :commands (mu4e mu4e-compose-new)
  :init
  (setq url-mail-command 'mu4e-compose-new)
  :config
  (progn
    (evil-set-initial-state 'mu4e-main-mode 'normal)
    (evil-set-initial-state 'mu4e-headers-mode 'normal)
    (evil-set-initial-state 'mu4e-view-mode 'normal)

    (setq mu4e-completing-read-function 'completing-read)
    (setq mu4e-confirm-quit nil)

    ;; default
    (setq mu4e-maildir (expand-file-name "~/Maildir"))

    (setq mu4e-refile-folder "/gmail/all mail")
    (setq mu4e-drafts-folder "/gmail/drafts")
    (setq mu4e-sent-folder   "/gmail/sent")
    (setq mu4e-trash-folder  "/gmail/trash")

    ;;rename files when moving
    ;;NEEDED FOR MBSYNC
    (setq mu4e-change-filenames-when-moving t)

    ;; don't save message to Sent Messages, GMail/IMAP will take care of this
    (setq mu4e-sent-messages-behavior 'delete)

    (setq mu4e-update-interval 1800)

    (setq mu4e-html2text-command "pandoc -f html -t plain --normalize")

    ;; setup some handy shortcuts
    (setq mu4e-maildir-shortcuts
          '(("/gmail/inbox"    . ?i)
            ("/gmail/sent"     . ?s)
            ("/gmail/trash"    . ?t)
            ("/gmail/all mail" . ?a)
            ("/ekohe/inbox"    . ?w)))

    (setq mu4e-bookmarks
          '(
            ("flag:unread AND NOT flag:trashed AND NOT maildir:\"/gmail/all mail\" AND NOT maildir:\"/ekohe/all mail\""
             "Unread messages"  ?u)
            ("date:today..now AND NOT maildir:\"/gmail/all mail\" AND NOT maildir:\"/ekohe/all mail\""
             "Today's messages" ?t)
            ("date:7d..now AND NOT maildir:\"/gmail/all mail\" AND NOT maildir:\"/ekohe/all mail\""
             "Last 7 days"      ?w)
            )
          )

    (setq mu4e-get-mail-command "mbsync -a")

    (setq mu4e-attachment-dir "~/Downloads")

    (setq mu4e-view-prefer-html nil)

    ;; Store link to message if in header view, not to header query
    (setq org-mu4e-link-query-in-headers-mode nil)

    (setq mu4e-org-contacts-file (expand-file-name "~/Org/contacts.org"))
    (eval-after-load 'mu4e-headers
      (lambda ()
        (add-to-list 'mu4e-headers-actions
                     '("org-contact-add" . mu4e-action-add-org-contact) t)))
    (eval-after-load 'mu4e-view
      (lambda ()
        (add-to-list 'mu4e-view-actions
                     '("org-contact-add" . mu4e-action-add-org-contact) t)))

    (setq message-send-mail-function 'smtpmail-send-it
          starttls-use-gnutls t
          smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
          smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg")
          smtpmail-default-smtp-server "smtp.gmail.com"
          smtpmail-smtp-server "smtp.gmail.com"
          smtpmail-smtp-service 587)

    (defvar my-mu4e-account-alist
      '(("gmail"
         (mu4e-sent-folder   "/gmail/sent")
         (mu4e-drafts-folder "/gmail/drafts")
         (user-mail-address "dsdshcym@gmail.com")
         (smtpmail-default-smtp-server "smtp.gmail.com")
         (smtpmail-smtp-user "dsdshcym")
         (smtpmail-smtp-server "smtp.gmail.com")
         ;; (smtpmail-stream-type starttls)
         (smtpmail-smtp-service 587))
        ("ekohe"
         (mu4e-sent-folder   "/ekohe/sent")
         (mu4e-drafts-folder "/ekohe/drafts")
         (user-mail-address "yiming@ekohe.com")
         (smtpmail-default-smtp-server "smtp.gmail.com")
         (smtpmail-smtp-user "yiming@ekohe.com")
         (smtpmail-smtp-server "smtp.gmail.com")
         ;; (smtpmail-stream-type starttls)
         (smtpmail-smtp-service 587))
        ))

    (defun my-mu4e-set-account ()
      "Set the account for composing a message."
      (let* ((account
              (if mu4e-compose-parent-message
                  (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                    (string-match "/\\(.*?\\)/" maildir)
                    (match-string 1 maildir))
                (completing-read (format "Compose with account: (%s) "
                                         (mapconcat #'(lambda (var) (car var))
                                                    my-mu4e-account-alist "/"))
                                 (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                                 nil t nil nil (caar my-mu4e-account-alist))))
             (account-vars (cdr (assoc account my-mu4e-account-alist))))
        (if account-vars
            (mapc #'(lambda (var)
                      (set (car var) (cadr var)))
                  account-vars)
          (error "No email account found"))))

    (add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)

    (add-to-list 'mu4e-view-actions
                 '("browser" . mu4e-action-view-in-browser)))
  :general
  (private/set-leader-keys
   "am" 'mu4e
   "aM" 'mu4e-compose-new)
  (general-evil-define-key 'normal 'mu4e-main-mode-map
    "j" 'mu4e~headers-jump-to-maildir
    "s" 'mu4e-headers-search
    "C" 'mu4e-compose-new
    "b" 'mu4e-headers-search-bookmark
    "u" 'mu4e-update-mail-and-index
    "q" 'mu4e-quit)
  (general-evil-define-key 'normal 'mu4e-headers-mode-map
    "RET" 'mu4e-headers-view-message
    "!"   'mu4e-headers-mark-for-read
    "#"   'mu4e-mark-resolve-deferred-marks
    "$"   'mu4e-show-log
    "%"   'mu4e-headers-mark-pattern
    "&"   'mu4e-headers-mark-custom
    "*"   'mu4e-headers-mark-for-something
    "+"   'mu4e-headers-mark-for-flag
    "-"   'mu4e-headers-mark-for-unflag
    "/"   'mu4e-headers-search-narrow
    ";"   'mu4e-context-switch
    "="   'mu4e-headers-mark-for-untrash
    "?"   'mu4e-headers-mark-for-unread
    "A"   'mu4e-headers-mark-for-action
    "B"   'mu4e-headers-search-bookmark-edit
    "C"   'mu4e-compose-new
    "D"   'mu4e-headers-mark-for-delete
    "E"   'mu4e-compose-edit
    "F"   'mu4e-compose-forward
    "H"   'mu4e-display-manual
    "O"   'mu4e-headers-change-sorting
    "P"   'mu4e-headers-toggle-threading
    "Q"   'mu4e-headers-toggle-full-search
    "R"   'mu4e-compose-reply
    "S"   'mu4e-headers-search-edit
    "T"   'mu4e-headers-mark-thread
    "U"   'mu4e-mark-unmark-all
    "V"   'mu4e-headers-toggle-skip-duplicates
    "W"   'mu4e-headers-toggle-include-related
    "["   'mu4e-headers-prev-unread
    "\\"  'mu4e-headers-query-prev
    "]"   'mu4e-headers-next-unread
    "a"   'mu4e-headers-action
    "b"   'mu4e-headers-search-bookmark
    "d"   'mu4e-headers-mark-for-trash
    "g"   'mu4e-headers-rerun-search
    ;; "j"   'mu4e~headers-jump-to-maildir
    "j"   'mu4e-headers-next
    "J"   'mu4e~headers-jump-to-maildir
    "m"   'mu4e-headers-mark-for-move
    "n"   'mu4e-headers-next
    "p"   'mu4e-headers-prev
    "q"   'mu4e~headers-quit-buffer
    "r"   'mu4e-headers-mark-for-refile
    "s"   'mu4e-headers-search
    "t"   'mu4e-headers-mark-subthread
    "u"   'mu4e-headers-mark-for-unmark
    "x"   'mu4e-mark-execute-all
    "y"   'mu4e-select-other-view
    "k"   'mu4e-headers-prev
    "q"   'mu4e~headers-quit-buffer)
  (general-evil-define-key 'normal 'mu4e-view-mode-map
    "!"               'mu4e-view-mark-for-read
    "#"               'mu4e-view-toggle-hide-cited
    "$"               'mu4e-show-log
    "%"               'mu4e-view-mark-pattern
    "&"               'mu4e-view-mark-custom
    "*"               'mu4e-view-mark-for-something
    "+"               'mu4e-view-mark-for-flag
    "-"               'mu4e-view-mark-for-unflag
    "."               'mu4e-view-raw-message
    "/"               'mu4e-view-search-narrow
    ";"               'mu4e-context-switch
    "="               'mu4e-view-mark-for-untrash
    "?"               'mu4e-view-mark-for-unread
    "A"               'mu4e-view-attachment-action
    "B"               'mu4e-headers-search-bookmark-edit
    "C"               'mu4e-compose-new
    "D"               'mu4e-view-mark-for-delete
    "E"               'mu4e-compose-edit
    "F"               'mu4e-compose-forward
    "H"               'mu4e-display-manual
    "O"               'mu4e-headers-change-sorting
    "P"               'mu4e-headers-toggle-threading
    "Q"               'mu4e-headers-toggle-full-search
    "R"               'mu4e-compose-reply
    "S"               'mu4e-view-search-edit
    "T"               'mu4e-view-mark-thread
    "U"               'mu4e-view-unmark-all
    "W"               'mu4e-headers-toggle-include-related
    "["               'mu4e-view-headers-prev-unread
    "]"               'mu4e-view-headers-next-unread
    "a"               'mu4e-view-action
    "b"               'mu4e-headers-search-bookmark
    "d"               'mu4e-view-mark-for-trash
    "e"               'mu4e-view-save-attachment
    ;; "f"               'mu4e-view-fetch-url
    "g"               'mu4e-view-go-to-url
    "h"               'mu4e-view-toggle-html
    "J"               'mu4e~headers-jump-to-maildir
    ;; "k"               'mu4e-view-save-url
    "m"               'mu4e-view-mark-for-move
    "n"               'mu4e-view-headers-next
    "o"               'mu4e-view-open-attachment
    "p"               'mu4e-view-headers-prev
    "q"               'mu4e~view-quit-buffer
    "r"               'mu4e-view-mark-for-refile
    "s"               'mu4e-headers-search
    "t"               'mu4e-view-mark-subthread
    "u"               'mu4e-view-unmark
    "v"               'mu4e-view-verify-msg-popup
    "x"               'mu4e-view-marked-execute
    "y"               'mu4e-select-other-view
    "|"               'mu4e-view-pipe))

(use-package org-mu4e
  :ensure nil
  :after mu4e)

(provide 'init-mu4e)
