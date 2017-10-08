(use-package org
  :ensure org-plus-contrib
  :defer t
  :init
  (progn
    (add-hook 'org-mode-hook 'auto-fill-mode)

    (setq org-directory "~/Org")
    (setq org-default-notes-file "~/Org/refile.org")
    (setq org-publish-timestamp-directory (concat private/cache-directory
                                                  ".org-timestamps/")
          ;; this is consistent with the value of
          ;; `helm-org-headings-max-depth'.
          org-imenu-depth 8
          org-todo-keywords
          '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
            (sequence "READING(r)" "TOREVIEW(v!/!)" "|" "READ(R!/!)")
            (sequence "PENDING(p)" "|" "MERGED(m!/!)" "CANCELLED(c@/!)")
            (sequence "WAITING(w@/!)" "SOMEDAY(s@/!)" "|" "CANCELLED(c@/!)")))
    (setq org-return-follows-link t)

    (setq org-enforce-todo-dependencies nil)
    (setq org-yank-adjusted-subtrees t)
    (setq org-blank-before-new-entry '((heading . nil)
                                       (plain-list-item . nil)))

    (setq org-modules '(org-crypt
                        org-docview
                        org-habit
                        org-id
                        org-info
                        org-irc
                        org-mhe
                        org-protocol
                        org-rmail
                        org-w3m
                        org-bullets
                        org-eww
                        org-mac-link
                        org-contacts))
    (eval-after-load 'org
      '(org-load-modules-maybe t))

    ;; -----------------------------
    ;; Refile
    ;; -----------------------------
    (defun private/opened-buffer-files ()
      "Return the list of files currently opened in emacs"
      (delq nil
            (mapcar (lambda (x)
                      (if (and (buffer-file-name x)
                               (string-match "\\.org$"
                                             (buffer-file-name x)))
                          (buffer-file-name x)))
                    (buffer-list))))

    (setq org-refile-targets '((private/opened-buffer-files :maxlevel . 9)))
    (setq org-refile-use-outline-path 'file)
    (setq org-outline-path-complete-in-steps nil)
    (setq org-refile-allow-creating-parent-nodes 'confirm)
    (setq org-refile-use-cache t)
    (run-with-idle-timer 300 t (lambda ()
                                 (org-refile-cache-clear)
                                 (org-refile-get-targets)))

    (setq org-log-into-drawer "LOGBOOK")
    (setq org-log-reschedule 'note)
    (setq org-log-redeadline 'note)
    (setq org-log-done 'time)
    (setq org-log-note-headings
          '((done . "CLOSING NOTE %t")
            (state . "State %-12s from %-12S %t")
            (note . "Note taken on %t")
            (reschedule . "Rescheduled from %S to %s on %t")
            (delschedule . "Not scheduled, was %S on %t")
            (redeadline . "New deadline from %S to %s on %t")
            (deldeadline . "Removed deadline, was %S on %t")
            (refile . "Refiled on %t")
            (clock-out . "")))

    ;; -----------------------------
    ;; Tags
    ;; -----------------------------
    (setq org-tag-alist '((:startgroup)
                          ("@SCHOOL" . ?s)
                          ("@HOME" . ?h)
                          ("@WORK" . ?w)
                          (:endgroup)
                          ("TOWATCH" . ?W)
                          ("TOREAD" . ?R)))
    (setq org-tags-exclude-from-inheritance '("elfeed"))

    ;; -----------------------------
    ;; Archive
    ;; -----------------------------
    (setq org-archive-location (concat org-directory "/Archived/" "%s_archive::"))

    ;; -----------------------------
    ;; Link
    ;; -----------------------------
    (setq org-keep-stored-link-after-insertion t)

    (defadvice org-insert-link (before remove-nil-link-from-org-stored-links)
      "org-link-fontify-links-to-this-file cannot handle (nil \"\")"
      (setq org-stored-links
            (remove-if (lambda (x) (eq nil (car x))) org-stored-links)))

    (defadvice org-insert-link (before remove-dups-in-org-stored-links)
      (delete-dups org-stored-links)))
  :general
  (general-mmap
   :keymaps 'org-mode-map
   "RET" 'org-open-at-point)
  (private/set-leader-keys-for-mode
   :keymaps 'org-mode-map
   "cc" 'org-clock-cancel
   "ci" 'org-clock-in
   "co" 'org-clock-out
   "cr" 'org-resolve-clocks
   "dd" 'org-deadline
   "ds" 'org-schedule
   "dt" 'org-time-stamp
   "dT" 'org-time-stamp-inactive

   "e" 'org-export-dispatch

   "a" 'org-agenda

   "t" 'org-todo

   ;; More cycling options (timestamps, headlines, items, properties)
   "L" 'org-shiftright
   "H" 'org-shiftleft
   "J" 'org-shiftdown
   "K" 'org-shiftup

   ;; Change between TODO sets
   "C-S-l" 'org-shiftcontrolright
   "C-S-h" 'org-shiftcontrolleft
   "C-S-j" 'org-shiftcontroldown
   "C-S-k" 'org-shiftcontrolup

   ;; Subtree editing
   "sa" 'org-archive-subtree
   "sb" (lambda () (interactive) (org-tree-to-indirect-buffer 4))
   "sh" 'org-promote-subtree
   "sj" 'org-move-subtree-down
   "sk" 'org-move-subtree-up
   "sl" 'org-demote-subtree
   "sn" 'org-narrow-to-subtree
   "sN" 'widen
   "sr" 'org-refile
   "ss" 'org-sparse-tree
   "sS" 'org-sort

   ;; Multi-purpose keys
   "'" 'org-ctrl-c-ctrl-c
   "*" 'org-ctrl-c-star
   "RET" 'org-ctrl-c-ret
   "-" 'org-ctrl-c-minus
   "#" 'org-update-statistics-cookies
   ;; attachments
   "A" 'org-attach
   ;; insertion
   "id" 'org-insert-drawer
   "ie" 'org-set-effort
   "if" 'org-footnote-new
   "ih" 'org-insert-heading
   "iH" 'org-insert-heading-after-current
   "iK" 'spacemacs/insert-keybinding-org
   "il" 'org-insert-link
   "ip" 'org-set-property
   "is" 'org-insert-subheading
   "it" 'org-set-tags)
  (private/set-leader-keys
   "aa" 'org-agenda-list
   "oa" 'org-agenda
   "ol" 'org-store-link
   "oL" 'org-insert-link-global
   "ob" 'org-iswitchb
   "os" 'org-save-all-org-buffers
   "og" 'org-clock-goto
   "oo" 'org-clock-out
   "oc" 'org-capture
   "oC" 'org-capture-goto-last-stored
   "oj" '(lambda () (interactive) (org-refile '(4)))
   "oJ" 'org-refile-goto-last-stored))

(use-package org-habit
  :ensure org-plus-contrib
  :config
  (setq org-habit-graph-column 50))

(use-package ob
  :ensure org-plus-contrib
  :after org
  :config
  (progn
    (org-babel-do-load-languages
     'org-babel-load-languages
     '(
       (shell . t)
       (emacs-lisp . t)
       (latex . t)
       (python . t)
       (ruby . t)
       (org . t)
       (sql . t)
       (C . t)
       (dot . t)
       (scheme . t)
       (plantuml . t)
       (ledger . t)
       (js . t)
       ))
    (setq org-export-babel-evaluate nil
          org-confirm-babel-evaluate nil)))

(use-package org-list
  :ensure org-plus-contrib
  :config
  (progn
    (setq org-cycle-include-plain-lists 'integrate)))

(use-package org-id
  :ensure org-plus-contrib
  :config
  (progn
    (setq org-id-locations-file (concat private/cache-directory ".org-id-locations"))
    (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)))

(use-package ox
  :ensure org-plus-contrib
  :after org
  :config
  (progn
    (setq org-export-coding-system 'utf-8)
    (setq org-export-backends '(beamer html latex md gfm))

    (setq org-export-with-sub-superscripts '{}
          org-export-with-section-numbers 3
          org-export-with-todo-keywords nil
          org-export-with-timestamps nil)

    ;; {{ export org-mode in Chinese into PDF
    ;; @see http://freizl.github.io/posts/2012-04-06-export-orgmode-file-in-Chinese.html
    (setq org-latex-pdf-process
          '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
            "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
            "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

    ;; Use listings to export code blocks
    (setq org-latex-listings t)
    (setq org-latex-listings-options
          '(("breaklines" "")
            ("keywordstyle" "\\color{black}\\bfseries")
            ("basicstyle" "\\ttfamily\\scriptsize")))
    (add-to-list 'org-latex-packages-alist '("" "listings"))
    (add-to-list 'org-latex-packages-alist '("" "color"))

    ;; copy/pasted from spacemacs chinese layer
    (defadvice org-html-paragraph (before org-html-paragraph-advice
                                          (paragraph contents info) activate)
      "Join consecutive Chinese lines into a single long line without
unwanted space when exporting org-mode to html."
      (let* ((origin-contents (ad-get-arg 1))
             (fix-regexp "[[:multibyte:]]")
             (fixed-contents
              (replace-regexp-in-string
               (concat
                "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)") "\\1\\2" origin-contents)))
        (ad-set-arg 1 fixed-contents)))

    (setq org-html-mathjax-options
          (quote
           ((path "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_SVG")
            (scale "100")
            (align "center")
            (indent "2em")
            (mathml nil))))))

(use-package org-clock
  :ensure org-plus-contrib
  :config
  (progn
    (setq org-clock-mode-line-total 'current)
    ;; Show lot of clocking history so it's easy to pick items
    (setq org-clock-history-length 20)
    ;; Resume clocking task on clock-in if the clock is open
    (setq org-clock-in-resume t)
    ;; Change task state to STARTED when clocking in
    ;; (setq org-clock-in-switch-to-state 'bh/clock-in-to-next)

    ;; Save clock data and state changes and notes in the LOGBOOK drawer
    (setq org-clock-into-drawer t)
    ;; Removes clocked tasks with 0:00 duration
    (setq org-clock-out-remove-zero-time-clocks t)
    ;; Save the running clock and all clock history when exiting Emacs, load it on startup
    (setq org-clock-persist t)
    ;; Do not prompt to resume an active clock
    (setq org-clock-persist-query-resume nil)
    ;; Resume clocking task when emacs is restarted
    (org-clock-persistence-insinuate)
    (setq org-clock-persist-file (concat private/cache-directory
                                         "org-clock-save.el"))
    ;; Clock out when moving task to a done state
    (setq org-clock-out-when-done t)
    ;; Enable auto clock resolution for finding open clocks
    (setq org-clock-auto-clock-resolution '(when-no-clock-is-running))
    ;; Include current clocking task in clock reports
    (setq org-clock-report-include-clocking-task t)
    (setq org-clock-clocktable-default-properties
          '(:link t :maxlevel 2 :scope file :narrow 70! :compact))
    (setq org-clock-idle-time 10)))

(use-package org-agenda
  :ensure org-plus-contrib
  :init
  (progn
    (setq org-agenda-diary-file "~/Org/journal.org")
    (setq org-agenda-files '("~/Org" "~/Org/notes"))

    ;; Overwrite the current window with the agenda
    (setq org-agenda-window-setup 'current-window)

    (setq org-agenda-restore-windows-after-quit t)

    (setq org-agenda-span 'day)

    (setq org-agenda-clockreport-parameter-plist
          '(:link t :maxlevel 9 :fileskip0 t :narrow 70! :formula "$4=$3*(60/25);t" :compact t))

    (setq org-agenda-custom-commands
          '(("h" "Agenda and Home-related tasks"
             ((agenda "")
              (tags-todo "@HOME")))
            ("w" "@WORK"
             ((agenda ""))
             ((org-agenda-tag-filter-preset '("+@WORK"))))))

    (setq org-agenda-skip-scheduled-if-deadline-is-shown 'not-today))
  :config
  (progn
    (evil-set-initial-state 'org-agenda-mode 'normal)

    (defun private/org-agenda-refresh-appt ()
      (interactive)
      (org-agenda-to-appt t))

    (run-at-time "24:01" 3600 'private/org-agenda-refresh-appt))
  :general
  (general-evil-define-key 'normal 'org-agenda-keymap
    "RET" 'org-agenda-switch-to
    "i" 'org-agenda-clock-in
    "s" 'org-agenda-schedule
    "d" 'org-agenda-deadline
    "t" 'org-agenda-todo
    "j" 'org-agenda-next-line
    "k" 'org-agenda-previous-line
    "L" 'org-agenda-log-mode
    "q" 'org-agenda-quit
    "f" 'org-agenda-later
    "b" 'org-agenda-earlier
    "r" 'org-agenda-redo
    "R" 'org-agenda-clockreport-mode
    "gj" 'org-agenda-next-line
    "gk" 'org-agenda-previous-line))

(use-package appt
  :ensure nil
  :defer t
  :config
  (progn
    (defun private/macos-do-not-display-is-on? ()
      (string-prefix-p "1"
                       (shell-command-to-string
                        "defaults read ~/Library/Preferences/ByHost/com.apple.notificationcenterui.plist doNotDisturb")))

    (defun private/appt-display (min-to-app new-time msg)
      (if (private/macos-do-not-display-is-on?)
          (appt-disp-window min-to-app new-time msg)
        (private/notification "Org Agenda Appointment" msg (format "Appointment in %s minute(s)" min-to-app) "1")))

    (setq appt-disp-window-function 'private/appt-display)))

(use-package org-capture
  :ensure org-plus-contrib
  :config
  (progn
    (defun private/link-to-org-tags (url)
      (cond
       ((not url) "")
       ((string-match-p "wanqu.co"      url) " :Wanqu:")
       ((string-match-p "youtube"       url) " :YouTube:")
       ((string-match-p "bilibili"      url) " :Bilibili:")
       ((string-match-p "zhihu"         url) " :Zhihu:")
       ((string-match-p "sspai"         url) " :少数派:")
       ((string-match-p "weixin.qq.com" url) " :WeChat:")))

    (defun private/capture-template ()
      (let ((link-to-org-tags "%(private/link-to-org-tags \"%l\")")
            (properties "\n:PROPERTIES:\n:CREATED: %U\n:END:\n")
            (content-before (plist-get org-capture-plist :content-before))
            (content-after (plist-get org-capture-plist :content-after)))
        (concat content-before link-to-org-tags properties content-after)))

    (setq org-capture-templates
          '(("t" "Todo Later" entry
             (file+headline "~/Org/refile.org" "Todo Later")
             (function private/capture-template)
             :content-before "* TODO %?")
            ("w" "Watch Later" entry
             (file+headline "~/Org/refile.org" "Watch Later")
             (function private/capture-template)
             :content-before "* TODO %a"
             :immediate-finish t)
            ("r" "Read Later" entry
             (file+headline "~/Org/refile.org" "Read Later")
             (function private/capture-template)
             :content-before "* TODO %a"
             :immediate-finish t)
            ("b" "Blog Thought" entry
             (file+headline "~/Org/personal.org" "Blog")
             (function private/capture-template)
             :content-before "* TODO %^{Title}"
             :immediate-finish t)
            ("T" "Clock-in Task" entry
             (file "~/Org/refile.org")
             (function private/capture-template)
             :content-before "* NEXT %?"
             :clock-in t
             :clock-resume t)
            ("i" "Interruption" entry
             (file+headline "~/Org/refile.org" "Todo Later")
             (function private/capture-template)
             :content-before "* TODO %^{Task}\nSCHEDULED: %t"
             :immediate-finish t)
            ("l" "Link to current file" entry
             (file "~/Org/refile.org")
             (function private/capture-template)
             :content-before "* TODO %a")
            ("L" "(Clocked in) Link to current file" entry
             (file "~/Org/refile.org")
             (function private/capture-template)
             :content-before "* NEXT %a"
             :clock-in t
             :clock-resume t)
            ("c" "Link under current clock" entry
             (clock)
             (function private/capture-template)
             :content-before "* TODO %a")
            ("C" "(Clocked-in) Link under current clock" entry
             (clock)
             (function private/capture-template)
             :content-before "* TODO %a"
             :clock-in t
             :clock-resume t)
            ("k" "Push to Kindle" entry
             (file+headline "~/Org/refile.org" "Push to Kindle")
             (function private/capture-template)
             :content-before "* TODO %a %(private/push-to-kindle \"%l\")"
             :immediate-finish t)
            ("p" "Github PR" entry
             (clock)
             (function private/capture-template)
             :content-before "* PENDING %a"
             :immediate-finish t)
            ("d" "Daily Review" entry
             (file+headline "~/Org/review.org" "Daily Review")
             (function private/capture-template)
             :content-before "* NEXT Review %u"
             :content-after "- Amazing things that happened today\n  1. %?\n- How could today have been even better?\n  1. "
             :clock-in t)
            ("D" "Daily Report at Ekohe" plain
             (file+headline "~/Org/ekohe.org" "Daily Report")
             "- %u\n  + Yesterday%?\n  + Today")))))

(use-package org-src
  :ensure org-plus-contrib)

(use-package org-indent
  :ensure org-plus-contrib
  :init (setq org-startup-indented t))

(use-package org-expiry
  :ensure org-plus-contrib
  :commands
  (org-expiry-insinuate
   org-expiry-deinsinuate
   org-expiry-insert-created
   org-expiry-insert-expiry
   org-expiry-add-keyword
   org-expiry-archive-subtree
   org-expiry-process-entry
   org-expiry-process-entries)
  :after org
  :config
  (progn
    (setq org-expiry-inactive-timestamps t)
    (org-expiry-insinuate)))

(use-package evil-org
  :commands
  (evil-org-define-eol-command
   evil-org-define-bol-command)
  :ensure t
  :init
  (progn
    (setq evil-org-special-o/O nil)
    (add-hook 'org-mode-hook 'evil-org-mode)
    (add-hook 'evil-org-mode-hook
              (lambda ()
                (evil-org-set-key-theme))))
  :general
  (general-mmap
   :keymaps 'org-mode-map
   "C-<return>" (evil-org-define-eol-command org-insert-heading-respect-content)
   "C-S-<return>" (evil-org-define-eol-command org-insert-todo-heading-respect-content)
   "M-<return>" (evil-org-define-eol-command org-meta-return)
   "M-S-<return>" (evil-org-define-eol-command org-insert-todo-heading)))

(use-package org-pomodoro
  :commands 'org-pomodoro
  :general
  (private/set-leader-keys
   "op" 'org-pomodoro))

(use-package org-page
  :commands (op/new-post op/do-publication op/do-publication-and-preview-site)
  :config
  (progn
    (setq op/repository-directory "~/Projects/dsdshcym.github.io/")
    (setq op/site-domain "http://dsdshcym.github.io")
    (setq op/personal-github-link "https://github.com/dsdshcym")
    (setq op/site-main-title "dsdshome")
    (setq op/site-sub-title "This is my small blog :)")
    (setq op/theme 'wy)))

(use-package org-tree-slide
  :commands 'org-tree-slide-mode
  :config
  (org-tree-slide-simple-profile)
  :general
  (private/set-leader-keys-for-mode
   :keymaps '(org-mode-map org-tree-slide-mode-map)
   "Ss" 'org-tree-slide-mode
   "Sn" 'org-tree-slide-move-next-tree
   "Sp" 'org-tree-slide-move-previous-tree
   "Sc" 'org-tree-slide-content))

(use-package ox-reveal
  :after ox
  :config
  (setq org-reveal-root "https://cdnjs.cloudflare.com/ajax/libs/reveal.js/3.5.0/"))

(use-package ox-md
  :ensure org
  :after ox)

(use-package ox-gfm
  :after ox)

(use-package clip2org
  :commands 'clip2org
  :ensure nil
  :config
  (progn
    (setq clip2org-clippings-file "/Volumes/Kindle/documents/My Clippings.txt")
    (setq clip2org-persistence-file
          (expand-file-name "clip2org-persist.txt" private/cache-directory))))

(provide 'init-org)
