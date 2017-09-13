(use-package evil
  :init
  (progn
    (setq evil-want-Y-yank-to-eol t)
    (setq evil-symbol-word-search t)
    (setq evil-want-C-u-scroll t)
    (evil-mode))
  :config
  (progn
    (mapc #'evil-declare-ignore-repeat
          '(kill-this-buffer
            ido-kill-buffer
            outline-next-visible-heading
            outline-previous-visible-heading
            outline-up-heading
            evil-visualstar/begin-search-forward
            evil-visualstar/begin-search-backward
            org-export-dispatch
            org-end-of-line
            org-beginning-of-line
            org-open-at-point
            org-cycle
            org-shifttab
            org-ctrl-c-ctrl-c
            org-next-visible-heading
            org-previous-visible-heading
            split-window-below
            split-window-below-and-focus
            split-window-right
            split-window-right-and-focus
            evilmi-jump-items))
    (defalias 'evil-visual-update-x-selection 'ignore)

    (setq evil-normal-state-cursor '(box "DarkGoldenrod2")
          evil-insert-state-cursor '((bar . 2) "chartreuse3")
          evil-emacs-state-cursor '(box "SkyBlue2")
          evil-replace-state-cursor '((hbar . 2) "chocolate")
          evil-visual-state-cursor '((hbar . 2) "gray")
          evil-motion-state-cursor '(box "plum3"))

    (setq-default evil-shift-width 2)

    (evil-define-motion evil-goto-line (count)
      "Go to the first non-blank character of line COUNT.
By default the (truly) last line."
      :jump t
      :type line
      (if (null count)
          (goto-char (buffer-size))
        (goto-char (point-min))
        (forward-line (1- count)))
      (evil-first-non-blank))))

(use-package general
  :config
  (progn
    (setq general-vim-definer-default 'states)

    (general-evil-setup)

    (general-create-definer
     private/set-leader-keys
     :prefix "SPC"
     :non-normal-prefix "M-m"
     :keymaps '(motion insert emacs))

    (general-create-definer
     private/set-leader-keys-for-mode
     :prefix "'"
     :non-normal-prefix "M-m m"
     :states '(motion insert emacs))

    (general-create-definer
     private/enable-leader-key-for-mode
     :status 'motion
     "SPC" nil)

    (private/set-leader-keys
     "" nil

     "SPC" 'execute-extended-command

     "u" 'universal-argument

     ;; File
     "ff" 'find-file
     "fel" 'find-library
     "fS" 'evil-write-all
     "fs" 'save-buffer
     "fvd" 'add-dir-local-variable
     "fvf" 'add-file-local-variable
     "fvp" 'add-file-local-variable-prop-line

     ;; Buffer
     "bb" 'switch-to-buffer
     "bd" 'kill-this-buffer
     "bw" 'read-only-mode
     "TAB" (lambda () (interactive) (switch-to-buffer (other-buffer (current-buffer) t)))

     ;; Jumping
     "sj" 'imenu

     ;; Help
     "h" (general-simulate-keys "<f1>")

     ;; Themes
     "Ts"  'load-theme

     ;; Window Management
     "wd" 'delete-window
     "wm" 'delete-other-windows
     "wf" 'make-frame
     "wH" 'evil-window-move-far-left
     "wh" 'evil-window-left
     "wJ" 'evil-window-move-very-bottom
     "wj" 'evil-window-down
     "wK" 'evil-window-move-very-top
     "wk" 'evil-window-up
     "wL" 'evil-window-move-far-right
     "wl" 'evil-window-right
     "wo" 'other-frame
     "ws" 'split-window-below
     "wS" 'split-window-below-and-focus
     "wv" 'split-window-right
     "wV" 'split-window-right-and-focus
     "ww" 'other-window
     "w=" 'balance-windows

     "qf" 'delete-frame
     "qq" 'kill-emacs)

    (private/enable-leader-key-for-mode
     :keymaps '(info-mode-map compilation-mode-map))

    (general-define-key
     :keymaps '(minibuffer-local-map
                minibuffer-local-ns-map
                minibuffer-local-completion-map
                minibuffer-local-must-match-map
                minibuffer-local-isearch-map)
     [escape] 'abort-recursive-edit)

    ;; Escape from isearch-mode("/" and "?" in evil-mode) like vim
    (general-define-key
     :keymaps 'isearch-mode-map
     [escape] 'isearch-cancel)

    (general-define-key
     :keymaps 'input-decode-map
     "C-h" "DEL"
     "C-i" [C-i])

    (general-mmap
     [C-i] 'evil-jump-forward)

    (general-nvmap
     "j" 'evil-next-visual-line
     "k" 'evil-previous-visual-line)))

(use-package hydra
  :defer t)

(use-package evil-commentary
  :delight
  :commands (evil-commentary
             evil-commentary-yank
             evil-commentary-line)
  :general
  (general-mmap
   "gc"     'evil-commentary
   "gy"     'evil-commentary-yank))

(use-package evil-args
  :defer t
  :config
  ;; TODO:
  ;; use a mode to delimiters alist to set delimiters for different modes
  ;; See also https://github.com/wcsmith/evil-args/issues/7
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (make-local-variable 'evil-args-delimiters)
              (add-to-list 'evil-args-delimiters " ")))
  :general
  (general-define-key
   :keymaps 'evil-inner-text-objects-map
   "a" 'evil-inner-arg)
  (general-define-key
   :keymaps 'evil-outer-text-objects-map
   "a" 'evil-outer-arg))

(use-package evil-surround
  :init
  (global-evil-surround-mode)
  :config
  (progn
    (defun private/evil-surround-org-drawer ()
      (let ((drawer-name (read-from-minibuffer "" "")))
        (cons (format ":%s:" (upcase (or drawer-name ""))) ":END:")))

    (defun private/evil-surround-org-source ()
      (let ((language (read-from-minibuffer "" "")))
        (cons (format "#+BEGIN_SRC %s" (or language "")) "#+END_SRC")))

    (defun private/add-evil-surrounds-for-org-mode ()
      (push '(?: . private/evil-surround-org-drawer) evil-surround-pairs-alist)
      (push '(?s . private/evil-surround-org-source) evil-surround-pairs-alist))

    (add-hook 'org-mode-hook 'private/add-evil-surrounds-for-org-mode))
  :general
  (general-vmap
   :keymaps 'evil-surround-mode-map
   "s" 'evil-surround-region
   "S" 'evil-substitute))

(use-package evil-ediff
  :after ediff)

(use-package evil-exchange
  :init (evil-exchange-install))

(use-package evil-multiedit
  :commands (evil-multiedit-match-all
             evil-multiedit-match-and-next
             evil-multiedit-match-and-prev
             evil-multiedit-toggle-marker-here
             evil-multiedit-ex-match)
  :config
  (evil-ex-define-cmd "ie[dit]" 'evil-multiedit-ex-match)
  :general
  (private/set-leader-keys
   "se" 'evil-multiedit-match-all
   "sr" 'evil-multiedit-restore
   "sm" 'evil-multiedit-toggle-marker-here)

  (general-define-key
   :keymaps 'evil-multiedit-state-map
   "C-f" 'iedit-restrict-function
   "S" 'evil-multiedit--substitute
   "C-n" 'evil-multiedit-next
   "C-p" 'evil-multiedit-prev)

  (general-define-key
   :keymaps 'evil-multiedit-insert-state-map
   "<escape>" (lambda () (interactive) (evil-multiedit-state) (evil-move-cursor-back)))

  (general-define-key
   :keymaps '(motion evil-multiedit-state-map)
   "RET" 'evil-multiedit-toggle-or-restrict-region))

(use-package evil-indent-plus
  :init (evil-indent-plus-default-bindings))

(use-package evil-matchit
  :init (global-evil-matchit-mode))

(use-package evil-numbers
  :commands (evil-numbers/inc-at-pt evil-numbers/inc-at-pt)
  :general
  (general-mmap
   "C-a" 'evil-numbers/inc-at-pt
   "C-x" 'evil-numbers/dec-at-pt))

(use-package evil-visualstar
  :commands (evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward)
  :config (global-evil-visualstar-mode)
  :general
  (general-vmap
   "*" 'evil-visualstar/begin-search-forward
   "#" 'evil-visualstar/begin-search-backward))

(use-package evil-lion
  :init
  (evil-lion-mode))

(use-package ace-window
  :defer t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :general
  (private/set-leader-keys
   "ww" 'ace-window
   "wW" 'ace-swap-window
   "wD" 'ace-delete-window
   "wM" 'ace-delete-other-windows))

(use-package dumb-jump
  :commands (dumb-jump-go)
  :config
  (setq dumb-jump-selector 'ivy)
  :general
  (general-mmap
   "gd" 'dumb-jump-go))

(use-package winner
  :init
  (winner-mode)
  :general
  (private/set-leader-keys
   "wU" 'winner-redo
   "wu" 'winner-undo))

(defun split-window-below-and-focus ()
  "Split the window vertically and focus the new window."
  (interactive)
  (split-window-below)
  (windmove-down))

(defun split-window-right-and-focus ()
  "Split the window horizontally and focus the new window."
  (interactive)
  (split-window-right)
  (windmove-right))

(provide 'init-keybinding)
