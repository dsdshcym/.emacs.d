(use-package evil
  :init (progn
          (setq evil-want-Y-yank-to-eol t)
          (setq evil-symbol-word-search t)
          (setq evil-want-C-u-scroll t)
          (evil-mode))
  :config (progn
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
                    split-window-right-and-focus))
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
              (evil-first-non-blank))

            ;; esc should always quit: http://stackoverflow.com/a/10166400/61435
            (define-key evil-normal-state-map [escape] 'keyboard-quit)
            (define-key evil-visual-state-map [escape] 'keyboard-quit)
            (define-key minibuffer-local-map [escape] 'abort-recursive-edit)
            (define-key minibuffer-local-ns-map [escape] 'abort-recursive-edit)
            (define-key minibuffer-local-completion-map [escape] 'abort-recursive-edit)
            (define-key minibuffer-local-must-match-map [escape] 'abort-recursive-edit)
            (define-key minibuffer-local-isearch-map [escape] 'abort-recursive-edit)
            ;; Escape from isearch-mode("/" and "?" in evil-mode) like vim
            (define-key isearch-mode-map (kbd "<escape>") 'isearch-cancel)
            (define-key evil-motion-state-map [C-i] 'evil-jump-forward)))

(use-package general
  :config (progn
            (general-create-definer
             private/set-leader-keys
             :prefix "SPC"
             :non-normal-prefix "M-m"
             :keymaps '(motion insert emacs))
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
             "TAB" (lambda () (interactive) (switch-to-buffer nil))

             ;; Help
             "sj" 'imenu

             ;; Jumping
             "hdf" 'describe-function
             "hdk" 'describe-key
             "hdv" 'describe-variable

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
             "wU" 'winner-redo
             "wu" 'winner-undo
             "wv" 'split-window-right
             "wV" 'split-window-right-and-focus
             "ww" 'other-window
             "w=" 'balance-windows

             "qf" 'delete-frame
             "qq" 'kill-emacs)
            (general-define-key
             :status 'motion
             :keymaps '(info-mode-map compilation-mode-map)
             "SPC" nil)))

(use-package evil-nerd-commenter
  :defer t
  :general
  (private/set-leader-keys
   ";"  'evilnc-comment-operator))

(use-package evil-args
  :defer t
  :init
  (add-to-list 'evil-args-delimiters " ")
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
  :general
  (general-define-key
   :states 'visual
   :keymaps 'evil-surround-mode-map
   "s" 'evil-surround-region
   "S" 'evil-substitute))

(use-package evil-ediff
  :after ediff)

(use-package evil-exchange
  :init (evil-exchange-install))

(use-package evil-iedit-state
  :commands (evil-iedit-state evil-iedit-state/iedit-mode)
  :init
  (setq iedit-toggle-key-default nil)
  :general
  (private/set-leader-keys
   "se" 'evil-iedit-state/iedit-mode))

(use-package evil-indent-plus
  :init (evil-indent-plus-default-bindings))

(use-package evil-matchit
  :init (global-evil-matchit-mode))

(use-package evil-numbers
  :commands (evil-numbers/inc-at-pt evil-numbers/inc-at-pt)
  :general
  (general-define-key
   :keymaps 'motion
   "+" 'evil-numbers/inc-at-pt
   "-" 'evil-numbers/dec-at-pt))

(use-package evil-visualstar
  :commands (evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward)
  :config (global-evil-visualstar-mode)
  :general
  (general-define-key
   :keymaps 'visual
   "*" 'evil-visualstar/begin-search-forward
   "#" 'evil-visualstar/begin-search-backward))

(use-package ace-window
  :defer t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :general
  (private/set-leader-keys
   "ww" 'ace-window
   "wM" 'ace-swap-window))

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
