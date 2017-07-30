(use-package evil
  :init (progn
          (setq evil-want-C-u-scroll t)
          (evil-mode))
  :config (progn
            ;; esc should always quit: http://stackoverflow.com/a/10166400/61435
            (define-key evil-normal-state-map [escape] 'keyboard-quit)
            (define-key evil-visual-state-map [escape] 'keyboard-quit)
            (define-key minibuffer-local-map [escape] 'abort-recursive-edit)
            (define-key minibuffer-local-ns-map [escape] 'abort-recursive-edit)
            (define-key minibuffer-local-completion-map [escape] 'abort-recursive-edit)
            (define-key minibuffer-local-must-match-map [escape] 'abort-recursive-edit)
            (define-key minibuffer-local-isearch-map [escape] 'abort-recursive-edit)
            ;; Escape from isearch-mode("/" and "?" in evil-mode) like vim
            (define-key isearch-mode-map (kbd "<escape>") 'isearch-cancel)))

(use-package general
  :config (progn
            (general-define-key :prefix "SPC"
                                :non-normal-prefix "M-m"
                                :keymaps '(motion insert emacs)
                                "" nil

                                "SPC" 'execute-extended-command

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
                                "wF" 'make-frame
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
                                "w=" 'balance-windows)))

(use-package evil-nerd-commenter
  :defer t
  :general
  (general-define-key
   :prefix "SPC"
   :non-normal-prefix "M-m"
   :keymaps '(motion insert emacs)
   ";"  'evilnc-comment-operator))

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
