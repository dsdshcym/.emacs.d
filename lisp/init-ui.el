(blink-cursor-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

;; copy/pasted from spacemacs chinese layer
(defun private/set-monospaced-font (english chinese english-size chinese-size)
  (set-face-attribute 'default nil :font
                      (format "%s:pixelsize=%d" english english-size))
  (set-frame-font english)
  (dolist (charset '(kana han cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family chinese :size chinese-size))))

(private/set-monospaced-font "Iosevka" "PingFang SC" 14 14)

(use-package solarized-theme
  :init (progn
          (setq solarized-use-variable-pitch nil)
          (setq solarized-scale-org-headlines nil)
          (load-theme 'solarized-dark t nil)))

(use-package delight
  :defer t)

(use-package smart-mode-line
  :init
  (progn
    (setq sml/position-percentage-format nil)
    (sml/setup)))

(use-package nyan-mode
  :commands 'nyan-mode)

(use-package popwin
  :after windows
  :demand t
  :config
  (progn
      (setq popwin:special-display-config nil)

      (push '("*Help*"                 :dedicated t :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config)
      (push '("*compilation*"          :dedicated t :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config)
      (push '("*Shell Command Output*" :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
      (push '("*Async Shell Command*"  :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
      (push '("*undo-tree*"            :dedicated t :position right  :stick t :noselect nil :width   60) popwin:special-display-config)
      (push '("*undo-tree Diff*"       :dedicated t :position bottom :stick t :noselect nil :height 0.3) popwin:special-display-config)
      (push '("*ert*"                  :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
      (push '("*grep*"                 :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
      (push '("*nosetests*"            :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
      (push '("^\*WoMan.+\*$" :regexp t             :position bottom                                   ) popwin:special-display-config)

      (popwin-mode 1))
  :general
  (general-define-key
   :prefix "SPC"
   :non-normal-prefix "M-m"
   :keymaps '(motion insert emacs)
   "wpc" 'popwin:close-popup-window
   "wps" 'popwin:stick-popup-window
   "wpl" 'popwin:switch-to-last-buffer
   "wpp" 'popwin:popup-last-buffer))

(provide 'init-ui)
