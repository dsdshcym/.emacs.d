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
  :init (sml/setup))

(provide 'init-ui)
