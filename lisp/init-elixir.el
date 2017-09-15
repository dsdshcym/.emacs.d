(use-package elixir-mode
  :defer t)

(use-package ob-elixir
  :after org
  :init
  (add-to-list 'org-babel-load-languages '(elixir . t)))

(use-package alchemist
  :defer t
  :init
  (add-hook 'elixir-mode-hook 'alchemist-mode)
  :general
  (private/set-leader-keys-for-mode
   :keymaps 'alchemist-mode-map
   "el" 'alchemist-eval-current-line
   "eL" 'alchemist-eval-print-current-line
   "er" 'alchemist-eval-region
   "eR" 'alchemist-eval-print-region
   "eb" 'alchemist-eval-buffer
   "eB" 'alchemist-eval-print-buffer
   "ej" 'alchemist-eval-quoted-current-line
   "eJ" 'alchemist-eval-print-quoted-current-line
   "eu" 'alchemist-eval-quoted-region
   "eU" 'alchemist-eval-print-quoted-region
   "ev" 'alchemist-eval-quoted-buffer
   "eV" 'alchemist-eval-print-quoted-buffer

   "h:" 'alchemist-help
   "hH" 'alchemist-help-history
   "hh" 'alchemist-help-search-at-point
   "hr" 'alchemist-help--search-marked-region

   "m:" 'alchemist-mix
   "mc" 'alchemist-mix-compile
   "mx" 'alchemist-mix-run

   "'"  'alchemist-iex-run
   "sc" 'alchemist-iex-compile-this-buffer
   "si" 'alchemist-iex-run
   "sI" 'alchemist-iex-project-run
   "sl" 'alchemist-iex-send-current-line
   "sL" 'alchemist-iex-send-current-line-and-go
   "sm" 'alchemist-iex-reload-module
   "sr" 'alchemist-iex-send-region
   "sR" 'alchemist-iex-send-region-and-go

   "ta" 'alchemist-mix-test
   "tb" 'alchemist-mix-test-this-buffer
   "tB" 'alchemist-project-run-tests-for-current-file
   "tt" 'alchemist-mix-test-at-point
   "tf" 'alchemist-mix-test-file
   "tF" 'alchemist-project-find-test
   "tn" 'alchemist-test-mode-jump-to-next-test
   "tp" 'alchemist-test-mode-jump-to-previous-test
   "tr" 'alchemist-mix-rerun-last-test
   "ts" 'alchemist-mix-test-stale
   "tR" 'alchemist-test-toggle-test-report-display
   "t <tab>" 'alchemist-project-toggle-file-and-tests
   "t <S-tab>" 'alchemist-project-toggle-file-and-tests-other-window

   "xb" 'alchemist-execute-this-buffer
   "xf" 'alchemist-execute-file
   "x:" 'alchemist-execute

   "cb" 'alchemist-compile-this-buffer
   "cf" 'alchemist-compile-file
   "c:" 'alchemist-compile

   "gg" 'alchemist-goto-definition-at-point
   "." 'alchemist-goto-definition-at-point
   "gb" 'alchemist-goto-jump-back
   ","  'alchemist-goto-jump-back
   "gN" 'alchemist-goto-jump-to-previous-def-symbol
   "gn" 'alchemist-goto-jump-to-next-def-symbol
   "gj" 'alchemist-goto-list-symbol-definitions

   "Xi" 'alchemist-hex-info-at-point
   "Xr" 'alchemist-hex-releases-at-point
   "XR" 'alchemist-hex-releases
   "XI" 'alchemist-hex-info
   "Xs" 'alchemist-hex-search

   "ol" 'alchemist-macroexpand-once-current-line
   "oL" 'alchemist-macroexpand-once-print-current-line
   "ok" 'alchemist-macroexpand-current-line
   "oK" 'alchemist-macroexpand-print-current-line
   "oi" 'alchemist-macroexpand-once-region
   "oI" 'alchemist-macroexpand-once-print-region
   "or" 'alchemist-macroexpand-region
   "oR" 'alchemist-macroexpand-print-region

   "fc" 'alchemist-phoenix-find-controllers
   "fC" 'alchemist-phoenix-find-channels
   "fm" 'alchemist-phoenix-find-models
   "fr" 'alchemist-phoenix-router
   "fs" 'alchemist-phoenix-find-static
   "ft" 'alchemist-phoenix-find-templates
   "fv" 'alchemist-phoenix-find-views
   "fw" 'alchemist-phoenix-find-web)
  (general-nmap
   :keymaps '(alchemist-compile-mode-map
              alchemist-eval-mode-map
              alchemist-execute-mode-map
              alchemist-message-mode-map
              alchemist-help-minor-mode-map
              alchemist-mix-mode-map
              alchemist-macroexpand-mode-map
              alchemist-refcard-mode-map
              alchemist-test-report-mode-map)
   "q" 'quit-window))

(use-package smartparens-elixir
  :ensure smartparens
  :config
  (progn
    (defun private/elixir-do-end-close-action (id action context)
      (when (eq action 'insert)
        (save-excursion (newline-and-indent))
        (indent-according-to-mode)))

    (sp-with-modes 'elixir-mode
      (sp-local-pair "cond" "end"
                     :when '(("RET" "<evil-ret>"))
                     :post-handlers '(sp-elixir-empty-do-block-post-handler))

      (sp-local-pair "do" "end"
                     :when '(("RET" "<evil-ret>"))
                     :skip-match 'sp-elixir-skip-def-p
                     :post-handlers '(private/elixir-do-end-close-action)))))

(provide 'init-elixir)
