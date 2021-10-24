(use-package lsp-mode
  :ensure t
  :defer t
  :after (which-key)
  :commands lsp
  :init
  (setq
   ;; Restart lsp if/when it crashes which it does a bit
   lsp-restart 'auto-restart

   ;; Turn off some features
   lsp-modeline-code-actions-enable nil
   lsp-lens-enable                  nil
   lsp-headerline-breadcrumb-enable nil
   lsp-enable-symbol-highlighting   nil

   ;; We use company for completions and snippets so turn them off for lsp
   lsp-completion-provider :none
   lsp-enable-snippet      nil

   ;; Run eslint on save not when typing as the prettier stuff is too annoying
   lsp-eslint-quiet           t
   lsp-eslint-run             "onSave"
   lsp-eslint-package-manager "yarn"

   ;; This doesn't seem to work but it should turn off prettier rules
   lsp-eslint-rules-customizations [((rule . "prettier*") (severity . "off"))]

   ;; The eslint client seems to be very CPU-intensive so disable for now
   lsp-disabled-clients '(eslint)

   ;;lsp-clients-typescript-log-verbosity "verbose"
   )

  :bind (
         ("C-c e n" . flycheck-next-error)
         ("C-c e p" . flycheck-previous-error)
         ("C-c e e" . lsp-treemacs-errors-list)
         ("C-c e r" . lsp-find-references)
         ("C-c e R" . lsp-rename)
         ("C-c e i" . lsp-find-implementation)
         ("C-c e t" . lsp-find-type-definition)
         )

  :hook (
         (typescript-mode . lsp-deferred)
         (js2-mode        . lsp-deferred)
         (lsp-mode        . lsp-enable-which-key-integration)
         )
  )

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode
  :bind (:map global-map
              ;; Bind a global key to bring up the documentation on on-demand
              ("C-x s-d" . lsp-ui-doc-glance))

  :config
  (setq
   lsp-ui-doc-enable           nil
   lsp-ui-doc-show-with-cursor nil
   lsp-ui-doc-show-with-mouse  nil

   lsp-ui-doc-delay               0.1
   lsp-ui-doc-position            'top
   lsp-ui-doc-header              t
   lsp-ui-doc-include-signature   t
   lsp-ui-doc-text-scale-level    1
   lsp-ui-doc-border              "white"
   lsp-ui-doc-alignment           'frame
   lsp-ui-doc-use-childframe      t
   lsp-ui-doc-use-webkit          t
   lsp-ui-doc-webkit-max-width-px 2000
   lsp-ui-doc-max-height          50

   ;; Do not display the code actions (ie: what can be done to fix the issue) as
   ;; they are too distracting. Delay showing the sideline so that it does not
   ;; show as we move around the file.
   lsp-ui-sideline-show-code-actions nil
   lsp-ui-sideline-delay             1
   )
  (set-face-attribute 'lsp-ui-doc-background nil :background "#10264a")
  )

;;(use-package lsp-treemacs
;;  :ensure t
;;  :after (treemacs)
;;  :config
;;  (lsp-treemacs-sync-mode 1)
;;  :commands lsp-treemacs-errors-list
;;  )

;; optionally if you want to use debugger
;;(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(provide 'setup-lsp-mode)

