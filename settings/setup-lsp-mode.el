(use-package lsp-mode
  :ensure t
  :defer t
  :after (which-key)
  :commands (lsp lsp-deferred)

  :init
  (setq
   ;; Restart lsp if/when it crashes which it does a bit
   lsp-restart 'auto-restart

   ;; The eslint client seems to be very CPU-intensive so disable for now
   lsp-disabled-clients '(eslint)

   ;; Turn off some features
   lsp-headerline-breadcrumb-enable      nil
   lsp-lens-enable                       nil
   lsp-enable-symbol-highlighting        nil
   lsp-apply-edits-after-file-operations nil
   lsp-modeline-code-actions-enable      nil

;;   lsp-completion-provider :none
   )

  :config
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\docs\\'")

  :bind (
         ;; ("C-c e e" . lsp-treemacs-errors-list)
         ;; ("C-c e r" . lsp-find-references)
         ;; ("C-c e R" . lsp-rename)
         ;; ("C-c e i" . lsp-find-implementation)
         ;; ("C-c e t" . lsp-find-type-definition)
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

   lsp-ui-doc-delay               1
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
;;   lsp-ui-sideline-enable            t
;;   lsp-ui-sideline-show-hover        t
   lsp-ui-sideline-show-code-actions nil
   lsp-ui-sideline-delay             1
   lsp-ui-sideline-update-mode       'line
   )
  (set-face-attribute 'lsp-ui-doc-background nil :background "#10264a")
  )

(provide 'setup-lsp-mode)
