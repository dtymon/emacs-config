(require 'which-key)

(use-package lsp-mode
  :ensure t
  :init
  (setq
   ;; Setting this value too high seems to cause issues where lsp does not run
   ;; and errors that have been fixed are still marked as broken.
   lsp-idle-delay 0.2
   lsp-restart 'auto-restart
   lsp-modeline-code-actions-enable nil
   lsp-lens-enable nil
   lsp-headerline-breadcrumb-enable nil
   lsp-completion-provider :company-capf
   lsp-enable-snippet t
;;   lsp-prefer-flymake t
;;   lsp-diagnostics-provider 'flymake

   lsp-clients-typescript-log-verbosity "verbose"
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
         (js2-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration)
         )
  :commands lsp
  )

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq
   lsp-ui-doc-enable nil
   lsp-ui-doc-position 'top
   lsp-ui-doc-delay 0.2
   lsp-ui-doc-include-signature t
   lsp-ui-doc-show-with-cursor t
   lsp-ui-doc-show-with-mouse t
   lsp-ui-doc-border "white"
   lsp-ui-doc-header t
   lsp-ui-doc-include-signature t
   lsp-ui-doc-alignment 'frame
   lsp-ui-doc-use-childframe t
   lsp-ui-doc-use-webkit t
;;   lsp-ui-doc-text-scale-level 1
   )
  (set-face-attribute 'lsp-ui-doc-background nil :background "#10264a")
  (global-set-key (kbd "C-x s-d") 'lsp-ui-doc-glance)
  )

;;(use-package helm-lsp
;;  :ensure t
;;  :after (helm)
;;  :commands helm-lsp-workspace-symbol
;;  )

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
;;; setup-lsp-mode.el ends here

