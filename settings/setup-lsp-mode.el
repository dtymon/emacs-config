;; -*- lexical-binding: t -*-

(use-package lsp-mode
  :ensure t
  :defer t
  :after (which-key)
  :blackout lsp-mode
  :commands (lsp lsp-deferred)
  :hook
  (typescript-mode    . lsp-deferred)
  (typescript-ts-mode . lsp-deferred)
  (tsx-ts-mode        . lsp-deferred)
  (python-mode        . lsp-deferred)
  (python-ts-mode     . lsp-deferred)

  :init
  (setq
   ;; Restart lsp if/when it crashes which it does a bit
   lsp-restart 'auto-restart

   ;; The eslint client seems to be very CPU-intensive so disable for now. And
   ;; besides, it doesn't work well with ESLint v9.
   ;;
   ;; Also disable some Python checkers as we are going to use ruff instead.
   lsp-disabled-clients '(eslint pyls)

   ;; Turn this on for verbose logging to debug lsp
   ;; lsp-log-io t

   ;; Turn off some features
   ;; lsp-eldoc-enable-hover                t
   lsp-headerline-breadcrumb-enable      nil
   lsp-lens-enable                       nil
   lsp-enable-symbol-highlighting        nil
   lsp-apply-edits-after-file-operations nil
   lsp-modeline-code-actions-enable      nil
   lsp-modeline-diagnostics-enable       nil
   lsp-modeline-workspace-status-enable  nil

   ;; Do not replace text to the right of the completion point when inserting
   ;; the selected completion. Just insert the text instead.
   lsp-completion-default-behaviour :insert

   ;; It looks like this poorly named variable is used to enable or disable lsp
   ;; displaying diagnostics in the minibuffer.
   ;; lsp--show-message nil

;;   lsp-completion-provider :none

   ;; Do not auto-configure dap-mode
   lsp-enable-dap-auto-configure nil

   ;; Don't start a pyright running in every single project that I've ever
   ;; visited!
   lsp-pyright-multi-root nil
   )

  :config
  ;; This is required if you want to refer to checkers below to add them in the
  ;; required order.
  (require 'lsp-diagnostics)
  (lsp-diagnostics-flycheck-enable)

  ;; This is ugly but looks like it's the only way to have ruff (via lsp)
  ;; followed by MyPy for Python files.
  ;; (flycheck-add-next-checker 'lsp 'python-pyright)
  ;; ;; (flycheck-add-next-checker 'lsp 'python-mypy)

  ;; If ruff-lsp is installed (via brew) then lsp will run ruff automatically.
  ;; In this case, there is no need to configure flycheck to run ruff.
  ;; (flycheck-add-next-checker 'python-mypy 'python-ruff)

  ;; Use flycheck's eslint checker after lsp rather than lsp-eslint
  ;; Nah, it's super slow and everything is really laggy.
  ;; (flycheck-add-next-checker 'lsp 'javascript-eslint)

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
  :bind (:map lsp-ui-mode-map
              ("C-c d" . lsp-ui-doc-glance))

  :config
  (setq
    lsp-ui-imenu-enable         nil
    lsp-ui-peek-enable          nil
    lsp-ui-doc-enable           nil
    lsp-ui-doc-max-height       50
    lsp-ui-doc-show-with-cursor nil
    lsp-ui-doc-show-with-mouse  nil

    lsp-ui-sideline-delay             1
    lsp-ui-sideline-show-code-actions nil
    lsp-ui-sideline-update-mode       'point
    )
)

(provide 'setup-lsp-mode)
