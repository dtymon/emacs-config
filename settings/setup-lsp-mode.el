;; (defun dtymon::set-eslint-config-after-direnv ()
;;   (let ((eslint-config (getenv "ESLINT_CONFIG_PATH")))
;;     (when eslint-config
;;       (message "Setting ESLint config to: %s" eslint-config)
;;       (setq-local lsp-eslint-options `(:overrideConfigFile eslint-config))
;;       (when (bound-and-true-p lsp-mode)
;;         (lsp--set-configuration)
;;       ))
;;   ))

(use-package lsp-mode
  :ensure t
  :defer t
  :after (which-key)
  :blackout lsp-mode
  :commands (lsp lsp-deferred)
  :hook
  (typescript-mode    . lsp-deferred)
  (typescript-ts-mode . lsp-deferred)
  (python-mode        . lsp-deferred)
  (python-ts-mode     . lsp-deferred)

  :init
  (setq
   ;; Restart lsp if/when it crashes which it does a bit
   lsp-restart 'auto-restart

   ;; The eslint client seems to be very CPU-intensive so disable for now. Also
   ;; disable some Python checkers as we are going to use ruff instead.
   lsp-disabled-clients '(eslint pyls)
   ;; lsp-disabled-clients '(pyls)

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

   lsp-eslint-package-manager "yarn"
   lsp-eslint-format nil
   ;; lsp-eslint-server-command '("eslint" "--stdin" "--stdin-filename" "%f")
   ;; lsp-eslint-options '(:overrideConfigFile "/Users/admzw26/work/ben/repos/kelpie/ben-kelpie-build/.eslint.config.mjs")
   ;; lsp-eslint-options '(:overrideConfigFile "/Users/admzw26/work/ben/repos/kelpie/ben-kelpie-build/tests/.eslint.config.spec.mjs")
   ;; lsp-eslint-options '(:overrideConfigFile "/Users/admzw26/work/ben/repos/kelpie/ben-kelpie-build/tests/.eslint.config.spec.mjs")
   ;; lsp-eslint-options '(:overrideConfigFile "application/.eslint.config.mjs")
   ;; lsp-eslint-options `(:overrideConfigFile ,(getenv "ESLINT_CONFIG_PATH"))
   ;; lsp-log-io t

   ;; lsp-eslint-working-directories '("." "./application" "./application/tests")
   lsp-eslint-working-directories '("./application")
   lsp-eslint-options '(:overrideConfigFile ".eslint.config.mjs")
   )

  :config
  ;; This is ugly but looks like it's the only way to have ruff (via lsp)
  ;; followed by MyPy for Python files. We need to bring in lsp-diagnostics
  ;; (require 'lsp-diagnostics)
  ;; (lsp-diagnostics-flycheck-enable)
  ;; (flycheck-add-next-checker 'lsp 'python-pyright)
  ;; ;; (flycheck-add-next-checker 'lsp 'python-mypy)

  ;; (flycheck-add-next-checker 'lsp 'javascript-eslint)

  ;; If ruff-lsp is installed (via brew) then lsp will run ruff automatically.
  ;; In this case, there is no need to configure flycheck to run ruff.
  ;; (flycheck-add-next-checker 'python-mypy 'python-ruff)

  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\docs\\'")

  ;; (add-hook 'hack-local-variables-hook #'dtymon::set-eslint-config-after-direnv)

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
    lsp-ui-doc-show-with-cursor nil
    lsp-ui-doc-show-with-mouse  nil

    lsp-ui-sideline-delay             1
    lsp-ui-sideline-show-code-actions nil
    lsp-ui-sideline-update-mode       'line
    )
)

;; (use-package lsp-ui
;;   :ensure t
;;   :after lsp-mode
;;   :commands lsp-ui-mode
;;   :bind (:map global-map
;;               ;; Bind a global key to bring up the documentation on on-demand
;;               ("C-x s-d" . lsp-ui-doc-glance))
;;
;;   :config
;;   ;;(set-face-attribute 'lsp-ui-doc-background nil :background "#10264a")
;;
;;   (setq
;;     lsp-ui-peek-enable          nil
;;     lsp-ui-doc-enable           nil
;;     lsp-ui-imenu-enable         nil
;;     lsp-ui-doc-enable           nil
;;     lsp-ui-doc-show-with-cursor nil
;;     lsp-ui-doc-show-with-mouse  nil
;;
;;    lsp-ui-doc-delay               2
;;    lsp-ui-doc-position            'top
;;    lsp-ui-doc-header              t
;;    lsp-ui-doc-include-signature   t
;;    lsp-ui-doc-text-scale-level    1
;;    lsp-ui-doc-border              "white"
;;    lsp-ui-doc-alignment           'frame
;;    lsp-ui-doc-use-childframe      t
;;    lsp-ui-doc-use-webkit          t
;;    lsp-ui-doc-webkit-max-width-px 2000
;;    lsp-ui-doc-max-height          50
;;
;;    ;; Do not display the code actions (ie: what can be done to fix the issue) as
;;    ;; they are too distracting. Delay showing the sideline so that it does not
;;    ;; show as we move around the file.
;;    ;;
;;    ;; It's too annoying. Doesn't take into consideration the left fringe width
;;    ;; so the messages always wrap by about one char. Disable it for now.
;;    lsp-ui-sideline-enable            nil
;;    ;; lsp-ui-sideline-show-hover        t
;;    ;; lsp-ui-sideline-show-code-actions nil
;;    ;; lsp-ui-sideline-delay             1
;;    ;; lsp-ui-sideline-update-mode       'line
;;    )
;;   )

(provide 'setup-lsp-mode)
