(use-package lsp-mode
  :ensure t
  :defer t
  :after (which-key)
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

   ;; Turn this on for verbose logging to debug lsp
   ;; lsp-log-io t

   ;; Turn off some features
   lsp-eldoc-enable-hover                nil
   lsp-headerline-breadcrumb-enable      nil
   lsp-lens-enable                       nil
   lsp-enable-symbol-highlighting        nil
   lsp-apply-edits-after-file-operations nil
   lsp-modeline-code-actions-enable      t

   ;; It looks like this poorly named variable is used to enable or disable lsp
   ;; displaying diagnostics in the minibuffer.
   ;; lsp--show-message nil

;;   lsp-completion-provider :none
   )

  :config
  ;; This is ugly but looks like it's the only way to have ruff (via lsp)
  ;; followed by MyPy for Python files.
  (require 'lsp-diagnostics)
  (lsp-diagnostics-flycheck-enable)
  ;;(flycheck-add-next-checker 'lsp 'python-mypy)
  (flycheck-add-next-checker 'lsp '(warning . python-mypy))
  ;; (flycheck-add-next-checker 'lsp 'python-dtymon)
  ;; (flycheck-add-next-checker 'python-dtymon 'python-mypy)

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
    lsp-ui-peek-enable          nil
    lsp-ui-doc-enable           nil
    lsp-ui-imenu-enable         nil
    lsp-ui-doc-enable           nil
    lsp-ui-doc-show-with-cursor nil
    lsp-ui-doc-show-with-mouse  nil

    lsp-ui-sideline-delay             1
    lsp-ui-sideline-show-code-actions t
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
