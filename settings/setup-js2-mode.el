(use-package js2-mode
  :ensure t
  :mode "\\.c?js\\'"
  :init
  (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))
  (setq-default js2-allow-rhino-new-expr-initializer nil)
  (setq-default js2-auto-indent-p t)
  (setq-default js2-enter-indents-newline t)
  (setq-default js2-global-externs '("module" "require" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON"))
  (setq-default js2-idle-timer-delay 0.1)
  (setq-default js2-indent-on-enter-key t)
  (setq-default js2-mirror-mode nil)
  (setq-default js2-strict-inconsistent-return-warning nil)
  (setq-default js2-auto-indent-p t)
  (setq-default js2-include-rhino-externs nil)
  (setq-default js2-include-gears-externs nil)
  (setq-default js2-concat-multiline-strings 'eol)
  (setq-default js2-rebind-eol-bol-keys nil)

  ;; Let flycheck handle parse errors
  (setq-default js2-show-parse-errors nil)
  (setq-default js2-strict-missing-semi-warning nil)
  (setq-default js2-strict-trailing-comma-warning t)
  :config
;;  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (add-hook 'js2-mode-hook
            (lambda ()
;;              (flycheck-mode 1)
              (fci-mode 1)
              (dtymon::auto-fill-comments-only-hook)

              (make-variable-buffer-local 'whitespace-style)
              (add-hook 'before-save-hook 'whitespace-cleanup nil 'local)
              (setq whitespace-style '(face tabs trailing space-before-tab::tab empty))
              (whitespace-mode 1)

              (setq
               js-indent-level              2
               js-switch-indent-offset      2
               js2-highlight-level          3
               js2-mode-show-parse-errors   nil
               js2-include-node-externs     t
               js2-indent-switch-body       t
               truncate-lines               nil

               ;; Don't use js2-mode's comment wrapping override functions as
               ;; they don't indent properly. Instead use the standard C ones as
               ;; they appear to work better.
               ;; comment-line-break-function #'js2-line-break
               ;; comment-line-break-function #'indent-new-comment-line
               ;; comment-line-break-function #'c-indent-new-comment-line
               )
              ))
  )

(provide 'setup-js2-mode)
