(use-package typescript-mode
  :ensure t
  :diminish "TS"
  :mode "\\.ts"
  :interpreter "node"
;;  :after (flycheck tree-sitter)
  :config
  ;; We use the comment wrapping function from js2-mode
  (require 'js2-mode)

  ;; Tweak some of the jsdoc faces
  (set-face-attribute 'typescript-jsdoc-tag nil :foreground "#445c60")
  (set-face-attribute 'typescript-jsdoc-value nil :foreground "SlateGray")

  ;; Turn on tree-sitter
  (add-hook 'typescript-mode-hook #'tree-sitter-mode)

  (add-hook 'typescript-mode-hook
            (lambda ()
              (eglot-ensure)

              ;; Only auto-fill comment lines. prettier will handle the code
              (dtymon::auto-fill-comments-only-hook)
              (prettier-mode 1)

              ;; Show the fill column indicator. This is the comment fill column
              ;; and not the code fill column which is defined in prettier and
              ;; could be different.
              (fci-mode 1)

              ;; Turn on flycheck
              ;; (flycheck-mode 1)
;;              (flycheck-add-next-checker 'lsp 'javascript-eslint)

              (setq
               typescript-indent-level 2

               ;; Long lines should wrap
               truncate-lines nil

               ;; This is the fill column for comments
               fill-column 80
               ;; fill-column         120

               ;; Don't use js2-mode's comment wrapping override functions as
               ;; they don't indent properly. Instead use the standard C ones as
               ;; they appear to work better.
               ;;
               ;; Actually revert back to js2-mode's comments as it seems to be
               ;; working better these days.
               comment-line-break-function #'js2-line-break
               ;; comment-line-break-function #'indent-new-comment-line
               ;; comment-line-break-function #'c-indent-new-comment-line
               js2-basic-offset 2

               comment-multi-line t
               )

              ;; Don't use the fill column from the prettier config
              (and (eq (cadar prettier-sync-settings) ':printWidth) (pop prettier-sync-settings))

;;              (push '(company-semantic :with company-yasnippet) company-backends)
;;              (set (make-local-variable 'company-backends)
;;                   '((company-dabbrev-code company-yasnippet)))
;;              (set (make-local-variable 'company-backends)
;;                   '((company-tide company-files :with company-yasnippet)
;;                     (company-dabbrev-code company-dabbrev)))

              ))
  :bind (:map typescript-mode-map
              ;; Use my function for filling comments which handles both block
              ;; and line comments.
              ("M-q" . dtymon::fill-comment-paragraph)
              )
  )

(provide 'setup-typescript-mode)
