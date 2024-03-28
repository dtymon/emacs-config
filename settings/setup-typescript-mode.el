(use-package typescript-mode
  :ensure t
  :blackout (typescript-mode . "TS")
  :mode "\\.ts"
  :interpreter "node"

  :init
  ;; Turn on tree-sitter and use it for highlighting
  (add-hook 'typescript-mode-hook #'tree-sitter-mode)
  (add-hook 'typescript-mode-hook #'tree-sitter-hl-mode)

  (add-hook 'typescript-mode-hook
            (lambda ()
              ;; Show the fill column indicator. This is the comment fill column
              ;; and not the code fill column which is defined in prettier and
              ;; could be different.
              (display-fill-column-indicator-mode 1)

              ;; Only auto-fill comment lines. prettier will handle the code.
              (dtymon::auto-fill-comments-only-hook)
              (prettier-mode 1)

              (setq
               typescript-indent-level 2

               ;; Long lines should wrap
               truncate-lines nil

               ;; This is the fill column for comments
               fill-column 80

               ;; Don't use js2-mode's comment wrapping override functions as
               ;; they don't indent properly. Instead use the standard C ones as
               ;; they appear to work better.
               ;;
               ;; Actually revert back to js2-mode's comments as it seems to be
               ;; working better these days.
               comment-line-break-function #'js2-line-break
               ;; comment-line-break-function #'indent-new-comment-line
               ;; comment-line-break-function #'c-indent-new-comment-line
               js-indent-level 2

               comment-multi-line t
               )

              ;; Don't use the fill column from the prettier config
              (and (eq (cadar prettier-sync-settings) ':printWidth) (pop prettier-sync-settings))
              ))
)

(provide 'setup-typescript-mode)
