(use-package go-mode
  :ensure t
  :mode
  "\\.go\\'"
  :config
  (add-hook 'go-mode-hook
            (lambda ()
              (display-fill-column-indicator-mode 1)
              ;; (flycheck-mode 1)
              (setq
               truncate-lines   nil
               indent-tabs-mode nil
               tab-width        4
               )
              ))
  )

(provide 'setup-go-mode)
