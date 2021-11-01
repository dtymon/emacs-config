(use-package json-mode
  :ensure t
  :mode "\\.json"
  :config
  (add-hook 'json-mode-hook (lambda ()
                            (flycheck-mode 1)
                            (prettier-mode 1)

                            (make-local-variable 'js-indent-level)
                            (setq js-indent-level 2)
                            ))
  )

(provide 'setup-json-mode)
