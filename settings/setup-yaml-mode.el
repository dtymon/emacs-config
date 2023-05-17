(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml"
  :config
  (add-hook 'yaml-mode-hook 'flymake-yamllint-setup)
  (add-hook 'yaml-mode-hook (lambda ()
                            ;; (flycheck-mode 1)
                            (flymake-mode 1)
                            (prettier-mode 1)
                            (fci-mode 1)
                            ))
  )

(cond
 (use-flymake
  (use-package flymake-yamllint
    :ensure t
    ))
 (t
  (use-package flycheck-yamllint
    :ensure t
    :after (flycheck)
    ))
 )

(provide 'setup-yaml-mode)
