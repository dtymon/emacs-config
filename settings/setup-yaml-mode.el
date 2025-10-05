;; -*- lexical-binding: t -*-

(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml"
  :config
  (setq yaml-indent-offset 2)
  (add-hook 'yaml-mode-hook (lambda ()
                              (cond
                               (use-flymake (flymake-mode 1))
                               (t (flycheck-mode 1))
                               )
                              ;; (prettier-mode 1)
                            ))
  )

(cond
 (use-flymake
  (use-package flymake-yamllint
    :ensure t
    :config
    (add-hook 'yaml-mode-hook 'flymake-yamllint-setup)
    ))
 (t
  (use-package flycheck-yamllint
    :ensure t
    :after (flycheck)
    ))
 )

(provide 'setup-yaml-mode)
