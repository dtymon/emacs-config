;; -*- lexical-binding: t -*-

(use-package prettier
  :ensure t
  :blackout (prettier-mode . " Pr")
  :init
  (setq prettier-mode-sync-config-flag nil)
  )

(provide 'setup-prettier)
