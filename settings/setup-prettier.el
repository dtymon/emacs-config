(use-package prettier
  :ensure t
  :blackout prettier-mode
  :init
  (setq prettier-mode-sync-config-flag nil)
  )

(provide 'setup-prettier)
