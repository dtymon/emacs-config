(use-package helm
  :ensure t
  :blackout
  )

(use-package helm-icons
  :ensure t
  :custom (helm-icons-provider 'all-the-icons)
  :config (helm-icons-enable)
  )

(provide 'setup-helm)
