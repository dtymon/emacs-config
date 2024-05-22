(use-package which-key
  :ensure t
  :blackout which-key-mode
  :custom
  (which-key-idle-delay 2)
  (which-key-idle-secondary-delay 0.3)
  (which-key-popup-type 'side-window)
  (which-key-side-window-location 'bottom)
  (which-key-compute-remaps t)
  (which-key-allow-multiple-replacements t)

  :config
  (which-key-mode 1)
  )

(provide 'setup-which-key)
