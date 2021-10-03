(use-package which-key
  :ensure t
  :defer 10
  :config
  (setq which-key-popup-type 'minibuffer)
  (setq which-key-compute-remaps t)
  (setq which-key-allow-multiple-replacements t)
  (which-key-mode 1)
  )

(provide 'setup-which-key)
