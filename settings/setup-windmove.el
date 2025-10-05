;; -*- lexical-binding: t -*-

(use-package windmove
  :ensure t
  :init
  (windmove-default-keybindings)
  (setq windmove-wrap-around nil)
  )

(provide 'setup-windmove)
