;; -*- lexical-binding: t -*-

(use-package saveplace
  :init
  (setq save-place-file (no-littering-expand-var-file-name "save-places.el"))
  (save-place-mode)
  )

(provide 'setup-saveplace)
