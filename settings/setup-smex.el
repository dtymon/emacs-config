;; -*- lexical-binding: t -*-

(use-package smex
  :ensure t
  :init
  (smex-initialize)
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         )
  )

(provide 'setup-smex)
