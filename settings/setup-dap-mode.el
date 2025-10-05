;; -*- lexical-binding: t -*-

(use-package dap-mode
  :ensure t
  :config
  (dap-ui-mode 1)
  (dap-ui-controls-mode 1)

  :custom
  (dap-python-debugger 'debugpy)
  )

(provide 'setup-dap-mode)
