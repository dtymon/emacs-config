(require 'grep)

(use-package wgrep
  :ensure t
  :bind (:map grep-mode-map
              ("E" . wgrep-change-to-wgrep-mode)
              ("C-x C-s" . wgrep-save-all-buffers)
              )
  :init
  (setq wgrep-too-many-file-length 50)
  )

(provide 'setup-wgrep)
