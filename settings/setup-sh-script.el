(use-package sh-script
  :config
  (add-hook 'sh-mode-hook
            (lambda ()
              (display-fill-column-indicator-mode 1)
              ))
  )

(provide 'setup-sh-script)
