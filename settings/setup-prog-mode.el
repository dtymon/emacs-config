(use-package prog-mode
  :hook
  (prog-mode . hl-line-mode)
  (prog-mode . display-fill-column-indicator-mode)
  )

(provide 'setup-prog-mode)
