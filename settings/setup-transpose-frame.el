(use-package transpose-frame
  :ensure t
  :bind (:map global-map
              ("C-x C-<left>" . rotate-frame-clockwise)
              )
  )

(provide 'setup-transpose-frame)
