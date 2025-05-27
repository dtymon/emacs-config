(use-package transpose-frame
  :ensure t
  :bind (:map global-map
              ("C-x M-<left>" . rotate-frame-clockwise)
              ("C-x M-<right>" . rotate-frame-anticlockwise)
              )
  )

(provide 'setup-transpose-frame)
