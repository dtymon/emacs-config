(use-package move-text
  :ensure t
  :bind (("<C-S-down>" . move-text-down)
         ("<C-S-up>" . move-text-up)
         )
  )

(provide 'setup-move-text)
