
(use-package vterm
  :ensure t
  :config
  (defun dtymon::vterm-turn-off-chrome ()
    (hl-line-mode -1)
    (display-line-numbers-mode -1))
  :hook (vterm-mode . dtymon::vterm-turn-off-chrome)
  )

(use-package vterm-toggle
  :ensure t
  :custom
  (vterm-toggle-fullscreen-p nil "Open a vterm in another window.")
  (vterm-toggle-scope 'projectile)
  :bind (
         ("C-c t" . #'vterm-toggle)
         :map vterm-mode-map
         ("s-t" . #'vterm) ; Open up new tabs quickly
         )
  )

(provide 'setup-vterm)
