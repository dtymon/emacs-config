(use-package vterm
  :ensure t
  :config
  ;; (setq term-prompt-regexp "^\nzsh .*\n[0-9][0-9]:[0-9][0-9][AP]M .*> ")

  (defun dtymon::vterm-turn-off-chrome ()
    (hl-line-mode -1)
    (display-line-numbers-mode -1))

  (let ((map vterm-mode-map))
    (define-key map (kbd "C-c C-x") #'vterm--self-insert)
    )
  :hook (vterm-mode . dtymon::vterm-turn-off-chrome)
  )

;; (use-package vterm-toggle
;;   :ensure t
;;   :custom
;;   (vterm-toggle-fullscreen-p nil "Open a vterm in another window.")
;;   (vterm-toggle-scope 'project)
;;   :bind (
;;          ("C-c t" . #'vterm-toggle)
;;          :map vterm-mode-map
;;          ("s-t" . #'vterm) ; Open up new tabs quickly
;;          )
;;   )

(provide 'setup-vterm)
