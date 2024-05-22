(use-package visual-regexp
  :ensure t
  :config (define-key global-map (kbd "M-%")
                      (lambda (&optional prefix)
                        (interactive "P")
                        (call-interactively
                         (if prefix  #'vr/replace #'vr/query-replace)
                         ))))

(provide 'setup-visual-regexp)
