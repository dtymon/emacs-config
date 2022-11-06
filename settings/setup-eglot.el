(use-package eglot
  :ensure t
  :commands eglot
  :config
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends)
                   '(
                     (
                      company-semantic
                      company-capf
                      :with
                      company-yasnippet
                      company-dabbrev-code
                      company-keywords
                      )
                     company-files
                     )
                   )))

;;                                     (add-to-list 'company-backends
;;                                                  '(company-capf :with company-yasnippet))))

  ;; (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t)
  ;; (add-to-list 'eglot-ignored-server-capabilites :hoverProvider)
)

(provide 'setup-eglot)
