(use-package python
  :ensure t
  :diminish "Py"
  :commands python-mode
  :init
  ;; Enable either eglot or lsp depending on the current preference
  (add-hook 'python-mode-hook
            (cond (use-flymake 'eglot-ensure)
                  (t 'lsp-deferred)))

  (add-hook 'python-mode-hook
            (lambda ()
              ;; Show the fill column indicator
              (display-fill-column-indicator-mode 1)

              (setq
               tab-width 4
               python-indent-offset 4

               ;; Long lines should wrap
               truncate-lines nil

               ;; This is the fill column for comments
               fill-column 80
               )
              ))
  )

(use-package python-black
  :ensure t
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))

(provide 'setup-python)
