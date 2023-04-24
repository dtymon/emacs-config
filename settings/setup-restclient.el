(use-package restclient
  :ensure t
  :mode ("\\.http\\'" . restclient-mode)
  :config
  (require 'restclient-jq)
  (add-hook 'restclient-mode-hook (lambda ()
                                    (setq lexical-binding t)))

  :bind (:map restclient-mode-map
              (
               ("C-c C-C" . restclient-http-send-current-stay-in-window)
               ))
  )

(use-package restclient-jq
  :ensure t
  :defer t
  )

(provide 'setup-restclient)
