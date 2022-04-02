(use-package restclient
  :mode ("\\.http\\'" . restclient-mode)
  :ensure t
  :defer t
  :config
  (add-hook 'restclient-mode-hook (lambda ()
                                    (setq lexical-binding t)))

  :bind (:map restclient-mode-map
              (
               ("C-c C-C" . restclient-http-send-current-stay-in-window)
               ))
  )

(require 'restclient-jq)
(provide 'setup-restclient)
