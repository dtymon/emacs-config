(use-package savehist
  :init
  (setq history-length 1000)
  :config
  ;; Additional things to save
  (setq savehist-additional-variables '(mark-ring
                                        global-mark-ring
                                        search-ring
                                        regexp-search-ring
                                        extended-command-history))

  (savehist-mode 1)
  )

(provide 'setup-savehist)
