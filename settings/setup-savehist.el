;; -*- lexical-binding: t -*-

(use-package savehist
  :init
  (setq
   history-length            1000
   history-delete-duplicates t
   )

  :config
  ;; Additional things to save
  (setq savehist-additional-variables '(extended-command-history
                                        global-mark-ring
                                        ;; Don't persist as whole-buffer copies
                                        ;; can be huge.
                                        ;; kill-ring
                                        mark-ring
                                        regexp-search-ring
                                        search-ring))

  (savehist-mode 1)
  )

(provide 'setup-savehist)
