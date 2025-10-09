;; -*- lexical-binding: t -*-

(use-package verb
  :ensure t
  ;; :bind (:map org-mode-map
  ;;             ("C-c C-r" . verb-command-map)
  ;;             )
  :config
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map)

  :custom
  verb-suppress-load-unsecure-prelude-warning t
  )

(provide 'setup-verb)
