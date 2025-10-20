;; -*- lexical-binding: t -*-

(use-package verb
  :ensure t
  ;; :bind (:map org-mode-map
  ;;             ("C-c C-r" . verb-command-map)
  ;;             )
  :config
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
  (setq
   verb-suppress-load-unsecure-prelude-warning t
   verb-auto-kill-response-buffers             t
   verb-auto-show-headers-buffer               t
   )
  )

(provide 'setup-verb)
