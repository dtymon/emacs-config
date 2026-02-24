;; -*- lexical-binding: t -*-

(defun dtymon::json-mode-hook ()
  ;; (flycheck-mode 1)
  (prettier-mode 1)
  (treesit-fold-indicators-mode 1)

  (make-local-variable 'js-indent-level)
  (setq js-indent-level 2))

(use-package json-mode
  :ensure t
  :mode "\\.json"
  :bind (:map json-mode-map
              ("C-c l" . json-ts-jq-path-at-point)
              )
  :init
  (add-hook 'json-mode-hook #'dtymon::json-mode-hook)
  )

(use-package json-ts-mode
  :ensure t
  :mode "\\.json"
  :bind (:map json-ts-mode-map
              ("C-c l" . json-ts-jq-path-at-point)
              )
  :init
  (add-hook 'json-ts-mode-hook #'dtymon::json-mode-hook)
  )

(provide 'setup-json-mode)
