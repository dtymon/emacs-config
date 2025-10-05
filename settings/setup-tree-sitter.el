;; -*- lexical-binding: t -*-

(use-package tree-sitter
  :ensure t
  :blackout tree-sitter-mode
  :init
  (global-tree-sitter-mode)
  )

(use-package tree-sitter-langs
  :ensure t
  :after (tree-sitter)
  )

;; (use-package treesit-auto
;;   :config
;;   (global-treesit-auto-mode)
;;   )

(provide 'setup-tree-sitter)
