(use-package tree-sitter
  :ensure t
  :diminish tree-sitter-mode
  :init (global-tree-sitter-mode)
;;  :hook (
;;         (typescript-mode . tree-sitter-hl-mode)
;;         )
  )

(use-package tree-sitter-langs
  :ensure t
  :after (tree-sitter)
  )

(provide 'setup-tree-sitter)
