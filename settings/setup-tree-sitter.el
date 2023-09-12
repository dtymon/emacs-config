(use-package tree-sitter
  :ensure t
  :diminish tree-sitter-mode
  :init
  (global-tree-sitter-mode)

  :config
  ;; (set-face-attribute 'tree-sitter-hl-face:property nil :slant 'normal)
  ;; (set-face-attribute 'tree-sitter-hl-face:function.call nil :weight 'normal)
  ;; (set-face-attribute 'tree-sitter-hl-face:method.call nil :weight 'normal)
  ;; (set-face-attribute 'tree-sitter-hl-face:string.special nil :weight 'normal)
  ;;
  ;; (set-face-foreground 'tree-sitter-hl-face:function.call
  ;;                      (face-foreground 'font-lock-function-name-face))
  ;; (set-face-foreground 'tree-sitter-hl-face:method.call
  ;;                      (face-foreground 'font-lock-function-name-face))
  ;; (set-face-foreground 'tree-sitter-hl-face:variable.builtin
  ;;                      (face-foreground 'font-lock-keyword-face))

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
