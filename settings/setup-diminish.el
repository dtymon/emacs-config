(use-package diminish
  :ensure t
  :config
  (eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
  (eval-after-load "eldoc" '(diminish 'eldoc-mode))
  (eval-after-load "paredit" '(diminish 'paredit-mode))
  (eval-after-load "tagedit" '(diminish 'tagedit-mode))
  (eval-after-load "elisp-slime-nav" '(diminish 'elisp-slime-nav-mode))
  (eval-after-load "smartparens" '(diminish 'smartparens-mode))
  (eval-after-load "guide-key" '(diminish 'guide-key-mode))
  (eval-after-load "whitespace-cleanup-mode" '(diminish 'whitespace-cleanup-mode))
  (eval-after-load "subword" '(diminish 'subword-mode))
  )

(provide 'setup-diminish)
