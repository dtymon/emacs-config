;; -*- lexical-binding: t -*-

(use-package yasnippet
  :ensure t
  :blackout yas-minor-mode
  :mode (("yasnippet/snippets" . snippet-mode)
         ("\\.yasnippet\\'" . snippet-mode))
  :bind (
         :map yas-minor-mode-map
         ("C-j" . yas-expand)
         ("TAB" . nil)
         :map yas-keymap
         ("C-j" . yas-next-field-or-maybe-expand)
         ("TAB" . nil)
         )

  :config
  (yas-global-mode 1)

;;  (setq
;;   yas-snippet-dirs '((expand-file-name "snippets" user-emacs-directory))
;;   )
  )

(provide 'setup-yasnippet)
