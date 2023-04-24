(use-package yasnippet
  :ensure t
  :mode (("yasnippet/snippets" . snippet-mode)
         ("\\.yasnippet\\'" . snippet-mode))
  :config
  (yas-global-mode 1)

  (define-key yas-minor-mode-map "\C-j" 'yas-expand)
  (define-key yas-keymap "\C-j" 'yas-next-field-or-maybe-expand)
  (dolist (keymap (list yas-minor-mode-map yas-keymap))
    (define-key keymap (kbd "TAB") nil)
    (define-key keymap [(tab)] nil))

;;  (setq
;;   yas-snippet-dirs '((expand-file-name "snippets" user-emacs-directory))
;;   )
  )

(provide 'setup-yasnippet)
