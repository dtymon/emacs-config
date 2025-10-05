;; -*- lexical-binding: t -*-

(use-package elisp-slime-nav
  :ensure t
  :blackout elisp-slime-nav-mode
  :config
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (elisp-slime-nav-mode t)
              (eldoc-mode 1)
              ))
  )

(provide 'setup-elisp-slime-nav)
