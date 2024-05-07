;; Save a list of recent files visited. (open recent file with C-x f)
(use-package recentf
  :init
  (setq recentf-max-saved-items 100)
  :config
  (recentf-mode 1)

  ;; Do not include any elpa files and other state-related files
  (add-to-list 'recentf-exclude "/\\.emacs\\.d/\\(var\\|elpa\\|\\.cache\\)/")
  (add-to-list 'recentf-exclude "^/private/var/")
  )

(provide 'setup-recentf)
