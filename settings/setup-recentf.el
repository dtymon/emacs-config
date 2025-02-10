;; Save a list of recent files visited. (open recent file with C-x f)
(use-package recentf
  :init
  (setq recentf-max-saved-items 500)
  :config
  (recentf-mode 1)

  ;; Do not include any elpa files and other state-related files
  (add-to-list 'recentf-exclude "/\\.emacs\\.d/\\(var\\|elpa\\|\\.cache\\)/")
  (add-to-list 'recentf-exclude "^/private/var/")

  (let ((map global-map))
    (define-key map (kbd "C-x C-S-r") #'find-file-read-only)
    (define-key map (kbd "C-x C-r") #'recentf-open)
    )
  )

(provide 'setup-recentf)
