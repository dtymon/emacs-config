(use-package desktop
  :config
  (setq
   desktop-auto-save-timeout 300
   desktop-base-file-name "desktop"
   desktop-files-not-to-save ".*"
   desktop-buffers-not-to-save ".*"
   desktop-globals-to-clear nil
   desktop-load-locked-desktop t
   desktop-missing-file-warning nil
   desktop-restore-eager 0
   desktop-restore-frames 0
   desktop-save 'ask-if-new
   )

  (dolist (symbol '(kill-ring log-edit-comment-ring))
    (add-to-list 'desktop-globals-to-save symbol))

  (desktop-save-mode 1)
  )

(provide 'setup-desktop)
