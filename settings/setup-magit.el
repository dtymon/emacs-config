(require 'vc-annotate)

(defun vc-annotate-quit ()
  "Restores the previous window configuration and kills the vc-annotate buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :vc-annotate-fullscreen))

(use-package magit
  :ensure t
  :bind (
         ("C-c g"     . magit-dispatch)
         ("C-c f"     . magit-file-dispatch)
         ("C-x m"     . magit-status)
         :map magit-status-mode-map
         ("S-<prior>" . magit-section-up)
         )

  :init
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
        magit-bury-buffer-function #'magit-restore-window-configuration
        magit-auto-revert-mode nil
        magit-define-global-key-bindings 'recommended
        )
  )

(use-package git-timemachine
  :ensure t
  )

(provide 'setup-magit)
