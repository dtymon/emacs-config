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

  :custom
  (magit-git-executable "/opt/homebrew/bin/git")
  ;; (magit-git-executable "/usr/bin/git")
  ;; enable this to get an idea on where all the time is spent
  ;; (magit-refresh-verbose t)

  :init
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
        magit-bury-buffer-function #'magit-restore-window-configuration
        magit-auto-revert-mode nil
        magit-define-global-key-bindings 'recommended
        magit-section-initial-visibility-alist '((stashes . hide)
                                                 (untracked . show)
                                                 )
        )

  :config
  (add-hook 'git-commit-setup-hook
            (lambda ()
              (setq-local fill-column 68)
              (turn-on-auto-fill))
            )

  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)
)

(use-package git-timemachine
  :ensure t
  )

(provide 'setup-magit)
