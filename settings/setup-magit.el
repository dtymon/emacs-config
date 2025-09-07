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

  ;; :custom
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

  ;; Remove some extra git stuff of little value to speed up magit
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)
)

(defun dtymon::maybe-bind-git-timemachine ()
  (when (and (buffer-file-name)
             (string= (vc-backend (buffer-file-name)) "Git"))
    (local-set-key (kbd "C-c t m") #'git-timemachine-toggle))
  )

(defun dtymon::tweak-git-timemachine-visuals ()
  (display-line-numbers-mode 1)
  (setq display-line-numbers 'absolute)
  (hl-line-mode 1)
  )

(use-package git-timemachine
  :ensure t
  :config
  (add-hook 'find-file-hook #'dtymon::maybe-bind-git-timemachine)
  (advice-add #'git-timemachine-show-revision :after
              (lambda (&rest _)
                (dtymon::tweak-git-timemachine-visuals)))
  )

(provide 'setup-magit)
