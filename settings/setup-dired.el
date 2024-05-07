(defun dtymon::dired-first-row ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 3))

(defun dtymon::dired-last-row ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(use-package dired
  :bind (:map dired-mode-map
              ("k" . dired-do-delete)
              ("E" . wdired-change-to-wdired-mode)
              ;; Going to start of buffer is better if it goes to the first
              ;; file and similarly for end of buffer.
              ([remap beginning-of-buffer] . dtymon::dired-first-row)
              ([remap end-of-buffer] . dtymon::dired-last-row)
         )

  :config
  ;; Move files between split panes
  ;;(setq dired-dwim-target t)

  (use-package treemacs-icons-dired
    :if (display-graphic-p)
    :config (treemacs-icons-dired-mode))
  )

;; Writable dired
(use-package wdired
  :bind (:map wdired-mode-map
              ;; Going to start of buffer is better if it goes to the first
              ;; file and similarly for end of buffer.
              ([remap beginning-of-buffer] . dtymon::dired-first-row)
              ([remap end-of-buffer] . dtymon::dired-last-row)
         )
  :config
  ;; Reload dired once wdired either commits or discards its changes
  (require 'dash)
  (--each '(dired-do-rename
            dired-do-copy
            dired-create-directory
            wdired-abort-changes)
    (eval `(defadvice ,it (after revert-buffer activate)
             (revert-buffer))))

  )

(provide 'setup-dired)
