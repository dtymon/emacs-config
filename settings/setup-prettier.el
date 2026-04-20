;; -*- lexical-binding: t -*-

(defun dtymon::post-prettier-format ()
  "Function to run after prettier has formatted the buffer."
  ;; After the buffer has been formatted by prettier, it is possible that any
  ;; flycheck errors rising from incorrect formatting could now be resolved.
  ;; This change of state is not always detected by flycheck so schedule a clear
  ;; and then a reassessment of the buffer after formatting has completed.
  (when (bound-and-true-p flycheck-mode)
    (run-at-time 1.25 nil #'flycheck-clear)
    (run-at-time 1.75 nil #'flycheck-buffer)
    ))

(use-package prettier
  :ensure t
  :init
  (setq prettier-mode-sync-config-flag nil)
  :config
  (advice-add 'prettier-prettify :after #'dtymon::post-prettier-format)
  )

(provide 'setup-prettier)
