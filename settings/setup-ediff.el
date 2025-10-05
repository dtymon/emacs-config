;; -*- lexical-binding: t -*-

(use-package ediff
  :defer t
  :config
  ;; (add-hook 'ediff-after-quit-hook-internal 'winner-undo)
  (setq
   ;; Don't ignore whitespace since it is important for Python
   ;; ediff-diff-options "-w"
   ediff-split-window-function 'split-window-horizontally
   ediff-window-setup-function 'ediff-setup-windows-plain
   )

  (set-face-attribute 'ediff-even-diff-A nil :background "#2a3a5a")
  (set-face-attribute 'ediff-even-diff-B nil :background "#2a3a5a")
  )

(provide 'setup-ediff)
