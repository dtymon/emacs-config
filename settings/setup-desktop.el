;; -*- lexical-binding: t -*-

;; This is not included any more. Desktop config is now in early-init
;; (use-package desktop
;;   :config
;;   (setq
;;    desktop-auto-save-timeout 300
;;    desktop-base-file-name "desktop"
;;    desktop-buffers-not-to-save ".*"
;;    desktop-files-not-to-save ".*"
;;    desktop-globals-to-clear nil
;;    desktop-load-locked-desktop t
;;    desktop-missing-file-warning nil
;;    desktop-restore-eager 0
;;    desktop-restore-frames 0
;;    desktop-save 'ask
;;    )
;;
;;   (dolist (symbol '(kill-ring log-edit-comment-ring read-expression-history))
;;     (add-to-list 'desktop-globals-to-save symbol))
;;
;;   (desktop-save-mode 1)
;;   )
;;
;; (provide 'setup-desktop)
