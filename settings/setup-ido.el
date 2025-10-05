;; -*- lexical-binding: t -*-

(use-package ido
  :ensure t
  :after (recentf)
  :custom
  (ido-enable-prefix                      nil)
  (ido-enable-flex-matching               nil)
  (ido-case-fold                          t)
  (ido-auto-merge-work-directories-length 0)
  (ido-create-new-buffer                  'prompt)
  (ido-use-filename-at-point               nil)
  (ido-max-prospects                       20)
  (ido-ignore-buffers                     '("\\` " "\*EGLOT"))
  ;; Don't raise frames to show selected buffer
  (ido-default-buffer-method 'selected-window)
  ;; Always rescan buffer for imenu
  (imenu-auto-rescan t)

  ;; Fix for the warnings produced by ido-ubiquitous
  ;; (defvar ido-cur-item nil)
  ;; (defvar ido-default-item nil)
  ;; (defvar ido-cur-list nil)
  ;; (defvar ido-require-match nil)

  :bind (:map ido-file-dir-completion-map
              ("<up>" . previous-history-element)
              ("<down>" . next-history-element)

              :map global-map
              ("C-x b" . ido-switch-buffer)
              ("C-x C-b" . ibuffer))

  :config
  (ido-mode 'buffers)
  )

;; Ido at point <M-tab>
;; (use-package ido-at-point
;;   :ensure t
;;   :config
;;   (ido-at-point-mode)
;;   )
;;
;; (use-package ido-completing-read+
;;   :ensure t
;;   :config
;;   (ido-ubiquitous-mode 1)
;;   )

;;(use-package ido-vertical-mode
;;  :ensure t
;;  :config
;;  (ido-vertical-mode 1)
;;  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
;;  )

(provide 'setup-ido)
