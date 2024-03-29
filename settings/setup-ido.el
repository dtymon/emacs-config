(use-package ido
  :ensure t
  :after (recentf)
  :config
  (ido-mode 'buffers)

  ;; Always rescan buffer for imenu
  (set-default 'imenu-auto-rescan t)

  ;;(setq ido-enable-prefix nil
  ;;      ido-enable-flex-matching t
  ;;      ido-case-fold nil
  ;;      ido-auto-merge-work-directories-length -1
  ;;      ido-create-new-buffer 'always
  ;;      ido-use-filename-at-point nil
  ;;      ido-max-prospects 10)
  (setq
   ido-enable-prefix                      nil
   ido-enable-flex-matching               nil
   ido-case-fold                          t
   ido-auto-merge-work-directories-length 0
   ido-create-new-buffer                  'prompt
   ido-use-filename-at-point               nil
   ido-max-prospects                       20
   ido-ignore-buffers                     '("\\` " "\*EGLOT")

   ;; Don't raise frames to show selected buffer
   ;; ido-default-buffer-method 'raise-frame
   ido-default-buffer-method 'selected-window
   )

  ;; Fix for the warnings produced by ido-ubiquitous
  (defvar ido-cur-item nil)
  (defvar ido-default-item nil)
  (defvar ido-cur-list nil)
  (defvar ido-require-match nil)


  :bind (:map ido-file-dir-completion-map
              ("<up>" . previous-history-element)
              ("<down>" . next-history-element)
              )

  :bind (:map global-map
              ;; Use beframe for default bindings and ido for capitalised
              ;; versions
              ("C-x b" . ido-switch-buffer)
              ("C-x B" . ibuffer)
              )

;; (add-to-list 'ido-ignore-directories "target")
;; (add-to-list 'ido-ignore-directories "node_modules")
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
