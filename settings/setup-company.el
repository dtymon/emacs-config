(use-package company
  :ensure t
  :defer t
  :diminish company-mode
  :init
  (global-company-mode)
  (setq company-idle-delay nil)
  (setq company-echo-delay 0)

  :bind (:map company-active-map
              ;; Return and arrow keys should behave as normal until we interact
              ;; with company
              ("<return>" . nil)
              ("RET" . nil)
              ("<up>" . nil)
              ("<down>" . nil)

              ;; A single Escape aborts completion
              ("\e" . company-abort)

              ;; When we are interacting with company, then Return and arrow
              ;; keys do different things.
              :map company-active-map
              :filter (company-explicit-action-p)

              ("<return>" . company-complete-selection)
              ("RET" . company-complete-selection)
              ("<up>" . company-select-previous)
              ("<down>" . company-select-next)
              )
  :bind* (
          ;; Make sure that M-TAB always starts completion and is never
          ;; overridden in other modes
          ("M-TAB" . company-manual-begin)
          )

  :config
  ;; Show completions after a short delay
  (setq company-idle-delay 0.4)

  ;; Show completions after typing a two characters
  (setq company-minimum-prefix-length 2)

  ;; Set the maximum number of candidates to show
  (setq company-tooltip-limit 15)

  ;; Show the entire suggestion
  (setq company-tooltip-minimum company-tooltip-limit)

  ;; Always display suggestions in the tooltip, even if there is only one. Also,
  ;; don't display metadata in the echo area. (This conflicts with ElDoc.)
  (setq company-frontends '(company-pseudo-tooltip-frontend))

  ;; Show quick-reference numbers in the tooltip. (Select a completion with M-1
  ;; through M-0.)
  (setq company-show-numbers t)

  ;; Prevent non-matching input (which will dismiss the completions menu), but
  ;; only if the user interacts explicitly with Company.
  (setq company-require-match #'company-explicit-action-p)

  ;; Company appears to override our settings in `company-active-map' based on
  ;; `company-auto-complete-chars'. Turning it off ensures we have full control.
  (setq company-auto-complete-chars nil)

  ;; Prevent Company completions from being lowercased in the completion menu
  (setq company-dabbrev-downcase nil)

  ;; Only search the current buffer to get suggestions for company-dabbrev (a
  ;; backend that creates suggestions from text found in your buffers). This
  ;; prevents Company from causing lag once you have a lot of buffers open.
  ;; (setq company-dabbrev-other-buffers nil)

  ;; Make company-dabbrev case-sensitive
  (setq company-dabbrev-ignore-case nil)
  )

;; This package adds usage-based sorting to company completions
(use-package company-statistics
  :ensure t
  :defer t
  :config
  (company-statistics-mode 1)
  )

(use-package company-quickhelp
  :ensure t
  :defer t
  :init (add-hook 'global-company-mode-hook #'company-quickhelp-mode)
  )

;;(use-package company-box
;;  :ensure t
;;  :defer t
;;  :hook (company-mode . company-box-mode)
;;  )

(use-package company-go
  :ensure t
  :defer t
  )

(use-package company-restclient
  :ensure t
  :defer t
  )

(provide 'setup-company)
