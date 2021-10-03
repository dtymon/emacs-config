(use-package company
  :ensure t
  :defer t
  :diminish company-mode
  :init
  (global-company-mode)
;;DAVIDT   (setq company-idle-delay nil)
;;DAVIDT   (setq company-echo-delay 0)

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
  (setq
;;DAVIDT   ;; Show completions after a short delay
;;DAVIDT   company-idle-delay 0.4

   ;; Show completions after typing one character
   company-minimum-prefix-length 1

   ;; Set the maximum number of candidates to show
   company-tooltip-limit 15

   ;; Show quick-reference numbers in the tooltip. (Select a completion with M-1
   ;; through M-0.)
   company-show-numbers t

   ;; Show the entire suggestion
   ;; company-tooltip-minimum company-tooltip-limit

;;DAVIDT   ;; Always display suggestions in the tooltip, even if there is only one. Also,
;;DAVIDT   ;; don't display metadata in the echo area. (This conflicts with ElDoc.)
;;DAVIDT   company-frontends '(company-pseudo-tooltip-frontend)

   ;; Prevent non-matching input (which will dismiss the completions menu), but
   ;; only if the user interacts explicitly with Company.
   ;; company-require-match #'company-explicit-action-p

;;DAVIDT   ;; Company appears to override our settings in `company-active-map' based on
;;DAVIDT   ;; `company-auto-complete-chars'. Turning it off ensures we have full control.
;;DAVIDT   setq company-auto-complete-chars nil

   ;; Prevent dabbrev completions from being lowercased and make the matching
   ;; case-sensitive.
   company-dabbrev-downcase nil
   company-dabbrev-ignore-case nil

   ;; Whether to search other buffers for dabbrev matches: nil, t or 'all. A
   ;; value of t limits the search to buffers of the same major mode.
   ;; company-dabbrev-other-buffers nil
   )
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
  :init
  (add-hook 'global-company-mode-hook #'company-quickhelp-mode)
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
