(use-package company
  :ensure t
  :defer t
  :diminish company-mode
  :init
  (global-company-mode)
  (setq company-idle-delay nil)
  (setq company-echo-delay 0)

  :bind (
         ;; Replace `completion-at-point' and `complete-symbol' with
         ;; `company-manual-begin'.
         ([remap completion-at-point] . company-manual-begin)
         ([remap complete-symbol] . company-manual-begin)

         ;; The following are keybindings that take effect whenever the
         ;; completions menu is visible, even if the user has not explicitly
         ;; interacted with company.

         :map company-active-map

         ;; Make TAB always complete the current selection
         ("<tab>" . company-complete-selection)

         ;; Prevent SPC from ever triggering a completionxs
         ("SPC" . nil)

         ;; The following are keybindings that only take effect if the user has
         ;; explicitly interacted with company.

         :map company-active-map
         :filter (company-explicit-action-p)

         ;; Make RET trigger a completion if and only if the user has explicitly
         ;; interacted with Company.
         ("<return>" . company-complete-selection)

         ;; We then do the same for the up and down arrows. Note that we use
         ;; `company-select-previous' instead of
         ;; `company-select-previous-or-abort'. I think the former makes more
         ;; sense since the general idea of this `company' configuration is to
         ;; decide whether or not to steal keypresses based on whether the user
         ;; has explicitly interacted with `company', not based on the number of
         ;; candidates.
         ("<up>" . company-select-previous)
         ("<down>" . company-select-next)

         ("C-M-i" . company-complete-common)
         )

  :bind* (
          ;; The default keybinding for `completion-at-point' and
          ;; `complete-symbol' is M-TAB or equivalently C-M-i. Here we make sure
          ;; that no minor modes override this keybinding.
          ("M-TAB" . company-manual-begin)
          )

  :config
  ;; Show completions instantly, rather than after half a second.
  (setq company-idle-delay 0)

  ;; Show completions after typing a single character, rather than after typing
  ;; three characters.
  (setq company-minimum-prefix-length 1)

  ;; Show a maximum of 10 suggestions. This is the default but I think it's best
  ;; to be explicit.
  (setq company-tooltip-limit 10)

  ;; Always display the entire suggestion list onscreen, placing it above the
  ;; cursor if necessary.
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

  ;; Prevent Company completions from being lowercased in the completion menu.
  ;; This has only been observed to happen for comments and strings in Clojure.
  (setq company-dabbrev-downcase nil)

  ;; Only search the current buffer to get suggestions for company-dabbrev (a
  ;; backend that creates suggestions from text found in your buffers). This
  ;; prevents Company from causing lag once you have a lot of buffers open.
  (setq company-dabbrev-other-buffers nil)

  ;; Make company-dabbrev case-sensitive. Case insensitivity seems like a great
  ;; idea, but it turns out to look really bad when you have domain-specific
  ;; words that have particular casing.
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
