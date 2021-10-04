(use-package company
  :ensure t
  :defer t
  :diminish company-mode
  :init
  (global-company-mode)

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
   ;; How many chars before showing suggestions
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

  ;; Setup company-tng
  (add-hook 'after-init-hook 'company-tng-mode)
  (define-key company-active-map (kbd "TAB") 'company-select-next)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  (define-key company-active-map (kbd "RET") nil)
  (setq company-require-match nil)


;; ;;   (defun mars/company-backend-with-yas (backends)
;; ;;     "Add :with company-yasnippet to company BACKENDS.
;; ;; Taken from https://github.com/syl20bnr/spacemacs/pull/179."
;; ;;     (if (and (listp backends) (memq 'company-yasnippet backends))
;; ;;         backends
;; ;;       (append (if (consp backends)
;; ;;                   backends
;; ;;                 (list backends))
;; ;;               '(:with company-yasnippet))))
;; ;;
;; ;;   ;; add yasnippet to all backends
;; ;; ;;  (setq company-backends
;; ;; ;;        (mapcar #'mars/company-backend-with-yas company-backends))
;; ;;   (setq company-backends (push 'company-yasnippet company-backends))
;; ;;   (setq company-backends '(company-yasnippet company-capf))
;; ;;   (setq company-backends '(company-capf :with company-yasnippet))
;;
   ;; Add yasnippet support for all company backends
   ;; https://github.com/syl20bnr/spacemacs/pull/179
   (defvar company-mode/enable-yas t
     "Enable yasnippet for all backends.")

   (defun company-mode/backend-with-yas (backend)
     (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
         backend
       (append (if (consp backend) backend (list backend))
               '(:with company-yasnippet)
               )))

   (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
;;   (setq company-backends '((:separate company-yasnippet company-capf)))
;;   (message "%s" company-backends)
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
