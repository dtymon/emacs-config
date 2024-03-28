(use-package company
  :ensure t
  :defer t
  :blackout company-mode
  :init
  (global-company-mode)

  :bind (:map company-active-map
         ;; Return and arrow keys should behave as normal until we interact with
         ;; company
         ("<return>" . nil)
         ("RET"      . nil)
         ("<up>"     . nil)
         ("<down>"   . nil)

         ;; A single Escape aborts completion
         ("\e" . company-abort)

         ;; When we are interacting with company, then Return and arrow keys do
         ;; different things.
         :map company-active-map
         :filter (company-explicit-action-p)

         ("<return>" . company-complete-selection)
         ("RET"      . company-complete-selection)
         ("<up>"     . company-select-previous)
         ("<down>"   . company-select-next)
         )
  :bind* (
          ;; Make sure that M-TAB always starts completion and is never
          ;; overridden in other modes
          ("M-TAB" . company-manual-begin)
          ("C-c o" . company-other-backend)
          )
  :custom
   ;; How many chars before showing suggestions
  (company-minimum-prefix-length 1)

   ;; How long before company appears
  (company-idle-delay .1)
  (company-tooltip-idle-delay .2)

  ;; Do not force a required match, must allow something to be added that is
  ;; not in the candidate list.
  (company-require-match nil)

  ;; Set the maximum number of candidates to show
  (company-tooltip-limit 10)
  (company-tooltip-width-grow-only t)

  ;; Show the entire suggestion
  (company-tooltip-minimum company-tooltip-limit)

  ;; Align tooltips to the right
  (company-tooltip-align-annotations t)

  ;; By default, use the pseudo-tooltip frontend which is the dropdown menu
  ;; approach.
  ;;
  ;; FYI, company-preview-frontend shows the top selection like it was inserted.
  ;; No menu is shown. company-echo-frontend shows candidates in the echo area
  (company-frontends '(company-pseudo-tooltip-frontend))

  ;; Prevent dabbrev completions from being lowercased and make the matching
  ;; case-sensitive.
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case nil)

  ;; Whether to search other buffers for dabbrev matches: nil, t or 'all. A
  ;; value of t limits the search to buffers of the same major mode.
  (company-dabbrev-other-buffers t)

  :config
  ;; Setup other transformers to sort the list of candidates and remove
  ;; duplicates
  (add-to-list 'company-transformers #'delete-dups)
  (add-to-list 'company-transformers #'company-sort-by-backend-importance)
  ;; (add-to-list 'company-transformers #'company-sort-prefer-same-case-prefix)
  ;; (add-to-list 'company-transformers #'company-sort-by-occurrence)

  ;; There doesn't seem any easy way to only consider completions that are
  ;; anchored to the start. Candidates can include entries with the prefix
  ;; appearing in the middle (funny, it is even referred to as the prefix by
  ;; company and yet it is not used as a prefix, go figure).
  ;;
  ;; To achieve anchoring to the start, define a transformer function that
  ;; will remove any candidate that does not start with the prefix. Not sure
  ;; that this is legitimate actions for a transformer but it appears to work.
  (defun dtymon::company-transformer-enforce-prefix (candidates)
    (cl-delete-if (lambda (k) (not (s-starts-with? company-prefix k))) candidates)
    )

  ;; And set this as the first transformer
  (add-to-list 'company-transformers #'dtymon::company-transformer-enforce-prefix)

;;   ;; Setup completion backends for programming modes
;;   (add-hook 'prog-mode-hook
;;             (lambda ()
;;               (set (make-local-variable 'company-backends)
;;                    '(
;;                      (
;; ;;                      company-semantic
;;                       company-capf
;;                       :with
;;                       company-yasnippet
;;                       company-dabbrev-code
;;                       company-keywords
;;                       )
;;                      company-files
;;                      )
;;                   )))
  )

(provide 'setup-company)
