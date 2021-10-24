(use-package company
  :ensure t
  :defer t
  :diminish company-mode
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
          )

  :config
  (setq
   ;; How many chars before showing suggestions
   company-minimum-prefix-length 2

   ;; How long before company appears
   company-idle-delay .1

   ;; Do not force a required match, must allow something to be added that is
   ;; not in the candidate list.
   company-require-match nil

   ;; Set the maximum number of candidates to show
   company-tooltip-limit 15
   company-tooltip-width-grow-only t

   ;; Show quick-reference numbers in the tooltip. (Select a completion with M-1
   ;; through M-0.)
   company-show-numbers t

   ;; Show the entire suggestion
   company-tooltip-minimum company-tooltip-limit

   ;; By default, use the pseudo-tooltip frontend which is the dropdown menu
   ;; approach.
   ;;
   ;; FYI, company-preview-frontend shows the top selection like it was
   ;; inserted. No menu is shown. company-echo-frontend shows candidates in the
   ;; echo area
   company-frontends '(company-pseudo-tooltip-frontend)

   ;; Prevent dabbrev completions from being lowercased and make the matching
   ;; case-sensitive.
   company-dabbrev-downcase nil
   company-dabbrev-ignore-case nil

   ;; Whether to search other buffers for dabbrev matches: nil, t or 'all. A
   ;; value of t limits the search to buffers of the same major mode.
   company-dabbrev-other-buffers t
   )

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

  ;; Setup other transformers to sort the list of candidates and remove
  ;; duplicates
  (add-to-list 'company-transformers #'delete-dups)
  (add-to-list 'company-transformers #'company-sort-prefer-same-case-prefix)
  (add-to-list 'company-transformers #'company-sort-by-occurrence)

  ;; Setup company-tng (tab-and-go). Seemed like a nice idea but could not find
  ;; a way to do "complete common" to be able to do progressive completions by
  ;; adding the prefix common to all candidates.
;;  (add-hook 'after-init-hook 'company-tng-mode)
;;  (define-key company-active-map (kbd "TAB") 'company-select-next)
;;  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
;;  (define-key company-active-map (kbd "RET") nil)

   ;; Setup completion backends for programming modes
   (add-hook 'prog-mode-hook
             (lambda ()
               (set (make-local-variable 'company-backends)
                    '(
                      (
                       company-yasnippet
                       company-capf
                       company-dabbrev
                       company-keywords
                       ))
                    )))
  )

;; TabNine looks good and worked OK. Needs to be looked into again at some
;; point.
;; (use-package company-tabnine
;;   :ensure t
;;   :requires company
;;   :config
;;   (setq company-tabnine--disable-next-transform nil)
;;   (defun my-company--transform-candidates (func &rest args)
;;     (if (not company-tabnine--disable-next-transform)
;;         (apply func args)
;;       (setq company-tabnine--disable-next-transform nil)
;;       (car args)))
;;
;;   (defun my-company-tabnine (func &rest args)
;;     (when (eq (car args) 'candidates)
;;       (setq company-tabnine--disable-next-transform t))
;;     (apply func args))
;;
;;   (advice-add #'company--transform-candidates :around #'my-company--transform-candidates)
;;   (advice-add #'company-tabnine :around #'my-company-tabnine)
;;   ;; Trigger completion immediately.
;;   ;; (setq company-idle-delay 0)
;;
;;   ;; Use the tab-and-go frontend.
;;   ;; Allows TAB to select and complete at the same time.
;; ;;   (company-tng-configure-default)
;;   (setq company-frontends
;;         '(company-tng-frontend
;;           company-pseudo-tooltip-frontend
;;           company-echo-metadata-frontend))
;;
;;   (dolist (mode (list
;;                'c-mode
;;                'c++-mode
;;                'swift-mode
;;                'lisp-mode
;;                'emacs-lisp-mode
;;                'sh-mode
;;                'lua-mode
;;                'haskell-mode
;;                'go-mode
;;                'java-mode
;;                'python-mode
;;                'typescript-mode
;;                ))
;;     (with-eval-after-load mode
;;       (add-to-list 'company-backends #'company-tabnine))
;;     )
;;
;;   (setq lsp-company-backends
;;         '(company-capf
;;           company-files
;;           company-yasnippet
;;           :separate
;;           company-tabnine
;;           ))
;;
;;   )

;; This package adds usage-based sorting to company completions
(use-package company-statistics
  :ensure t
  :defer t
  :config
  (company-statistics-mode 1)
  (setq
   company-statistics-size 2000
   )
  )

(use-package company-quickhelp
  :ensure t
  :defer t
  :init
  (add-hook 'global-company-mode-hook #'company-quickhelp-mode)
  )

;; A nice looking frontend
(use-package company-box
  :ensure t
  :defer t
  :hook (company-mode . company-box-mode)
  :init
  (setq
   company-box-icons-alist 'company-box-icons-all-the-icons
   company-box-tooltip-minimum-width 80
   company-box-tooltip-maximum-width 100
   ;; company-box-backends-colors nil
   company-box-show-single-candidate 'always
   company-box-max-candidates 10
   company-box-scrollbar nil

   ;; Make snippets standout more
   company-box-backends-colors
   '(
     (company-yasnippet . (:all "lime green" :selected (:background "lime green" :foreground "black")))
     )
   )
  )

;; company-posframe is causing a weird issue where the window is lowered after
;; a period of inactivity after completion has started, causing Emacs to be
;; placed behind all other windows.
;;
;;(use-package company-posframe
;;  :ensure t
;;  :defer t
;;  :hook (company-mode . company-posframe-mode)
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
