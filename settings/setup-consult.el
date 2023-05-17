(use-package consult
  :ensure t
  :bind (
         ("C-c M-x" . consult-mode-command)
         ("M-y"     . consult-yank-pop)
         ("C-x C-j" . consult-goto-line)

         )

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  :config
  (let ((map global-map))
    (define-key map (kbd "C-c M-x") #'consult-mode-command)
    (define-key map (kbd "M-y")     #'consult-yank-pop)
    (define-key map (kbd "C-x C-j") #'consult-goto-line)
    (define-key map (kbd "C-c C-l") #'consult-line)
    (define-key map (kbd "C-c C-r") #'consult-line-multi)
    (define-key map (kbd "C-c C-f") #'consult-focus-lines)

    ;; Override the default behaviour with the consult variants
    (define-key map (kbd "C-x g")   #'consult-register-load)
    (define-key map (kbd "C-x j")   #'consult-register-load)
    )
  )

(provide 'setup-consult)
