;; -*- lexical-binding: t -*-

(use-package consult
  :ensure t
  :bind (
         ("C-c M-x" . consult-mode-command)
         ("M-y"     . consult-yank-pop)
         ("C-x C-j" . consult-goto-line)
         ("C-c C-l" . consult-line)
         ("C-c C-r" . consult-line-multi)
         ("C-c C-f" . consult-focus-lines)
         ;; Override the default behaviour with the consult variants
         ("C-x g"   . consult-register-load)
         ("C-x j"   . consult-register-load)
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
  (setq consult-narrow-key "<")
  )

(use-package consult-projectile
  :ensure t
  :bind (
         ("C-c s-p" . consult-projectile)
         )

  :config
  (setq consult-projectile-sources
        '(consult-projectile--source-projectile-buffer
          consult-projectile--source-projectile-file
          consult-projectile--source-projectile-project
          consult-projectile--source-projectile-dir
          consult-projectile--source-projectile-recentf
          ))

  )

(provide 'setup-consult)
