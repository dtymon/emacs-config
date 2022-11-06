(use-package flymake
  :ensure t
  :init
  ;; steal the flycheck error indicator for flymake's use
  ;; maxmum width is 16px according to emacs docs
  (define-fringe-bitmap 'flymake-big-indicator
      (vector #b0000000000000000
              #b0000000000000000
              #b0000000000000000
              #b0111000111000000
              #b0011100011100000
              #b0001110001110000
              #b0000111000111000
              #b0000011100011100
              #b0000011100011100
              #b0000111000111000
              #b0001110001110000
              #b0011100011100000
              #b0111000111000000
              #b0000000000000000
              #b0000000000000000
              #b0000000000000000)
    16 16 'center)

  :bind (
         ("C-s-n" . flymake-goto-next-error)
         ("C-s-p" . flymake-goto-prev-error)
         ("C-s-l" . flymake-show-project-diagnostics)
         )

  :config
  (setq flymake-error-bitmap '(flymake-big-indicator compilation-error)
        flymake-warning-bitmap '(flymake-big-indicator compilation-warning)
        flymake-note-bitmap '(flymake-big-indicator compilation-info)
        flymake-mode-line-title " ✓"
        )

  ;; Try to reuse the diagnostic windows
  (add-to-list 'display-buffer-alist
               '("\\*Flymake diagnostics for .*\\*"
                 (display-buffer-reuse-mode-window display-buffer-at-bottom)
                 (window-height . 0.1)))

  (defun toggle-flymake-buffer-diagnostics-mode ()
    "Show or destroy the flymake buffer diagnostics"
    (interactive)
    (let* ((name (flymake--diagnostics-buffer-name))
           (target (get-buffer name)))
      (cond ((eq target nil)
             (flymake-show-buffer-diagnostics))
            (t (dtymon::kill-buffer-and-window target (get-buffer-window target)))
            )
      ))

  (defun toggle-flymake-project-diagnostics-mode ()
    "Show or destroy the flymake project diagnostics"
    (interactive)
    (let* ((prj (project-current))
           (root (project-root prj))
           (name (format "*Flymake diagnostics for `%s'*" root))
           (target (get-buffer name)))
      (cond ((eq target nil)
             (flymake-show-project-diagnostics))
            (t (dtymon::kill-buffer-and-window target (get-buffer-window target)))
            )
      ))

  ;; Bind these to keys
  (define-key flymake-mode-map (kbd "C-s-b") 'toggle-flymake-buffer-diagnostics-mode)
  (define-key flymake-mode-map (kbd "C-s-z") 'toggle-flymake-project-diagnostics-mode)

  ;; When someone hits 'q' in these buffers make sure we completely destroy the
  ;; buffer.
  (define-key flymake-diagnostics-buffer-mode-map (kbd "q")
              (lambda ()
                (interactive)
                (dtymon::kill-buffer-and-window (current-buffer) (selected-window))
                ))

  (define-key flymake-project-diagnostics-mode-map (kbd "q")
              (lambda ()
                (interactive)
                (quit-window t)
                ))
  )

(use-package flymake-diagnostic-at-point
  :ensure t
  :config
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode)
  )

(provide 'setup-flymake)
