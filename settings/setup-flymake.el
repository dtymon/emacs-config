(defun dtymon::toggle-flymake-show-buffer-diagnostics ()
  "Show or destroy the flymake buffer diagnostics"
  (interactive)
  (let* ((name (flymake--diagnostics-buffer-name))
         (target (get-buffer name)))
    (cond ((eq target nil)
           (flymake-show-buffer-diagnostics))
          (t (dtymon::kill-buffer-and-window target (get-buffer-window target)))
          )
    ))

(defun dtymon::toggle-flymake-show-project-diagnostics ()
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

(use-package flymake
  :ensure t
  :init
  ;; steal the flycheck error indicator for flymake's use
  ;; maximum width is 16px according to emacs docs
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

  :custom
  (flymake-suppress-zero-counters t)
  (flymake-start-on-flymake-mode t)
  (flymake-no-changes-timeout 0.5)
  (flymake-start-on-save-buffer t)
  (flymake-proc-compilation-prevents-syntax-check t)
  (flymake-wrap-around nil)
  (flymake-error-bitmap '(flymake-big-indicator compilation-error))
  (flymake-warning-bitmap '(flymake-big-indicator compilation-warning))
  (flymake-note-bitmap '(flymake-big-indicator compilation-info))
  (flymake-mode-line-title " ✓")
  (flymake-mode-line-format '("" flymake-mode-line-exception flymake-mode-line-counters))
  (flymake-mode-line-counter-format
   '(" " flymake-mode-line-error-counter flymake-mode-line-warning-counter flymake-mode-line-note-counter ""))

  :config
  ;; Try to reuse the diagnostic windows
  (add-to-list 'display-buffer-alist
               '("\\*Flymake diagnostics for .*\\*"
                 (display-buffer-reuse-mode-window display-buffer-at-bottom)
                 (window-height . 0.1)))

  ;; Hitting 'q' in the diagnostics buffers complete destroys the buffer
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

  ;; Define keys to jump to errors and toggle diagnostics
  (let ((map flymake-mode-map))
    (define-key map (kbd "C-c C-p") #'flymake-goto-prev-error)
    (define-key map (kbd "C-c C-n") #'flymake-goto-next-error)
    (define-key map (kbd "C-c C-d") (lambda ()
                                    (interactive)
                                    (cond ((project-current) (dtymon::toggle-flymake-show-project-diagnostics))
                                          (t (dtymon::toggle-flymake-show-buffer-diagnostics)))))

    (define-key map (kbd "C-c C-e") #'consult-flymake)
    )
  )

;; (use-package flymake-diagnostic-at-point
;;   :ensure t
;;   :config
;;   (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode)
;;   )

(provide 'setup-flymake)
