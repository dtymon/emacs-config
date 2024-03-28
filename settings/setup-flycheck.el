;; Define a list of flycheck error modes that use a buffer anchored to the
;; bottom of the frame.
(defvar dtymon::flycheck-error-modes '())
(defun dtymon::flycheck-error-modes-window-p (buf act)
  (with-current-buffer buf (member major-mode dtymon::flycheck-error-modes)))

;; Define the attributes for the flycheck error buffer
(add-to-list 'display-buffer-alist
             '(dtymon::flycheck-error-modes-window-p
               (display-buffer-in-side-window)
               (window-height . 0.10)
               (window-width . 0.55)
               (dedicated . t)
               (side . bottom)
               (slot . 0)
               (window-parameters . ((no-other-window . nil)
                                     (no-delete-other-windows . t)
                                     (mode-line-format . 'none)))))

(defun dtymon::toggle-flycheck-show-buffer-diagnostics ()
  "Show or destroy the flycheck buffer diagnostics"
  (interactive)
  (let* ((name "Flycheck errors")
         (target (get-buffer name)))
    (cond ((eq target nil)
           (flycheck-list-errors))
          (t (dtymon::kill-buffer-and-window target (get-buffer-window target)))
          )
    ))

(defun dtymon::toggle-flycheck-show-project-diagnostics ()
  "Show or destroy the flycheck project diagnostics"
  (interactive)
  (let* ((prj (project-current))
         (root (project-root prj))
         (name "*Project errors*")
         (target (get-buffer name)))
    (cond ((eq target nil)
           (flycheck-projectile-list-errors))
          (t (kill-buffer target))
          )
    ))

(use-package flycheck
  :ensure t
  :init
  (setq
   flycheck-display-errors-function #'flycheck-display-error-messages
   flycheck-checker-error-threshold 2000
   flycheck-idle-change-delay 2
   flycheck-check-syntax-automatically '(save
                                         idle-change
                                         idle-buffer-switch
                                         new-line
                                         mode-enabled)
   )

  :config
  (set-face-attribute 'flycheck-fringe-error nil :background "yellow" :foreground "red")

  ;; Display the flycheck errors in a buffer anchored to the bottom of the frame
  (add-to-list 'dtymon::flycheck-error-modes 'flycheck-error-list-mode)

  ;; Disable some annoying checkers
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(emacs-lisp
                          emacs-lisp-checkdoc
                          javascript-jscs
                          javascript-jshint
                          javascript-standard)))

  ;; Key bindings
  (let ((map flycheck-mode-map))
    (define-key map (kbd "C-c C-p") #'flycheck-previous-error)
    (define-key map (kbd "C-c C-n") #'flycheck-next-error)
    (let ((diag-map (define-prefix-command 'dtymon::diag-map)))
      (define-key diag-map "p" (lambda ()
                                 (interactive)
                                 (cond ((project-current) (dtymon::toggle-flycheck-show-project-diagnostics))
                                       (t (dtymon::toggle-flycheck-show-buffer-diagnostics)))))
      (define-key diag-map "b" (lambda ()
                                 (interactive)
                                 (dtymon::toggle-flycheck-show-buffer-diagnostics))))
    (define-key map (kbd "C-c C-d") 'dtymon::diag-map)
    )
  )

(use-package flycheck-projectile
  :ensure t
  :config

  ;; Display the flycheck project errors in a buffer anchored to the bottom of
  ;; the frame.
  (add-to-list 'dtymon::flycheck-error-modes 'flycheck-projectile-error-list-mode)
  )

(use-package consult-flycheck
  :ensure t
  :defer t
  )

(use-package flycheck-yamllint
  :ensure t
  :after (flycheck)
  )

(provide 'setup-flycheck)
