(defun dtymon::adjust-flycheck-automatic-syntax-eagerness ()
  "Adjust how often we check for errors based on if there are any.

This lets us fix any errors as quickly as possible, but in a
clean buffer we're an order of magnitude laxer about checking."
  (setq flycheck-idle-change-delay
        (if flycheck-current-errors 1.0 5.0)))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  (setq flycheck-display-errors-function #'flycheck-display-error-messages)

  ;; Disable some annoying checkers
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(emacs-lisp
                          emacs-lisp-checkdoc
                          javascript-jscs
                          javascript-jshint
                          javascript-standard)))

  ;; Each buffer gets its own idle-change-delay because of the buffer-sensitive
  ;; adjustment above.
  ;;
  ;; This is disabled as there appears to be issues introduced by it
  ;;(make-variable-buffer-local 'flycheck-idle-change-delay)
  ;;(add-hook 'flycheck-after-syntax-check-hook 'adjust-flycheck-automatic-syntax-eagerness)

;;  (add-hook 'flycheck-mode-hook
;;            (lambda ()
;;              (set-face-attribute 'flycheck-error-level-fringe-face nil :background "yellow" :foreground "red")))
;;
  )

(use-package flycheck-yamllint
  :ensure t
  :after (flycheck)
  )

(provide 'setup-flycheck)
