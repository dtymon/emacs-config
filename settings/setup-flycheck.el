(use-package flycheck
  :ensure t
  :init
  (setq flycheck-display-errors-function #'flycheck-display-error-messages)
  (setq flycheck-checker-error-threshold 2000)

  :config
  (set-face-attribute 'flycheck-fringe-error nil :background "yellow" :foreground "red")

  ;; Disable some annoying checkers
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(emacs-lisp
                          emacs-lisp-checkdoc
                          javascript-jscs
                          javascript-jshint
                          javascript-standard)))

  ;; (add-hook 'flycheck-mode-hook
  ;;          (lambda ()
  ;;            (set-face-attribute 'flycheck-fringe-error nil :background "yellow" :foreground "red")))

  )

(provide 'setup-flycheck)
