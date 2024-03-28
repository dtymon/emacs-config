(defun dtymon::flycheck-after-save ()
  (run-with-timer 3 nil (lambda ()
                          (flycheck-buffer)
                          ))
  )

(defun dtymon::common-python-hook ()
  ;; Format on save
  (python-black-on-save-mode)

  ;; Show the fill column indicator
  (display-fill-column-indicator-mode 1)

  ;; For now, show the docs in the minibuffer
  (setq-local lsp-eldoc-enable-hover t)

  ;; Disable some flycheck checkers
  (add-to-list 'flycheck-disabled-checkers 'python-flake8)
  (add-to-list 'flycheck-disabled-checkers 'python-pylint)
  (add-to-list 'flycheck-disabled-checkers 'python-pycompile)

  ;; Turn on auto fill but only for comments
  (auto-fill-mode 1)
  (dtymon::auto-fill-comments-only-hook)

  (add-hook 'after-save-hook #'dtymon::flycheck-after-save t t)

  (setq
   tab-width 4
   python-indent-offset 4

   ;; Long lines should wrap
   truncate-lines nil

   ;; This is the fill column for comments
   fill-column 79

   ;; We want ruff to organise imports
   lsp-ruff-lsp-advertize-organize-imports t

   ;; Change pylsp to use codestyle rather than flake8
   lsp-pylsp-configuration-sources ["pycodestyle"]

   ;; Disable docstyle linting
   ;; lsp-pylsp-plugins-pydocstyle-enabled nil
   lsp-pylsp-plugins-pycodestyle-enabled nil

   ;; Use Jedi for completion
   lsp-pylsp-plugins-jedi-completion-enabled     t
   lsp-pylsp-plugins-jedi-definition-enabled     t
   lsp-pylsp-plugins-jedi-hover-enabled          t
   lsp-pylsp-plugins-jedi-references-enabled     t
   lsp-pylsp-plugins-jedi-signature-help-enabled t
   lsp-pylsp-plugins-jedi-symbols-enabled        t
   lsp-pylsp-plugins-jedi-use-pyenv-environment  t

   lsp-pylsp-plugins-flake8-enabled              nil
   ;; lsp-pylsp-plugins-flake8-ignore               '("D103")
   ;; lsp-pylsp-plugins-flake8-max-line-length      88
   ;; lsp-pylsp-plugins-pycodestyle-max-line-length 88

   ;; Disable code complexity measure
   ;; lsp-pylsp-plugins-mccabe-enabled          nil

   ;; lsp-pylsp-plugins-preload-enabled         nil
   ;; lsp-pylsp-plugins-pylint-enabled          nil
   ;; lsp-pylsp-plugins-pyflakes-enabled        nil
   ;; lsp-pylsp-plugins-rope-completion-enabled nil
   ;; lsp-pylsp-plugins-autopep8-enabled        nil
   ;; lsp-pylsp-plugins-yapf-enabled            nil
   ;; lsp-pylsp-plugins-black-enabled           nil
   )
  )

(use-package python
  :ensure t
  :blackout (python-mode . "Py")
  :commands python-mode
  :init
  (add-hook 'python-mode-hook    'dtymon::common-python-hook)
  (add-hook 'python-ts-mode-hook 'dtymon::common-python-hook)
  )

(use-package python-black
  :ensure t
  :after python
  :blackout python-black-on-save-mode
  )

;; (use-package lsp-pyright
;;   :ensure t
;;   :after python
;;   :blackout
;;   )

(use-package pyenv-mode
  :ensure t
  :config
  (pyenv-mode)
  )

(use-package poetry
  :ensure t
  :defer t
  :commands poetry-tracking-mode
  :config
  (setq poetry-tracking-strategy 'switch-buffer)
  )

(use-package python-pytest
  :ensure t
  :config
  (let ((map python-mode-map))
    (define-key map (kbd "C-c t a") #'python-pytest)
    (define-key map (kbd "C-c t f") #'python-pytest-file)
    (define-key map (kbd "C-c t t") #'python-pytest-function-dwim)
    (define-key map (kbd "C-c t r") #'python-pytest-repeat)
    )
  )

;; (use-package auto-virtualenv
;;   :ensure t
;;   :config
;;   (add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)
;;   (add-hook 'projectile-after-switch-project-hook 'auto-virtualenv-set-virtualenv)
;;   )

;; (flycheck-define-checker python-dtymon
;;   "A Python syntax and style checker using the ruff utility.
;; To override the path to the ruff executable, set
;; `flycheck-python-ruff-executable'.
;; See URL `http://pypi.python.org/pypi/ruff'."
;;   :command ("ruff"
;;             "--format=text"
;;             (eval (when buffer-file-name
;;                     (concat "--stdin-filename=" buffer-file-name)))
;;             "-")
;;   :standard-input t
;;   :error-filter (lambda (errors)
;;                   (let ((errors (flycheck-sanitize-errors errors)))
;;                     (seq-map #'flycheck-flake8-fix-error-level errors)))
;;   :error-patterns
;;   ((warning line-start
;;             (file-name) ":" line ":" (optional column ":") " "
;;             (id (one-or-more (any alpha)) (one-or-more digit)) " "
;;             (message (one-or-more not-newline))
;;             line-end))
;;   :modes python-ts-mode)
;;
;; (add-to-list 'flycheck-checkers 'python-dtymon)

(provide 'setup-python)
