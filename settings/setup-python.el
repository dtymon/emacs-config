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

  (setq
   tab-width 4
   python-indent-offset 4

   ;; Long lines should wrap
   truncate-lines nil

   ;; This is the fill column for comments
   fill-column 79

   ;; We don't want ruff to organise imports
   lsp-ruff-lsp-advertize-organize-imports nil

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
  :diminish "Py"
  :commands python-mode
  :init
  (add-hook 'python-mode-hook    'dtymon::common-python-hook)
  (add-hook 'python-ts-mode-hook 'dtymon::common-python-hook)
  )

(use-package pyenv-mode
  :ensure t
  :config
  (pyenv-mode)
  )

(use-package python-black
  :ensure t
  :after python
  :diminish "Bl"
  )

(provide 'setup-python)
