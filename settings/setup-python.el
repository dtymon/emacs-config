(defun dtymon::flycheck-after-save ()
  (run-with-timer 3 nil (lambda ()
                          (flycheck-buffer)
                          ))
  )

(defun dtymon::common-python-hook ()
  ;; Format on save
  (python-black-on-save-mode)

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

   ;; Disable code and doc style linting
   lsp-pylsp-plugins-pycodestyle-enabled nil
   lsp-pylsp-plugins-pydocstyle-enabled  nil

   ;; Use Jedi for completion
   lsp-pylsp-plugins-jedi-completion-enabled        t
   lsp-pylsp-plugins-jedi-completion-include-params nil
   lsp-pylsp-plugins-jedi-definition-enabled        t
   lsp-pylsp-plugins-jedi-hover-enabled             t
   lsp-pylsp-plugins-jedi-references-enabled        t
   lsp-pylsp-plugins-jedi-signature-help-enabled    t
   lsp-pylsp-plugins-jedi-symbols-enabled           t
   lsp-pylsp-plugins-jedi-use-pyenv-environment     t

   ;; Disable the pylsp modules that are not required. Descriptions of these
   ;; can be found here:
   ;;   https://github.com/python-lsp/python-lsp-server/blob/develop/CONFIGURATION.md
   ;;
   lsp-pylsp-plugins-flake8-enabled          nil
   lsp-pylsp-plugins-mccabe-enabled          nil
   lsp-pylsp-plugins-preload-enabled         nil
   lsp-pylsp-plugins-pylint-enabled          nil
   lsp-pylsp-plugins-pyflakes-enabled        nil
   lsp-pylsp-plugins-rope-completion-enabled nil
   lsp-pylsp-plugins-autopep8-enabled        nil
   lsp-pylsp-plugins-yapf-enabled            nil
   lsp-pylsp-plugins-black-enabled           nil

   ;; lsp-pylsp-plugins-mypy-enabled t
   ;; lsp-pylsp-plugins-mypy-dmypy t
   )
  )

(use-package python
  :ensure t
  :blackout (python-mode . "Py")
  :commands python-mode
  :init
  (add-hook 'python-mode-hook    'dtymon::common-python-hook)
  (add-hook 'python-ts-mode-hook 'dtymon::common-python-hook)
  :bind (:map python-mode-map
              ("C-c C-t" . git-timemachine-toggle)
              ("C-c t a" . python-pytest)
              ("C-c t f" . python-pytest-file)
              ("C-c t t" . python-pytest-function-dwim)
              ("C-c t r" . python-pytest-repeat)
              )

  :config
  ;; Make the python prefixes more obvious in which-key
  (which-key-add-keymap-based-replacements python-mode-map
    "C-c C-i" "python imports"
    "C-c t" "run tests"
    )
  )

(use-package python-black
  :ensure t
  :after python
  :blackout python-black-on-save-mode
  )

(use-package pyenv-mode
  :ensure t
  ;; This package is a bad citizen as it sets keys in the global map rather
  ;; than being able to restrict it to just python keymaps. So override their
  ;; keymap such that it is empty and set the key bindings when visiting python
  ;; sources.
  :init
  (setq pyenv-mode-map
        (let ((map (make-sparse-keymap)))
          map))
  :hook python-ts-mode python-mode
  :bind
  (:map python-mode-map
        ("C-c C-s" . pyenv-mode-set)
        ("C-c C-u" . pyenv-mode-unset)
        :map python-ts-mode-map
        ("C-c C-s" . pyenv-mode-set)
        ("C-c C-u" . pyenv-mode-unset)
        )
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
  )

(use-package lsp-pyright
  :ensure t
  :custom
  ;; Turn this off to avoid the annoying unused variable stuff that the pyright
  ;; developers aren't willing to suppress with a leading underscore.
  ;; https://github.com/microsoft/pyright/issues/1118
  (lsp-pyright-disable-tagged-hints t)
  )

(provide 'setup-python)
