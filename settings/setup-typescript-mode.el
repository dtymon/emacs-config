(defun dtymon::common-ts-mode-hook ()
  ;; Only auto-fill comment lines. prettier will handle the code.
  (dtymon::auto-fill-comments-only-hook)
  (prettier-mode 1)

  (setq
   typescript-indent-level 2

   ;; Long lines should wrap
   truncate-lines nil

   ;; This is the fill column for comments
   fill-column 80

   ;; comment-line-break-function #'indent-new-comment-line
   ;; comment-line-break-function #'c-indent-new-comment-line
   js-indent-level 2

   comment-multi-line t

   ;; These code actions are very OTT
   lsp-ui-sideline-show-code-actions nil
   )

  ;; This needs to be set as a buffer-local var to avoid affecting
  ;; modes other than TS.
  ;;
  ;; We use the js2-line-break function for filling because it
  ;; handles both single-line and multi-line comments as required.
  ;; The default TS fill function does not add a '*' prefix when
  ;; wrapping multi-line comments.
  (setq-local
   comment-line-break-function #'js2-line-break
   )

  ;; Don't use the fill column from the prettier config
  (and (eq (cadar prettier-sync-settings) ':printWidth) (pop prettier-sync-settings))

  ;; Setup the tree-sitter highlighting
  (setq-local treesit-font-lock-settings (typescript-ts-mode--font-lock-settings 'typescript))
  (treesit-major-mode-setup)

  (setq-local
   ;; Run node rather than npx so we can pass some special arguments
   ;; jest-test-command-string "node %s ./node_modules/.bin/jest %s %s"
   jest-test-command-string (concat "NODE_CONFIG_DIR=" (dtymon::kelpie-config-dir) " NODE_ENV=test node %s ./node_modules/.bin/jest --config " (dtymon::jest-config-file) " %s %s")
   )
  )

(defun dtymon::use-local-eslint ()
  (let ((eslint (expand-file-name "node_modules/.bin/eslint"
                                  (locate-dominating-file
                                   (or (buffer-file-name) default-directory)
                                   "node_modules"))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint)))
  )

(defun dtymon::get-eslint-config-path ()
  (let* ((root (or (locate-dominating-file
                    (or (buffer-file-name) default-directory)
                    (lambda (dir)
                      (directory-files dir nil "^\\.eslint\\(\\.config\\.mjs\\|\\.config\\.js\\|rc\\.*\\)$")))
                   default-directory))
         (flat-config (or (expand-file-name ".eslint.config.mjs" root)
                          (expand-file-name ".eslint.config.js" root)))
         (legacy-config (or (expand-file-name ".eslintrc" root)
                            (expand-file-name ".eslintrc.js" root)
                            (expand-file-name ".eslintrc.json" root))))
    (cond
     ((and flat-config (file-exists-p flat-config)) flat-config)
     ((and legacy-config (file-exists-p legacy-config)) legacy-config)
     (t (error "No ESLint config found in project"))
     ))
  )

(defun dtymon::set-eslint-config-path ()
  (let* ((config-file (dtymon::get-eslint-config-path)))
    (setq-local flycheck-eslint-args (list "--config" config-file))
  ))

(defun flycheck-eslint-config-exists-p ()
  "Whether there is a valid eslint config for the current buffer."
  (eql 0 (flycheck-call-checker-process
          'javascript-eslint nil nil nil
          "--config" (dtymon::get-eslint-config-path) "--print-config" (or buffer-file-name "index.js"))))

(use-package typescript-mode
  :ensure t
  :blackout (typescript-mode . "TS")
  :mode "\\.[cm]?[jt]s"
  :interpreter "node"
  :bind (:map typescript-mode-map
              ("C-c C-t" . git-timemachine-toggle)
              )

  :init
  ;; We require js2-mode as we use its fill function
  (require 'js2-mode)

  ;; Include the tree-sitter settings for Typescript
  (require 'typescript-ts-mode)

  ;; Turn on tree-sitter and use it for highlighting. Also add the common
  ;; initialisation hook.
  (dolist (hook '(typescript-mode-hook typescript-ts-mode-hook))
    (add-hook hook #'tree-sitter-mode)
    (add-hook hook #'tree-sitter-hl-mode)
    (add-hook hook 'dtymon::common-ts-mode-hook)
    ;; (add-hook hook 'dtymon::use-local-eslint)
    ;; (add-hook hook 'dtymon::set-eslint-config-path)
    )

  ;; (flycheck-add-mode 'javascript-eslint 'typescript-mode)
  ;; (flycheck-add-mode 'javascript-eslint 'typescript-ts-mode)
)

(use-package jest-test-mode
  :ensure t
  :commands jest-test-mode
  :hook (typescript-mode typescript-mode-ts)
  :bind (:map typescript-mode-map
              ("C-c t a" . jest-test-run-all-tests)
              ("C-c t f" . jest-test-run)
              ("C-c t t" . jest-test-run-at-point)
              ("C-c t r" . jest-test-rerun-test)
              ("C-c C-t" . git-timemachine-toggle)
              )
  :config
  (make-local-variable 'jest-test-command-string)

  (setq
   ;; Run node rather than npx so we can pass some special arguments
   ;; jest-test-command-string "node %s ./node_modules/.bin/jest %s %s"
   jest-test-command-string "NODE_CONFIG_DIR=./application/src/config NODE_ENV=test node %s ./node_modules/.bin/jest --config application/jest.config.js %s %s"
   )

  (setq
   ;; The arguments for node
   jest-test-npx-options '("--experimental-vm-modules")

   ;; The arguments for jest
   jest-test-options '("--color"
                       "--coverage=false"
                       "--verbose=false"
                       "--passWithNoTests")
   )
  ;; Make the prefixes more obvious in which-key
  (which-key-add-keymap-based-replacements typescript-mode-map
    "C-c t" "run tests"
    )
  )

(provide 'setup-typescript-mode)
