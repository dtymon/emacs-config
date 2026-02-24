;; -*- lexical-binding: t -*-

(use-package treesit
  :preface
  (defun dtymon::setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((bash       . ("https://github.com/tree-sitter/tree-sitter-bash"))
               (elisp      . ("https://github.com/Wilfred/tree-sitter-elisp"))
               (go         . ("https://github.com/tree-sitter/tree-sitter-go"))
               (html       . ("https://github.com/tree-sitter/tree-sitter-html"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
               (jsdoc      . ("https://github.com/tree-sitter/tree-sitter-jsdoc"))
               (json       . ("https://github.com/tree-sitter/tree-sitter-json"))
               (python     . ("https://github.com/tree-sitter/tree-sitter-python"))
               (toml       . ("https://github.com/tree-sitter/tree-sitter-toml"))
               (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" :source-dir "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" :source-dir "typescript/src"))
               (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml")))
             )
      (add-to-list 'treesit-language-source-alist grammar)
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))
      ))

  (dolist (mapping
         '((bash-mode       . bash-ts-mode)
           (go-mode         . go-ts-mode)
           (js2-mode        . js-ts-mode)
           (js-json-mode    . json-ts-mode)
           (json-mode       . json-ts-mode)
           (python-mode     . python-ts-mode)
           ;; (typescript-mode . typescript-ts-mode)
           (yaml-mode       . yaml-ts-mode)
           ))
    (add-to-list 'major-mode-remap-alist mapping))

  :config
  (dtymon::setup-install-grammars)
  )

(defconst dtymon::treesit-fold-dir (expand-file-name "treesit-fold" site-lisp-dir))

(use-package treesit-fold
  :load-path dtymon::treesit-fold-dir
  :blackout
  :bind (:map treesit-fold-mode-map
              ("C-s-o"   . treesit-fold-open)
              ("C-S-s-o" . treesit-fold-open-all)
              ("C-s-c"   . treesit-fold-close)
              ("C-S-s-c" . treesit-fold-close-all)
              )
  :config
  (global-treesit-fold-mode t)
  (setq treesit-fold-line-count-show t)
  )
(require 'treesit-fold)

(use-package treesit-fold-indicators
  :load-path dtymon::treesit-fold-dir
  )

(provide 'setup-treesit)
