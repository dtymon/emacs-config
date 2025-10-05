;; -*- lexical-binding: t -*-

(use-package treesit
  :mode (("\\.tsx\\'" . tsx-ts-mode))
  :preface
  (defun dtymon::setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((bash . ("https://github.com/tree-sitter/tree-sitter-bash" "v0.20.5"))
               (elisp "https://github.com/Wilfred/tree-sitter-elisp")
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.21.0"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.21.0"))
               (toml "https://github.com/tree-sitter/tree-sitter-toml")
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.4" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "typescript/src"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0")))
             )
      (add-to-list 'treesit-language-source-alist grammar)
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))
      ))

  ;; (dolist (mapping
  ;;        '((python-mode . python-ts-mode)
  ;;          (typescript-mode . typescript-ts-mode)
  ;;          (js2-mode . js-ts-mode)
  ;;          (bash-mode . bash-ts-mode)
  ;;          (json-mode . json-ts-mode)
  ;;          (js-json-mode . json-ts-mode)))
  ;;   (add-to-list 'major-mode-remap-alist mapping))

  :config
  (dtymon::setup-install-grammars)
  )

(provide 'setup-treesit)
