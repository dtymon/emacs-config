;; -*- lexical-binding: t -*-

(use-package gptel
  :ensure t
  :config
  (setq gptel-backend
        (gptel-make-ollama "Ollama"
                           :host "localhost:11434"
                           :stream t
                           :models '(llama3.1:latest qwen2.5-coder:7b-instruct-q6_K)))

  (setq gptel-model 'llama3.1:latest)
  (setq gptl-use-curl nil)
)

(use-package gptel-quick
  :ensure nil ;; it is located in site-lisp
  :after (gptel)
  )

;; (use-package gptel-commit
;;   :ensure t
;;   :after (gptel magit)
;;   :custom
;;   (gptel-commit-stream t)
;;   )

(use-package gptel-magit
  :ensure t
  :after (gptel magit)
  :hook (magit-mode . gptel-magit-install)
)

(provide 'setup-gptel)