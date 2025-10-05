;; -*- lexical-binding: t -*-

;; Allow recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Show how deep the minibuffer is
(minibuffer-depth-indicate-mode)

;; Bury the completions buffer after exiting the minibuffer
(add-hook 'minibuffer-exit-hook
          (lambda ()
            (if (get-buffer "*Completions*")
                (bury-buffer "*Completions*"))
            ))

(provide 'setup-minibuffer)
