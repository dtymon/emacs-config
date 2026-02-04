;; -*- lexical-binding: t -*-

(use-package vterm
  :ensure t
  :bind (:map vterm-mode-map
              ("C-c C-x"     . #'vterm--self-insert)
              ("C-c M-l"     . #'vterm-clear-scrollback)
              ("C-x <prior>" . #'vterm-copy-mode)
              ("C-q"         . #'vterm-send-next-key)

              :map vterm-copy-mode-map
              ("C-c C-o" . #'find-file-at-point)
              ("C-c f" . #'dtymon::vterm-insert-e-at-point)
              )

  :config
  (setq
   vterm-shell "zsh -l"
   vterm-max-scrollback 100000
   )
  ;; (setq term-prompt-regexp "^\nzsh .*\n[0-9][0-9]:[0-9][0-9][AP]M .*> ")

  (add-to-list 'vterm-eval-cmds '("find-file-line"
                                  (lambda (file line)
                                    (let ((line-num (string-to-number line)))
                                      (find-file-other-window file)
                                      (goto-char (point-min))
                                      (goto-line line-num)))))

  (defun dtymon::vterm-insert-e-at-point (&optional no-return)
    (interactive "P")
    (let ((file (thing-at-point 'filename t)))
      (unless file
        (user-error "No filename at point"))
      ;; leave copy-mode so input goes to the terminal
      (vterm-copy-mode -1)
      ;; send command
      (vterm-send-string (concat " e " file))
      (unless no-return
        (vterm-send-return))))

  (defun dtymon::vterm-turn-off-chrome ()
    (hl-line-mode -1)
    (display-line-numbers-mode -1))

  :hook (vterm-mode . dtymon::vterm-turn-off-chrome)
  )

;; (use-package vterm-toggle
;;   :ensure t
;;   :custom
;;   (vterm-toggle-fullscreen-p nil "Open a vterm in another window.")
;;   (vterm-toggle-scope 'project)
;;   ;; :bind (
;;   ;;        ("C-c t" . #'vterm-toggle)
;;   ;;        :map vterm-mode-map
;;   ;;        ("s-t" . #'vterm) ; Open up new tabs quickly
;;   ;;        )
;;   )

(provide 'setup-vterm)
