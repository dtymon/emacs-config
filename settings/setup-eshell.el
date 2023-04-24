;; Setup shell

;; Note: Emacs runs .bashrc in *shell*
;; So mac users should ln -s .profile .bashrc

;; bash-completion

;; (autoload 'bash-completion-dynamic-complete
;;   "bash-completion"
;;   "BASH completion hook")
;; (add-hook 'shell-dynamic-complete-functions
;;           'bash-completion-dynamic-complete)
;; (add-hook 'shell-command-complete-functions
;;           'bash-completion-dynamic-complete)
;;
;; ;; tab-completion for shell-command
;;
;; (require 'shell-command)
;; (shell-command-completion-mode)
;;
;; ;; C-d to kill buffer if process is dead.
;;
;; (defun comint-delchar-or-eof-or-kill-buffer (arg)
;;   (interactive "p")
;;   (if (null (get-buffer-process (current-buffer)))
;;       (kill-buffer)
;;     (comint-delchar-or-maybe-eof arg)))
;;
;; (add-hook 'shell-mode-hook
;;           (lambda ()
;;             (define-key shell-mode-map (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)))
;;

(use-package xterm-color
  :ensure t
  :defer t)

(with-eval-after-load 'eshell
  (require 'xterm-color)
  (setenv "TERM" "xterm-256color")
;;  (add-hook 'eshell-before-prompt-hook
;;            (lambda ()
;;              (setq xterm-color-preserve-properties t)))
;;
;;  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
;;  (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
  )

(provide 'setup-eshell)
