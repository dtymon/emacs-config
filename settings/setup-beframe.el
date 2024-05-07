(use-package beframe
  :ensure t
  :init
  (beframe-mode 1)
  :bind (:map global-map
              ;; Use beframe for default bindings
              ("C-x b" . beframe-switch-buffer)
              ("C-x C-b" . beframe-buffer-menu)
              )

  :config
  (defun beframe--buffer-prompt (&optional frame)
    "Prompt for buffer among `beframe-buffer-names'.

Use the previous buffer as the default value, such that
subsequent invocations of this command flip between the current
and previous buffers.

With optional FRAME, use list of buffers specific to the given
frame name."
  (read-buffer
   (format "Switch to frame buffer (%s): " (buffer-name (other-buffer (current-buffer))))
   (other-buffer (current-buffer))
   (confirm-nonexistent-file-or-buffer)
   ;; NOTE: This predicate is not needed if `beframe-mode' is
   ;; non-nil because it sets the `read-buffer-function'.
   (lambda (buf)
     (beframe--read-buffer-p buf frame))
   ))
  )

(provide 'setup-beframe)
