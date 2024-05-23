(use-package beframe
  :ensure t
  :init
  (beframe-mode 1)
  :bind (:map global-map
              ("s-b b" . beframe-switch-buffer)
              ("s-b l" . beframe-buffer-menu)
              )

  :custom
  ;; Don't create separate scratch buffers for new frames as it quickly
  ;; degenerates to something unworkable.
  (beframe-create-frame-scratch-buffer nil)

  :config
  ;; Redefine the beframe buffer prompt to select the most recent buffer when
  ;; an empty input is provided.
  (defun beframe--buffer-prompt (&optional frame)
    "Prompt for buffer among `beframe-buffer-names'.

Use the previous buffer as the default value, such that
subsequent invocations of this command flip between the current
and previous buffers.

With optional FRAME, use list of buffers specific to the given
frame name."
  (read-buffer
   ;; Include the default buffer name in the prompt
   (format "Switch to frame buffer (%s): " (buffer-name (other-buffer (current-buffer))))
   (other-buffer (current-buffer))
   ;; Always require a match. I would classify this as a bug in beframe since:
   ;;  (confirm-nonexistent-file-or-buffer)
   ;;
   ;; returns 'confirm-after-completion when set to 'after-completion. However
   ;; this does not prevent beframe from creating a new buffer with a partial
   ;; completion as its name. To prevent switching to a non-existent buffer
   ;; because I forgot to press TAB first to complete, this needs to set to
   ;; true.
   t
   ;; NOTE: This predicate is not needed if `beframe-mode' is
   ;; non-nil because it sets the `read-buffer-function'.
   (lambda (buf)
     (beframe--read-buffer-p buf frame))
   ))
  )

(provide 'setup-beframe)
