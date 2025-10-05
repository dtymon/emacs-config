;; -*- lexical-binding: t -*-

(use-package rotate
  :ensure t
  :config
  ;; Write a little function that does the rotation but keeps focus in the
  ;; currently selected window rather than the focus following the buffer.
  (defun dtymon::rotate-keep-focus()
    (interactive)
    (let ((curr-window (selected-window)))
      (rotate-window)
      (select-window curr-window)
    ))
  :bind (:map global-map
              ("<S-kp-add>" . dtymon::rotate-keep-focus)
              )
  )

(provide 'setup-rotate)
