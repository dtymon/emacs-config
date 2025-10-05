;; -*- lexical-binding: t -*-

(defun dtymon::display-buffer-from-compilation-p (_buffer-name _action)
  (unless current-prefix-arg
    (with-current-buffer (window-buffer)
      (derived-mode-p 'compilation-mode))))

(push '(display-buffer-from-compilation-p
        display-buffer-same-window
        (inhibit-same-window . nil))
      display-buffer-alist)

(provide 'setup-compilation-mode)
