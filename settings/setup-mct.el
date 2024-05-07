(use-package mct
  :ensure t
  :custom
  (mct-minimum-input 2)
  (mct-live-update-delay 0.5)
  (mct-apply-completion-stripes t)
  (mct-completion-passlist '(beframe-switch-buffer))

  :config
  (mct-minibuffer-mode 1)

  ;; Restore DEL back to just deleting a char
  (let ((map mct-minibuffer-local-completion-map))
    (define-key map (kbd "DEL") #'backward-delete-char)

    ;; Undo remaps for arrow keys, they should go through the history
    (define-key map [remap next-line] nil)
    (define-key map [remap next-line-or-history-element] nil)
    (define-key map [remap previous-line] nil)
    (define-key map [remap previous-line-or-history-element] nil)
    )
  )

(provide 'setup-mct)
