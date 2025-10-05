;; -*- lexical-binding: t -*-

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
    ;; (define-key map (kbd "RET") #'mct-choose-completion-exit)
    ;; (define-key map (kbd "RET") #'mct-choose-completion-dwim)

    ;; Undo remaps for arrow keys, they should go through the history
    (define-key map [remap next-line] nil)
    (define-key map [remap next-line-or-history-element] nil)
    (define-key map [remap previous-line] nil)
    (define-key map [remap previous-line-or-history-element] nil)
    (define-key map (kbd "<up>") #'previous-line-or-history-element)
    (define-key map (kbd "<down>") #'next-line-or-history-element)

    ;; Map C-s and C-r to be a bit like ido-switch-buffer except it doesn't
    ;; search but goes back and forth through the history.
    (define-key map (kbd "C-s") #'previous-line-or-history-element)
    (define-key map (kbd "C-r") #'next-line-or-history-element)

    ;; Rebind S-up and S-down to jump to the top/bottom of completions
    (define-key map [remap windmove-up] #'mct-switch-to-completions-top)
    (define-key map [remap windmove-down] #'mct-switch-to-completions-bottom)
    )
  )

(provide 'setup-mct)
