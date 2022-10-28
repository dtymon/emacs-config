(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (setq
   ;; which-key-popup-type 'minibuffer
   which-key-popup-type 'side-window
   which-key-side-window-location 'right
   which-key-compute-remaps t
   which-key-allow-multiple-replacements t
   which-key-idle-delay 3
   )
  (which-key-mode 1)
  )

(provide 'setup-which-key)
