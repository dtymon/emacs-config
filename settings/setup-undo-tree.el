(use-package undo-tree
  :ensure t
  :init
  ;; Reduce the limits from the defaults
  (setq
   ;; Don't save the undo history as it is too slow
   undo-tree-auto-save-history nil
   undo-tree-limit          8000000
   undo-tree-strong-limit  12000000
   undo-tree-outer-limit   36000000
   undo-tree-mode-lighter  ""
   )

  :config
  (global-undo-tree-mode)

  ;; Compress the files because there is a lot of them and they can be very
  ;; large.
  (defadvice undo-tree-make-history-save-file-name
      (after undo-tree activate)
    (setq ad-return-value (concat ad-return-value ".gz")))
  )

(provide 'setup-undo-tree)
