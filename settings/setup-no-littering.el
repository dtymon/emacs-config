;; -*- lexical-binding: t -*-

(use-package no-littering
  :ensure t
  :defer t
  :config

  ;; Store auto-save files in the no-littering var directory
  (setq
   dtymon::auto-save-backup-dir (expand-file-name (convert-standard-filename "backup/") no-littering-var-directory)

   backup-directory-alist
   `((".*" . ,dtymon::auto-save-backup-dir))

   auto-save-file-name-transforms
   `((".*" ,dtymon::auto-save-backup-dir t))
   ))

(provide 'setup-no-littering)
