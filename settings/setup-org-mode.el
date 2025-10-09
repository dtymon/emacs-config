;; -*- lexical-binding: t -*-

(use-package org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (js . t)
     ))
  )

(provide 'setup-org-mode)
