(defun ztree-diff-node-ignore-p (node)
  "Determine if the NODE is in filter list.
If the node is in the filter list it shall not be visible,
unless it is a parent node."
  (let ((name (ztree-diff-node-short-name node))
        (left-path (ztree-diff-node-left-path node))
        (right-path (ztree-diff-node-right-path node))
        )

    (cond
     ;; Don't filter the root
     ((not (ztree-diff-node-parent node)) nil)

     ((and left-path
           (ztree-find ztree-diff-dir-ignore-list #'(lambda (rx) (string-match rx left-path)))
           ))
     ((and right-path
           (ztree-find ztree-diff-dir-ignore-list #'(lambda (rx) (string-match rx right-path)))
           ))
     (t
      (ztree-find ztree-diff-filter-list #'(lambda (rx) (string-match rx name))))
     )
    ))

(use-package ztree
  :ensure t
  :config
  (setq
   ztree-diff-filter-list '("^\\." "__pycache__")
   ztree-diff-dir-ignore-list '("/\\(__pycache__\\|\\.git\\)")
   ztree-draw-unicode-lines t
   )
  )

(provide 'setup-ztree)
