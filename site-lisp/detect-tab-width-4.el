(defgroup dtymon::dtw nil
  "Set a buffer's tab with to 4 if there is evidence to suggest it was edited
   with that setting"
  :group 'convenience)

(defcustom dtymon::dtw-required-evidence 1
  "Minimum number of lines required to be found that suggest a tab width of 4"
  :type 'integer)

(defconst dtymon::dtw-mixed-leading-ws-re
  (rx line-start
      (or (seq (+ ?\t) " ")
          (seq (* ?\t) (+ " ") ?\t)))
  "Regex matching lines starting with a mixture of tabs and spaces in any order"
  )

(defun dtymon::dtw--visual-indent-size-with-tw-4 ()
  "Determine the visual indentation size if the current line was indented with
 a tab width of 4"
  (save-excursion
    (let ((tab-width 4))
      (back-to-indentation)
      (current-column))))

(defun dtymon::dtw--evidence-line-p ()
  "Determine if the current line is evidence that tab width should be set to 4"
  (when (save-excursion (beginning-of-line) (looking-at dtymon::dtw-mixed-leading-ws-re))
    (let ((col (dtymon::dtw--visual-indent-size-with-tw-4)))
      (and (zerop (% col 4))
           (not (zerop (% col 8)))))))

(defun dtymon::dtw--find-evidence-for-tw-4 ()
  "Return non-nil if there is evidence to suggest a tab width of 4"
  (let ((evidence 0))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (and (not (eobp))
                    (< evidence dtymon::dtw-required-evidence))
          (when (dtymon::dtw--evidence-line-p)
            (setq evidence (1+ evidence)))
          (forward-line 1))))
    (>= evidence dtymon::dtw-required-evidence)))

(defun dtymon::dtw--detect-tw-4 ()
  "If there is evidence to suggest the current buffer was edited with a tab
 width of 4 then set tab-width accordingly"
  (when (and (bound-and-true-p tree-sitter-mode)
             tree-sitter-tree
             (dtymon::dtw--find-evidence-for-tw-4))
    (setq-local tab-width 4)
    (message "Evidence suggests the tab width should be set to 4")
  ))

(defun dtymon::detect-tab-width-4 ()
  ;; Allow other prog-hooks to run in case they set the tab width explicitly
  (run-at-time 0 nil (lambda ()
                       (unless (or (eq tab-width 4) (local-variable-p 'tab-width))
                         (dtymon::dtw--detect-tw-4)))))

(add-hook 'prog-mode-hook #'dtymon::detect-tab-width-4)

(provide 'detect-tab-width-4)
