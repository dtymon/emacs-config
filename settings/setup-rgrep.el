(require 's)
(require 'dash)
(require 'grep)

(defun rgrep-fullscreen (regexp &optional files dir confirm)
  "Open grep in full screen, saving windows."
  (interactive
   (progn
     (grep-compute-defaults)
     (cond
      ((and grep-find-command (equal current-prefix-arg '(16)))
       (list (read-from-minibuffer "Run: " grep-find-command
                                   nil nil 'grep-find-history)))
      ((not grep-find-template)
       (error "grep.el: No `grep-find-template' available"))
      (t (let* ((regexp (grep-read-regexp))
                (files (grep-read-files regexp))
                (dir (ido-read-directory-name "Base directory: "
                                              nil default-directory t))
                (confirm (equal current-prefix-arg '(4))))
           (list regexp files dir confirm))))))
  (window-configuration-to-register ?$)
  (rgrep regexp files dir confirm)
  (switch-to-buffer "*grep*")
  (delete-other-windows)
  (beginning-of-buffer))

(defun rgrep-quit-window ()
  (interactive)
  (kill-buffer)
  (jump-to-register ?$))

(defun rgrep-goto-file-and-close-rgrep ()
  (interactive)
  (compile-goto-error)
  (kill-buffer "*grep*")
  (delete-other-windows)
  (message "Type C-x r j $ to return to pre-rgrep windows."))

(defvar git-grep-switches "--extended-regexp -I -n"
  "Switches to pass to `git grep'.")

(defun git-grep-fullscreen (regexp &optional files dir confirm)
  (interactive
   (let* ((regexp (grep-read-regexp))
          (files (grep-read-files regexp))
          (files (if (string= "* .*" files) "*" files))
          (dir (ido-read-directory-name "Base directory: "
                                        nil default-directory t))
          (confirm (equal current-prefix-arg '(4))))
     (list regexp files dir confirm)))
  (let ((command (format "cd %s && git --no-pager grep %s %s -e %S -- '%s' "
                         dir
                         git-grep-switches
                         (if (s-lowercase? regexp) " --ignore-case" "")
                         regexp
                         files))
        (grep-use-null-device nil))
    (when confirm
      (setq command (read-shell-command "Run git-grep: " command 'git-grep-history)))
    (window-configuration-to-register ?$)
    (grep command)
    (switch-to-buffer "*grep*")
    (delete-other-windows)
    (beginning-of-buffer)))

(defvar dtymon::grep-match-positions nil)
(make-variable-buffer-local 'dtymon::grep-match-positions)

(defun dtymon::grep-register-match-positions ()
  (save-excursion
    (forward-line 0)
    (let ((end (point)) beg)
      (goto-char compilation-filter-start)
      (forward-line 0)
      (setq beg (point))
      ;; Only operate on whole lines so we don't get caught with part of an
      ;; escape sequence in one chunk and the rest in another.
      (when (< (point) end)
        (setq end (copy-marker end))
        ;; Register all positions of matches
        (while (re-search-forward "\033\\[0?1;31m\\(.*?\\)\033\\[[0-9]*m" end 1)
          (add-to-list 'dtymon::grep-match-positions (set-marker (make-marker) (match-beginning 1))))))))

(defun dtymon::grep-after-advice ()
  (add-hook 'compilation-filter-hook 'dtymon::grep-register-match-positions nil t))

(eval-after-load 'grep
  '(progn
     ;; Don't recurse into some directories
     (add-to-list 'grep-find-ignored-directories "target")
     (add-to-list 'grep-find-ignored-directories "node_modules")
     (add-to-list 'grep-find-ignored-directories "vendor")
     (advice-add 'grep-mode :after #'dtymon::grep-after-advice)

     (define-key grep-mode-map (kbd "q") 'rgrep-quit-window)
     (define-key grep-mode-map (kbd "C-<return>") 'rgrep-goto-file-and-close-rgrep)
     ))

(provide 'setup-rgrep)
