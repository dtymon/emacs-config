(require 'dired)

;; Writable dired
(require 'wdired)

;; Move files between split panes
;;(setq dired-dwim-target t)

;; Reload dired after making changes
(require 'dash)
(--each '(dired-do-rename
          dired-do-copy
          dired-create-directory
          wdired-abort-changes)
        (eval `(defadvice ,it (after revert-buffer activate)
                 (revert-buffer))))

(define-key dired-mode-map (kbd "k") 'dired-do-delete)

;; C-a is nicer in dired if it moves back to start of files
(define-key dired-mode-map (kbd "C-a") (lambda ()
                                         (interactive)
                                         (backward-char (- (current-column) 2))))

;; M-up is nicer if it goes to the first file
(define-key dired-mode-map [remap beginning-of-buffer] (lambda ()
                                                         (interactive)
                                                         (beginning-of-buffer)
                                                         (dired-next-line 3)))

;; M-down is nicer in dired if it moves to the last file
(define-key dired-mode-map [remap end-of-buffer] (lambda ()
                                                   (interactive)
                                                   (end-of-buffer)
                                                   (dired-next-line -1)))

;; Delete with C-x C-k to match file buffers and magit
(define-key dired-mode-map (kbd "C-x C-k") 'dired-do-delete)

;; Make buffer writable
(define-key dired-mode-map (kbd "E") 'wdired-change-to-wdired-mode)
(define-key wdired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)
(define-key wdired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
(define-key wdired-mode-map (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

;; ;; dired-sidebar
;; (define-key global-map (kbd "C-x C-n") 'dired-sidebar-toggle-sidebar)
;;
;; ;; keep the directory up-to-date as the directory changes
;; (add-hook 'dired-sidebar-mode-hook
;;             (lambda ()
;;               (unless (file-remote-p default-directory)
;;                 (auto-revert-mode))))
;;
;; (setq dired-sidebar-subtree-line-prefix "__"
;;       dired-sidebar-theme 'vscode
;;       dired-sidebar-use-term-integration t
;;       dired-sidebar-use-custom-font t
;;       insert-directory-program (or (executable-find "gls")
;;                                    (executable-find "ls"))
;;       )

(provide 'setup-dired)
